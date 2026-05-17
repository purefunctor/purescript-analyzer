//! Implements checking and inference rules for value groups.
//!
//! See [`check_value_equations`] and [`infer_value_equations`].

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, constraint, signature, toolkit, unification};
use crate::error::ErrorKind;
use crate::source::binder;
use crate::source::terms::guarded;
use crate::state::CheckState;

/// The syntactic origin of an equation's expected type.
pub enum EquationTypeOrigin {
    /// There is a syntactic origin.
    Explicit(lowering::TypeId),
    /// This is no syntactic origin.
    Implicit,
}

struct ValueEquationSignature {
    signature: TypeId,
    arguments: Vec<TypeId>,
    result: TypeId,
}

/// See documentation for [`check_value_equations`].
pub type ValueEquationPatterns = Vec<TypeId>;

/// Checks a group of [`lowering::Equation`].
///
/// This function returns the instantiated types of the equation's
/// arguments for use in exhaustiveness checking by the callers.
pub fn check_value_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    origin: EquationTypeOrigin,
    expected_type: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<ValueEquationPatterns>
where
    Q: ExternalQueries,
{
    let required = equations.iter().map(|equation| equation.binders.len()).max().unwrap_or(0);

    let signature::SkolemisedSignature { substitution, constraints, arguments, result } =
        signature::expect_term_signature(state, context, expected_type, required)?;

    for &constraint in &constraints {
        if !constraint::is_type_error(state, context, constraint)? {
            state.push_given(constraint);
        }
    }

    let signature = context.intern_function_list(&arguments, result);
    let signature = ValueEquationSignature { signature, arguments, result };

    let mut arguments = ValueEquationPatterns::clone(&signature.arguments);
    instantiate_pattern_arguments(state, context, &mut arguments, equations)?;

    state.with_implicit(context, &substitution, |state| {
        check_equations(state, context, origin, &signature, &arguments, equations)
    })?;

    Ok(arguments)
}

/// Infers a group of [`lowering::Equation`].
///
/// The `group_type` is a placeholder unification variable for the
/// equation group. Each inferred equation type must be [`subtype`]
/// of `group_type`.
///
/// [`subtype`]: unification::subtype
pub fn infer_value_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    group_type: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<ValueEquationPatterns>
where
    Q: ExternalQueries,
{
    let minimum_equation_arity =
        equations.iter().map(|equation| equation.binders.len()).min().unwrap_or(0);

    for equation in equations {
        let mut inferred_argument_types = vec![];
        for &binder_id in equation.binders.iter() {
            let argument_type = binder::infer_binder(state, context, binder_id)?;
            inferred_argument_types.push(argument_type);
        }

        let result_type = state.fresh_unification(context.queries, context.prim.t);

        let inferred_argument_types = &inferred_argument_types[..minimum_equation_arity];
        let equation_type = context.intern_function_list(inferred_argument_types, result_type);
        unification::subtype(state, context, equation_type, group_type)?;

        if let Some(guarded) = &equation.guarded {
            let inferred_type = guarded::infer_guarded_expression(state, context, guarded)?;
            unification::subtype(state, context, inferred_type, result_type)?;
        }
    }

    let toolkit::InspectFunction { arguments, .. } =
        toolkit::inspect_function(state, context, group_type)?;

    Ok(arguments)
}

fn check_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    origin: EquationTypeOrigin,
    signature: &ValueEquationSignature,
    arguments: &[TypeId],
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let expected_arity = signature.arguments.len();

    for equation in equations {
        let equation_arity = equation.binders.len();

        if equation_arity > expected_arity {
            state.insert_error(ErrorKind::TooManyBinders {
                signature: match origin {
                    EquationTypeOrigin::Explicit(signature_id) => Some(signature_id),
                    EquationTypeOrigin::Implicit => None,
                },
                expected: expected_arity as u32,
                actual: equation_arity as u32,
            });
        }

        for (&binder_id, &argument_type) in equation.binders.iter().zip(arguments) {
            binder::check_argument_binder(state, context, binder_id, argument_type)?;
        }

        if equation_arity > expected_arity {
            for &binder_id in &equation.binders[expected_arity..] {
                binder::infer_binder(state, context, binder_id)?;
            }
        }

        let expected_type = expected_guarded_type(context, signature, equation_arity);
        if let Some(guarded) = &equation.guarded {
            guarded::check_guarded_expression(state, context, guarded, expected_type)?;
        }
    }

    Ok(())
}

fn instantiate_pattern_arguments<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    pattern_arguments: &mut [TypeId],
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    binder::instantiate_pattern_column_types(
        state,
        context,
        pattern_arguments,
        equations.iter().flat_map(|equation| equation.binders.iter().copied().enumerate()),
    )
}

fn expected_guarded_type<Q>(
    context: &CheckContext<Q>,
    signature: &ValueEquationSignature,
    equation_arity: usize,
) -> TypeId
where
    Q: ExternalQueries,
{
    let expected_arity = signature.arguments.len();
    if equation_arity == 0 {
        signature.signature
    } else if equation_arity >= expected_arity {
        signature.result
    } else {
        let remaining = &signature.arguments[equation_arity..];
        context.intern_function_list(remaining, signature.result)
    }
}
