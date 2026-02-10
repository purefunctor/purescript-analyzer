use building_types::QueryResult;
use indexing::TermItemId;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{binder, exhaustiveness, inspect, term, toolkit, unification};
use crate::core::{Type, TypeId};
use crate::error::ErrorKind;

/// Type origin for the [`patterns`] function.
pub enum ExhaustivenessOrigin<'a> {
    /// The types of equation patterns comes from checking.
    FromSignature(&'a [TypeId]),
    /// The types of equation patterns comes from inference.
    FromType(TypeId),
}

/// Checks and reports exhaustiveness for an equation group.
pub fn patterns<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    origin: ExhaustivenessOrigin<'_>,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    match origin {
        ExhaustivenessOrigin::FromSignature(signature) => {
            let exhaustiveness =
                exhaustiveness::check_equation_patterns(state, context, signature, equations)?;
            state.report_exhaustiveness(exhaustiveness);
        }
        ExhaustivenessOrigin::FromType(t) => {
            let (arguments, _) = toolkit::extract_function_arguments(state, t);
            let exhaustiveness =
                exhaustiveness::check_equation_patterns(state, context, &arguments, equations)?;
            state.report_exhaustiveness(exhaustiveness);
        }
    };
    Ok(())
}

/// Constraints policy for the [`constraints`] function.
pub enum ConstraintsPolicy {
    /// Residual constraints are returned to the caller.
    Return,
    /// Residual constraints are eagerly reported.
    Report,
}

/// Solves constraints for an equation group.
pub fn constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    policy: ConstraintsPolicy,
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let residual = state.solve_constraints(context)?;
    match policy {
        ConstraintsPolicy::Return => Ok(residual),
        ConstraintsPolicy::Report => {
            for constraint in residual {
                let constraint = state.render_local_type(context, constraint);
                state.insert_error(ErrorKind::NoInstanceFound { constraint });
            }
            Ok(vec![])
        }
    }
}

/// Infers the type of top-level value group equations.
///
/// This function depends on the unification variable created for the current
/// binding group by [`CheckState::with_term_group`]. This function returns
/// the inferred type and residual constraints for later generalisation via
/// [`term_item::commit_value_group`].
///
/// [`term_item::commit_value_group`]: crate::algorithm::term_item::commit_value_group
pub fn infer_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    equations: &[lowering::Equation],
) -> QueryResult<(TypeId, Vec<TypeId>)>
where
    Q: ExternalQueries,
{
    let group_type = state
        .binding_group
        .lookup_term(item_id)
        .expect("invariant violated: invalid binding_group in type inference");

    infer_equations_core(state, context, group_type, equations)?;

    let origin = ExhaustivenessOrigin::FromType(group_type);
    patterns(state, context, origin, equations)?;

    let residual = constraints(state, context, ConstraintsPolicy::Return)?;
    Ok((group_type, residual))
}

/// Infers the type of value group equations.
///
/// This function infers the type of each value equation, and then checks
/// that it's a subtype of the provided `group_type`. The `group_type` is
/// usually a unification variable.
///
/// This function is used to implement inference for the following:
/// - [`lowering::TermItemIr::ValueGroup`]
/// - [`lowering::LetBindingNameGroup`]
/// - [`lowering::InstanceMemberGroup`]
pub(crate) fn infer_equations_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    group_type: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let minimum_equation_arity =
        equations.iter().map(|equation| equation.binders.len()).min().unwrap_or(0);

    for equation in equations {
        let mut argument_types = vec![];
        for &binder_id in equation.binders.iter() {
            let argument_type = binder::infer_binder(state, context, binder_id)?;
            argument_types.push(argument_type);
        }

        let result_type = state.fresh_unification_type(context);

        // Only use the minimum number of binders across equations.
        let argument_types = &argument_types[..minimum_equation_arity];
        let equation_type = state.make_function(argument_types, result_type);
        let _ = unification::subtype(state, context, equation_type, group_type)?;

        if let Some(guarded) = &equation.guarded {
            let inferred_type = term::infer_guarded_expression(state, context, guarded)?;
            let _ = unification::subtype(state, context, inferred_type, result_type)?;
        }
    }

    Ok(())
}

/// Checks the type of value group equations.
///
/// This function checks each value equation against the signature previously
/// checked by the [`check_term_signature`] and [`inspect_signature_core`]
/// functions.
///
/// This function depends on a couple of side-effects produced by the
/// [`inspect_signature_core`] function. Type variables that appear in the
/// signature are made visible through rebinding, and given constraints
/// are pushed onto the environment. See the implementation for more details.
///
/// This function solves all constraints during checking using the
/// [`CheckState::solve_constraints`] function, and reports residual
/// constraints as [`ErrorKind::NoInstanceFound`] errors.
///
/// [`check_term_signature`]: crate::algorithm::term_item::check_term_signature
/// [`inspect_signature_core`]: crate::algorithm::inspect::inspect_signature_core
pub fn check_equations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_id: lowering::TypeId,
    signature: inspect::InspectSignature,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    check_equations_core(state, context, signature_id, &signature, equations)?;

    let origin = ExhaustivenessOrigin::FromSignature(&signature.arguments);
    patterns(state, context, origin, equations)?;

    let _ = constraints(state, context, ConstraintsPolicy::Report)?;

    if let Some(variable) = signature.variables.first() {
        state.type_scope.unbind_name(&variable.variable);
    }

    Ok(())
}

pub(crate) fn check_equations_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_id: lowering::TypeId,
    signature: &inspect::InspectSignature,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let expected_arity = signature.arguments.len();

    for equation in equations {
        let equation_arity = equation.binders.len();

        if equation_arity > expected_arity {
            let expected = expected_arity as u32;
            let actual = equation_arity as u32;
            state.insert_error(ErrorKind::TooManyBinders {
                signature: signature_id,
                expected,
                actual,
            });
        }

        for (&binder_id, &argument_type) in equation.binders.iter().zip(&signature.arguments) {
            let _ = binder::check_argument_binder(state, context, binder_id, argument_type)?;
        }

        if equation_arity > expected_arity {
            let extra_binders = &equation.binders[expected_arity..];
            for &binder_id in extra_binders {
                let _ = binder::infer_binder(state, context, binder_id)?;
            }
        }

        // Compute expected result type based on how many binders there
        // are on each equation, wrapping remaining arguments if partial.
        //
        // foo :: forall a. a -> a -> Int
        // foo = \a b -> a + b
        // foo a = \b -> a + b
        // foo a b = a + b
        //
        // signature.arguments := [a, a]
        // signature.result    := Int
        //
        // expected_type :=
        //   0 binders := forall a. a -> a -> Int
        //   1 binder  := a -> Int
        //   2 binders := Int
        //
        // This matters for type synonyms that expand to functions. The
        // return type synonym introduces hidden function arrows that
        // increase the expected arity after expansion.
        //
        // type ReturnsInt a = a -> Int
        //
        // bar :: forall a. ReturnsInt a -> ReturnsInt a
        // bar = \f -> f
        // bar f = f
        // bar f a = f a
        //
        // signature.arguments := [ReturnsInt a, a]
        // signature.result    := Int
        //
        // expected_type :=
        //   0 binders := forall a. ReturnsInt a -> ReturnsInt a
        //   1 binder  := ReturnsInt a
        //   2 binders := Int
        let expected_type = if equation_arity == 0 {
            signature.function
        } else if equation_arity >= expected_arity {
            signature.result
        } else {
            let remaining_arguments = &signature.arguments[equation_arity..];
            remaining_arguments.iter().rfold(signature.result, |result, &argument| {
                state.storage.intern(Type::Function(argument, result))
            })
        };

        if let Some(guarded) = &equation.guarded {
            term::check_guarded_expression(state, context, guarded, expected_type)?;
        }
    }

    Ok(())
}
