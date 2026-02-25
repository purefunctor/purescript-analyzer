//! Implements equation checking and inference rules for value groups.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, exhaustive, toolkit, unification};
use crate::error::ErrorKind;
use crate::source::terms::form_let;
use crate::source::{binder, terms};
use crate::state::CheckState;

/// Infers the type of value group equations.
///
/// For each equation: infer binders, create a result unification variable,
/// build the function type, and subtype against `group_type`. Then infer
/// the guarded expression and subtype against the result type.
pub fn infer_equations_core<Q>(
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

        let result_type = state.fresh_unification(context.queries, context.prim.t);

        let argument_types = &argument_types[..minimum_equation_arity];
        let equation_type =
            context.intern_function_chain(argument_types.iter().copied(), result_type);
        unification::subtype(state, context, equation_type, group_type)?;

        if let Some(guarded) = &equation.guarded {
            let inferred_type = infer_guarded_expression(state, context, guarded)?;
            unification::subtype(state, context, inferred_type, result_type)?;
        }
    }

    let toolkit::InspectFunction { arguments, .. } =
        toolkit::inspect_function(state, context, group_type)?;

    let exhaustiveness =
        exhaustive::check_equation_patterns(state, context, &arguments, equations)?;
    state.report_exhaustiveness(context, exhaustiveness);

    Ok(())
}

/// Checks value group equations against a signature.
///
/// For each equation: check binders against signature arguments, compute
/// expected result type based on equation arity vs signature arity, then
/// check the guarded expression.
pub fn check_equations_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_id: lowering::TypeId,
    arguments: &[TypeId],
    result: TypeId,
    function: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let expected_arity = arguments.len();

    for equation in equations {
        let equation_arity = equation.binders.len();

        if equation_arity > expected_arity {
            state.insert_error(ErrorKind::TooManyBinders {
                signature: signature_id,
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

        let expected_type = if equation_arity == 0 {
            function
        } else if equation_arity >= expected_arity {
            result
        } else {
            let remaining = &arguments[equation_arity..];
            context.intern_function_chain(remaining.iter().copied(), result)
        };

        if let Some(guarded) = &equation.guarded {
            check_guarded_expression(state, context, guarded, expected_type)?;
        }
    }

    let exhaustiveness = exhaustive::check_equation_patterns(state, context, arguments, equations)?;
    state.report_exhaustiveness(context, exhaustiveness);

    Ok(())
}

pub fn infer_guarded_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    guarded: &lowering::GuardedExpression,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match guarded {
        lowering::GuardedExpression::Unconditional { where_expression } => {
            let Some(w) = where_expression else {
                return Ok(context.unknown("missing guarded expression"));
            };
            infer_where_expression(state, context, w)
        }
        lowering::GuardedExpression::Conditionals { pattern_guarded } => {
            let mut inferred_type = context.unknown("empty conditionals");
            for pattern_guarded in pattern_guarded.iter() {
                for pattern_guard in pattern_guarded.pattern_guards.iter() {
                    check_pattern_guard(state, context, pattern_guard)?;
                }
                if let Some(w) = &pattern_guarded.where_expression {
                    inferred_type = infer_where_expression(state, context, w)?;
                }
            }
            Ok(inferred_type)
        }
    }
}

pub fn check_guarded_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    guarded: &lowering::GuardedExpression,
    expected: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    match guarded {
        lowering::GuardedExpression::Unconditional { where_expression } => {
            let Some(w) = where_expression else {
                return Ok(());
            };
            check_where_expression(state, context, w, expected)?;
            Ok(())
        }
        lowering::GuardedExpression::Conditionals { pattern_guarded } => {
            for pattern_guarded in pattern_guarded.iter() {
                for pattern_guard in pattern_guarded.pattern_guards.iter() {
                    check_pattern_guard(state, context, pattern_guard)?;
                }
                if let Some(w) = &pattern_guarded.where_expression {
                    check_where_expression(state, context, w, expected)?;
                }
            }
            Ok(())
        }
    }
}

fn check_pattern_guard<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    guard: &lowering::PatternGuard,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(expression) = guard.expression else {
        return Ok(());
    };

    let expression_type = super::infer_expression(state, context, expression)?;

    let Some(binder) = guard.binder else {
        return Ok(());
    };

    binder::check_binder(state, context, binder, expression_type)?;

    Ok(())
}

pub fn infer_where_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    where_expression: &lowering::WhereExpression,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    form_let::check_let_chunks(state, context, &where_expression.bindings)?;

    let Some(expression) = where_expression.expression else {
        return Ok(context.unknown("missing where expression"));
    };

    terms::infer_expression(state, context, expression)
}

fn check_where_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    where_expression: &lowering::WhereExpression,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    form_let::check_let_chunks(state, context, &where_expression.bindings)?;

    let Some(expression) = where_expression.expression else {
        return Ok(context.unknown("missing where expression"));
    };

    terms::check_expression(state, context, expression, expected)
}
