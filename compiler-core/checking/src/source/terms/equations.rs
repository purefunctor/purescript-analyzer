//! Implements equation checking and inference rules for value groups.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, exhaustive, signature, toolkit, unification};
use crate::error::ErrorKind;
use crate::source::terms::form_let;
use crate::source::{binder, terms};
use crate::state::CheckState;

pub enum EquationTypeOrigin {
    Explicit(lowering::TypeId),
    Implicit,
}

pub enum EquationMode {
    Check { origin: EquationTypeOrigin, expected_type: TypeId },
    Infer { group_type: TypeId },
}

#[derive(Copy, Clone, Debug)]
enum GuardedExpressionMode {
    Infer,
    Check { expected: TypeId },
}

#[derive(Copy, Clone, Debug)]
enum WhereExpressionMode {
    Infer,
    Check { expected: TypeId },
}

pub struct EquationSet {
    pub arguments: Vec<TypeId>,
}

pub fn analyse_equation_set<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mode: EquationMode,
    equations: &[lowering::Equation],
) -> QueryResult<EquationSet>
where
    Q: ExternalQueries,
{
    match mode {
        EquationMode::Check { origin, expected_type } => {
            let required =
                equations.iter().map(|equation| equation.binders.len()).max().unwrap_or(0);

            let signature::SkolemisedSignature { substitution, constraints: _, arguments, result } =
                signature::expect_signature_patterns(state, context, expected_type, required)?;

            let function = context.intern_function_list(&arguments, result);
            state.with_implicit(context, &substitution, |state| {
                check_equations_core(
                    state, context, origin, &arguments, result, function, equations,
                )
            })?;

            Ok(EquationSet { arguments })
        }
        EquationMode::Infer { group_type } => {
            infer_equation_set(state, context, group_type, equations)
        }
    }
}

pub fn compute_equation_exhaustiveness<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    set: &EquationSet,
    equations: &[lowering::Equation],
) -> QueryResult<exhaustive::ExhaustivenessReport>
where
    Q: ExternalQueries,
{
    exhaustive::check_equation_patterns(state, context, &set.arguments, equations)
}

/// Infers the type of value group equations.
///
/// For each equation: infer binders, create a result unification variable,
/// build the function type, and subtype against `group_type`. Then infer
/// the guarded expression and subtype against the result type.
fn infer_equation_set<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    group_type: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<EquationSet>
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
        let equation_type = context.intern_function_list(argument_types, result_type);
        unification::subtype(state, context, equation_type, group_type)?;

        if let Some(guarded) = &equation.guarded {
            let inferred_type = infer_guarded_expression(state, context, guarded)?;
            unification::subtype(state, context, inferred_type, result_type)?;
        }
    }

    let toolkit::InspectFunction { arguments, .. } =
        toolkit::inspect_function(state, context, group_type)?;

    Ok(EquationSet { arguments })
}

/// Checks value group equations against a signature.
///
/// For each equation: check binders against signature arguments, compute
/// expected result type based on equation arity vs signature arity, then
/// check the guarded expression.
pub fn check_equations_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    origin: EquationTypeOrigin,
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

        let expected_type = if equation_arity == 0 {
            function
        } else if equation_arity >= expected_arity {
            result
        } else {
            let remaining = &arguments[equation_arity..];
            context.intern_function_list(remaining, result)
        };

        if let Some(guarded) = &equation.guarded {
            check_guarded_expression(state, context, guarded, expected_type)?;
        }
    }

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
    guarded_expression_core(state, context, guarded, GuardedExpressionMode::Infer)
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
    guarded_expression_core(state, context, guarded, GuardedExpressionMode::Check { expected })?;
    Ok(())
}

fn guarded_expression_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    guarded: &lowering::GuardedExpression,
    mode: GuardedExpressionMode,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match guarded {
        lowering::GuardedExpression::Unconditional { where_expression } => {
            let Some(where_expression) = where_expression else {
                return match mode {
                    GuardedExpressionMode::Infer => {
                        Ok(context.unknown("missing guarded expression"))
                    }
                    GuardedExpressionMode::Check { expected } => Ok(expected),
                };
            };

            match mode {
                GuardedExpressionMode::Infer => {
                    infer_where_expression(state, context, where_expression)
                }
                GuardedExpressionMode::Check { expected } => {
                    check_where_expression(state, context, where_expression, expected)
                }
            }
        }
        lowering::GuardedExpression::Conditionals { pattern_guarded } => {
            let expected_type = match mode {
                GuardedExpressionMode::Infer => {
                    state.fresh_unification(context.queries, context.prim.t)
                }
                GuardedExpressionMode::Check { expected } => expected,
            };

            for pattern_guarded in pattern_guarded.iter() {
                for pattern_guard in pattern_guarded.pattern_guards.iter() {
                    check_pattern_guard(state, context, pattern_guard)?;
                }
                if let Some(where_expression) = &pattern_guarded.where_expression {
                    check_where_expression(state, context, where_expression, expected_type)?;
                }
            }

            Ok(expected_type)
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
    where_expression_core(state, context, where_expression, WhereExpressionMode::Infer)
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
    where_expression_core(state, context, where_expression, WhereExpressionMode::Check { expected })
}

fn where_expression_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    where_expression: &lowering::WhereExpression,
    mode: WhereExpressionMode,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    form_let::check_let_chunks(state, context, &where_expression.bindings)?;

    let Some(expression) = where_expression.expression else {
        return Ok(context.unknown("missing where expression"));
    };

    match mode {
        WhereExpressionMode::Infer => terms::infer_expression(state, context, expression),
        WhereExpressionMode::Check { expected } => {
            terms::check_expression(state, context, expression, expected)
        }
    }
}
