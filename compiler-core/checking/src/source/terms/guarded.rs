//! Implements checking and inference rules for guarded and where expressions.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, toolkit};
use crate::source::binder;
use crate::source::terms;
use crate::source::terms::form_let;
use crate::state::CheckState;

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

    if let Some(binder) = guard.binder {
        let expression_type = terms::infer_expression(state, context, expression)?;
        let expression_type = toolkit::instantiate_constrained(state, context, expression_type)?;

        binder::check_binder(state, context, binder, expression_type)?;
    } else {
        terms::check_expression(state, context, expression, context.prim.boolean)?;
    }

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
