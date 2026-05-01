use std::iter;

use building_types::QueryResult;
use itertools::{Itertools, Position};

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, toolkit, unification};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::source::binder;
use crate::source::terms::{application, form_let};
use crate::state::CheckState;

enum DoStep<'a> {
    Bind {
        statement: lowering::DoStatementId,
        binder_type: TypeId,
        expression: Option<lowering::ExpressionId>,
    },
    Discard {
        statement: lowering::DoStatementId,
        expression: Option<lowering::ExpressionId>,
    },
    Let {
        statement: lowering::DoStatementId,
        statements: &'a [lowering::LetBindingChunk],
    },
}

/// Lookup `bind` from resolution, or synthesize `?m ?a -> (?a -> ?m ?b) -> ?m ?b`.
pub fn lookup_or_synthesise_bind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(resolution) = resolution {
        toolkit::lookup_term_variable(state, context, resolution)
    } else {
        let m = state.fresh_unification(context.queries, context.prim.type_to_type);
        let a = state.fresh_unification(context.queries, context.prim.t);
        let b = state.fresh_unification(context.queries, context.prim.t);
        let m_a = context.intern_application(m, a);
        let m_b = context.intern_application(m, b);
        let a_to_m_b = context.intern_function(a, m_b);
        Ok(context.intern_function_list(&[m_a, a_to_m_b], m_b))
    }
}

/// Lookup `discard` from resolution, or synthesize `?m ?a -> (?a -> ?m ?b) -> ?m ?b`.
pub fn lookup_or_synthesise_discard<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    // Same shape as bind
    lookup_or_synthesise_bind(state, context, resolution)
}

/// Lookup `map` from resolution, or synthesize `(?a -> ?b) -> ?f ?a -> ?f ?b`.
pub fn lookup_or_synthesise_map<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(resolution) = resolution {
        toolkit::lookup_term_variable(state, context, resolution)
    } else {
        let f = state.fresh_unification(context.queries, context.prim.type_to_type);
        let a = state.fresh_unification(context.queries, context.prim.t);
        let b = state.fresh_unification(context.queries, context.prim.t);
        let f_a = context.intern_application(f, a);
        let f_b = context.intern_application(f, b);
        let a_to_b = context.intern_function(a, b);
        Ok(context.intern_function_list(&[a_to_b, f_a], f_b))
    }
}

/// Lookup `apply` from resolution, or synthesize `?f (?a -> ?b) -> ?f ?a -> ?f ?b`.
pub fn lookup_or_synthesise_apply<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(resolution) = resolution {
        toolkit::lookup_term_variable(state, context, resolution)
    } else {
        let f = state.fresh_unification(context.queries, context.prim.type_to_type);
        let a = state.fresh_unification(context.queries, context.prim.t);
        let b = state.fresh_unification(context.queries, context.prim.t);
        let a_to_b = context.intern_function(a, b);
        let f_a_to_b = context.intern_application(f, a_to_b);
        let f_a = context.intern_application(f, a);
        let f_b = context.intern_application(f, b);
        Ok(context.intern_function_list(&[f_a_to_b, f_a], f_b))
    }
}

/// Lookup `pure` from resolution, or synthesize `?a -> ?f ?a`.
pub fn lookup_or_synthesise_pure<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(resolution) = resolution {
        toolkit::lookup_term_variable(state, context, resolution)
    } else {
        let f = state.fresh_unification(context.queries, context.prim.type_to_type);
        let a = state.fresh_unification(context.queries, context.prim.t);
        let f_a = context.intern_application(f, a);
        Ok(context.intern_function(a, f_a))
    }
}

pub fn infer_do<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bind: Option<lowering::TermVariableResolution>,
    discard: Option<lowering::TermVariableResolution>,
    statement_ids: &[lowering::DoStatementId],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    // First, perform a forward pass where variable bindings are bound
    // to unification variables. Let bindings are not checked here to
    // avoid premature solving of unification variables. Instead, they
    // are checked inline during the statement checking loop.
    let mut steps = vec![];
    for &statement_id in statement_ids.iter() {
        let Some(statement) = context.lowered.info.get_do_statement(statement_id) else {
            continue;
        };
        match statement {
            lowering::DoStatement::Bind { binder, expression } => {
                let binder_type = if let Some(binder) = binder {
                    binder::infer_binder(state, context, *binder)?
                } else {
                    state.fresh_unification(context.queries, context.prim.t)
                };
                steps.push(DoStep::Bind {
                    statement: statement_id,
                    binder_type,
                    expression: *expression,
                });
            }
            lowering::DoStatement::Let { statements } => {
                steps.push(DoStep::Let { statement: statement_id, statements });
            }
            lowering::DoStatement::Discard { expression } => {
                steps.push(DoStep::Discard { statement: statement_id, expression: *expression });
            }
        }
    }

    let action_count = steps
        .iter()
        .filter(|step| matches!(step, DoStep::Bind { .. } | DoStep::Discard { .. }))
        .count();

    let (has_bind_step, has_discard_step) = {
        let mut has_bind = false;
        let mut has_discard = false;
        for (position, statement) in steps.iter().with_position() {
            let is_final = matches!(position, Position::Last | Position::Only);
            match statement {
                DoStep::Bind { .. } => has_bind = true,
                DoStep::Discard { .. } if !is_final => has_discard = true,
                _ => (),
            }
        }
        (has_bind, has_discard)
    };

    let bind_type = if has_bind_step {
        lookup_or_synthesise_bind(state, context, bind)?
    } else {
        context.unknown("unused bind")
    };

    let discard_type = if has_discard_step {
        lookup_or_synthesise_discard(state, context, discard)?
    } else {
        context.unknown("unused discard")
    };

    let pure_expression = match steps.iter().last() {
        Some(statement) => match statement {
            // Technically valid, syntactically disallowed. This allows
            // partially-written do expressions to infer, with a friendly
            // warning to nudge the user that `bind` is prohibited.
            DoStep::Bind { statement, expression, .. } => {
                state.with_error_crumb(ErrorCrumb::InferringDoBind(*statement), |state| {
                    state.insert_error(ErrorKind::InvalidFinalBind);
                });
                expression
            }
            DoStep::Discard { expression, .. } => expression,
            DoStep::Let { statement, .. } => {
                state.with_error_crumb(ErrorCrumb::CheckingDoLet(*statement), |state| {
                    state.insert_error(ErrorKind::InvalidFinalLet);
                });
                return Ok(context.unknown("invalid final let"));
            }
        },
        None => {
            state.insert_error(ErrorKind::EmptyDoBlock);
            return Ok(context.unknown("empty do block"));
        }
    };

    // If either don't actually have expressions, it's empty!
    let Some(pure_expression) = *pure_expression else {
        state.insert_error(ErrorKind::EmptyDoBlock);
        return Ok(context.unknown("empty do block"));
    };

    // Create unification variables that each statement in the do expression
    // will unify against. The next section will get into more detail how
    // these are used. These unification variables are used to enable GHC-like
    // left-to-right checking of do expressions while maintaining the same
    // semantics as rebindable `do` in PureScript.
    let continuation_types =
        iter::repeat_with(|| state.fresh_unification(context.queries, context.prim.t))
            .take(action_count)
            .collect_vec();

    // Let's trace over the following example:
    //
    //   main = do
    //     a <- effect
    //     b <- aff
    //     pure { a, b }
    //
    // For the first statement, we know the following information. We
    // instantiate the `bind` function to prepare it for application.
    // The first argument is easy, it's just the expression_type; the
    // second argument involves synthesising a function type using the
    // `binder_type` and the `next` continuation. The application of
    // these arguments creates important unifications, listed below.
    // Additionally, we also create a unification to unify the `now`
    // type with the result of the `bind` application.
    //
    //   expression_type := Effect Int
    //   binder_type     := ?a
    //   now             := ?0
    //   next            := ?1
    //   lambda_type     := ?a -> ?1
    //
    //   bind_type       := m a -> (a -> m b) -> m b
    //                   |
    //                   := apply(expression_type)
    //                   := (Int -> Effect ?r1) -> Effect ?r1
    //                   |
    //                   := apply(lambda_type)
    //                   := Effect ?r1
    //                   |
    //                   >> ?a := Int
    //                   >> ?1 := Effect ?r1
    //                   >> ?0 := Effect ?r1
    //
    // For the second statement, we know the following information.
    // The `now` type was already solved by the previous statement,
    // and an error should surface once we check the inferred type
    // of the statement against it.
    //
    //   expression_type := Aff Int
    //   binder_type     := ?b
    //   now             := ?1 := Effect ?r1
    //   next            := ?2
    //   lambda_type     := ?b -> ?2
    //
    //   bind_type       := m a -> (a -> m b) -> m b
    //                   |
    //                   := apply(expression_type)
    //                   := (Int -> Aff ?r2) -> Aff ?r2
    //                   |
    //                   := apply(lambda_type)
    //                   := Aff ?r2
    //                   |
    //                   >> ?b := Int
    //                   >> ?2 := Aff ?r2
    //                   |
    //                   >> ?1         ~ Aff ?r2
    //                   >> Effect ?r1 ~ Aff ?r2
    //                   |
    //                   >> Oh no!
    //
    // This unification error is expected, but this left-to-right checking
    // approach significantly improves the reported error positions compared
    // to the previous approach that emulated desugared checking.

    let mut continuations = continuation_types.iter().tuple_windows::<(_, _)>();

    for step in &steps {
        match step {
            DoStep::Let { statement, statements } => {
                state.with_error_crumb(ErrorCrumb::CheckingDoLet(*statement), |state| {
                    form_let::check_let_chunks(state, context, statements)
                })?;
            }
            DoStep::Bind { statement, binder_type, expression } => {
                let Some((&now_type, &next_type)) = continuations.next() else {
                    continue;
                };
                let Some(expression) = *expression else {
                    continue;
                };
                state.with_error_crumb(ErrorCrumb::InferringDoBind(*statement), |state| {
                    let statement_type = infer_do_bind_core(
                        state,
                        context,
                        bind_type,
                        next_type,
                        expression,
                        *binder_type,
                    )?;
                    unification::subtype(state, context, statement_type, now_type)?;
                    Ok(())
                })?;
            }
            DoStep::Discard { statement, expression } => {
                let Some((&now_type, &next_type)) = continuations.next() else {
                    continue;
                };
                let Some(expression) = *expression else {
                    continue;
                };
                state.with_error_crumb(ErrorCrumb::InferringDoDiscard(*statement), |state| {
                    let statement_type =
                        infer_do_discard_core(state, context, discard_type, next_type, expression)?;
                    unification::subtype(state, context, statement_type, now_type)?;
                    Ok(())
                })?;
            }
        }
    }

    // The `first_continuation` is the overall type of the do expression,
    // built iteratively and through solving unification variables. On
    // the other hand, the `final_continuation` is the expected type for
    // the final statement in the do expression. If there is only a single
    // statement in the do expression, then these two bindings are equivalent.
    let first_continuation =
        *continuation_types.first().expect("invariant violated: empty continuation_types");
    let final_continuation =
        *continuation_types.last().expect("invariant violated: empty continuation_types");

    super::check_expression(state, context, pure_expression, final_continuation)?;

    Ok(first_continuation)
}

pub fn infer_do_bind_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bind_type: TypeId,
    continuation_type: TypeId,
    expression: lowering::ExpressionId,
    binder_type: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let expression_type = super::infer_expression(state, context, expression)?;
    let lambda_type = context.intern_function(binder_type, continuation_type);

    let Some(application::GenericApplication { argument, result }) =
        application::check_generic_application(state, context, bind_type)?
    else {
        return Ok(context.unknown("invalid function application"));
    };
    unification::subtype(state, context, expression_type, argument)?;

    let Some(application::GenericApplication { argument, result }) =
        application::check_generic_application(state, context, result)?
    else {
        return Ok(context.unknown("invalid function application"));
    };
    unification::subtype(state, context, lambda_type, argument)?;

    Ok(result)
}

pub fn infer_do_discard_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    discard_type: TypeId,
    continuation_type: TypeId,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let binder_type = state.fresh_unification(context.queries, context.prim.t);
    infer_do_bind_core(state, context, discard_type, continuation_type, expression, binder_type)
}
