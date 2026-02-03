use std::iter;

use building_types::QueryResult;
use itertools::{Itertools, Position};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::unification::ElaborationMode;
use crate::algorithm::{
    binder, equation, exhaustiveness, inspect, kind, operator, substitute, toolkit, transfer,
    unification,
};
use crate::core::{RowField, RowType, Type, TypeId};
use crate::error::{ErrorKind, ErrorStep};

/// Checks the type of an expression.
#[tracing::instrument(skip_all, name = "check_expression")]
pub fn check_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    crate::trace_fields!(state, context, { expected = expected });
    state.with_error_step(ErrorStep::CheckingExpression(expr_id), |state| {
        let inferred = infer_expression_quiet(state, context, expr_id)?;
        let _ = unification::subtype(state, context, inferred, expected)?;
        crate::trace_fields!(state, context, { inferred = inferred, expected = expected });
        Ok(inferred)
    })
}

fn check_expression_argument<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::CheckingExpression(expr_id), |state| {
        let inferred = infer_expression_quiet(state, context, expr_id)?;
        // Instantiate the inferred type when the expected type is not
        // polymorphic, so constraints are collected as wanted rather
        // than leaking into unification. Skipped for higher-rank
        // arguments where constraints must match structurally.
        let expected = state.normalize_type(expected);
        let inferred =
            if matches!(state.storage[expected], Type::Forall(..) | Type::Constrained(..)) {
                inferred
            } else {
                toolkit::instantiate_constrained(state, inferred)
            };
        unification::subtype_with_mode(state, context, inferred, expected, ElaborationMode::No)?;
        crate::trace_fields!(state, context, { inferred = inferred, expected = expected });
        Ok(inferred)
    })
}

/// Infers the type of an expression.
#[tracing::instrument(skip_all, name = "infer_expression")]
pub fn infer_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::InferringExpression(expr_id), |state| {
        let inferred = infer_expression_quiet(state, context, expr_id)?;
        crate::trace_fields!(state, context, { inferred = inferred });
        Ok(inferred)
    })
}

fn infer_expression_quiet<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(section_result) = context.sectioned.expressions.get(&expr_id) {
        infer_sectioned_expression(state, context, expr_id, section_result)
    } else {
        infer_expression_core(state, context, expr_id)
    }
}

fn infer_sectioned_expression<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
    section_result: &sugar::SectionResult,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let parameter_types = section_result.iter().map(|&section_id| {
        let parameter_type = state.fresh_unification_type(context);
        state.term_scope.bind_section(section_id, parameter_type);
        parameter_type
    });

    let parameter_types = parameter_types.collect_vec();
    let result_type = infer_expression_core(state, context, expr_id)?;

    Ok(state.make_function(&parameter_types, result_type))
}

fn infer_expression_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    expr_id: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let unknown = context.prim.unknown;

    let Some(kind) = context.lowered.info.get_expression_kind(expr_id) else {
        return Ok(unknown);
    };

    match kind {
        lowering::ExpressionKind::Typed { expression, type_ } => {
            let Some(e) = expression else { return Ok(unknown) };
            let Some(t) = type_ else { return Ok(unknown) };

            let (t, _) = kind::infer_surface_kind(state, context, *t)?;
            check_expression(state, context, *e, t)?;

            Ok(t)
        }

        lowering::ExpressionKind::OperatorChain { .. } => {
            let (_, inferred_type) = operator::infer_operator_chain(state, context, expr_id)?;
            Ok(inferred_type)
        }

        lowering::ExpressionKind::InfixChain { head, tail } => {
            let Some(head) = *head else { return Ok(unknown) };
            infer_infix_chain(state, context, head, tail)
        }

        lowering::ExpressionKind::Negate { negate, expression } => {
            let Some(negate) = negate else { return Ok(unknown) };
            let Some(expression) = expression else { return Ok(unknown) };

            let negate_type = lookup_term_variable(state, context, *negate)?;
            check_function_term_application(state, context, negate_type, *expression)
        }

        lowering::ExpressionKind::Application { function, arguments } => {
            let Some(function) = function else { return Ok(unknown) };

            let mut function_t = infer_expression(state, context, *function)?;

            for argument in arguments.iter() {
                function_t = check_function_application(state, context, function_t, argument)?;
            }

            Ok(function_t)
        }

        lowering::ExpressionKind::IfThenElse { if_, then, else_ } => {
            infer_if_then_else(state, context, *if_, *then, *else_)
        }

        lowering::ExpressionKind::LetIn { bindings, expression } => {
            check_let_chunks(state, context, bindings)?;

            let Some(expression) = expression else { return Ok(unknown) };

            infer_expression(state, context, *expression)
        }

        lowering::ExpressionKind::Lambda { binders, expression } => {
            infer_lambda(state, context, binders, *expression)
        }

        lowering::ExpressionKind::CaseOf { trunk, branches } => {
            infer_case_of(state, context, trunk, branches)
        }

        lowering::ExpressionKind::Do { bind, discard, statements } => {
            infer_do(state, context, *bind, *discard, statements)
        }

        lowering::ExpressionKind::Ado { map, apply, pure, statements, expression } => {
            infer_ado(state, context, *map, *apply, *pure, statements, *expression)
        }

        lowering::ExpressionKind::Constructor { resolution } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };
            lookup_file_term(state, context, *file_id, *term_id)
        }

        lowering::ExpressionKind::Variable { resolution } => {
            let Some(resolution) = *resolution else { return Ok(unknown) };
            lookup_term_variable(state, context, resolution)
        }

        lowering::ExpressionKind::OperatorName { resolution } => {
            let Some((file_id, term_id)) = resolution else { return Ok(unknown) };
            lookup_file_term(state, context, *file_id, *term_id)
        }

        lowering::ExpressionKind::Section => {
            if let Some(type_id) = state.term_scope.lookup_section(expr_id) {
                Ok(type_id)
            } else {
                Ok(unknown)
            }
        }

        lowering::ExpressionKind::Hole => Ok(unknown),

        lowering::ExpressionKind::String => Ok(context.prim.string),

        lowering::ExpressionKind::Char => Ok(context.prim.char),

        lowering::ExpressionKind::Boolean { .. } => Ok(context.prim.boolean),

        lowering::ExpressionKind::Integer => Ok(context.prim.int),

        lowering::ExpressionKind::Number => Ok(context.prim.number),

        lowering::ExpressionKind::Array { array } => infer_array(state, context, array),

        lowering::ExpressionKind::Record { record } => infer_record(state, context, record),

        lowering::ExpressionKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return Ok(unknown) };
            infer_expression(state, context, *parenthesized)
        }

        lowering::ExpressionKind::RecordAccess { record, labels } => {
            let Some(record) = *record else { return Ok(unknown) };
            let Some(labels) = labels else { return Ok(unknown) };
            infer_record_access(state, context, record, labels)
        }

        lowering::ExpressionKind::RecordUpdate { record, updates } => {
            let Some(record) = *record else { return Ok(unknown) };
            infer_record_update(state, context, record, updates)
        }
    }
}

fn infer_if_then_else<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    if_: Option<lowering::ExpressionId>,
    then: Option<lowering::ExpressionId>,
    else_: Option<lowering::ExpressionId>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(if_) = if_ {
        check_expression(state, context, if_, context.prim.boolean)?;
    }

    let result_type = state.fresh_unification_type(context);

    if let Some(then) = then {
        check_expression(state, context, then, result_type)?;
    }

    if let Some(else_) = else_ {
        check_expression(state, context, else_, result_type)?;
    }

    Ok(result_type)
}

fn infer_infix_chain<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    head: lowering::ExpressionId,
    tail: &[lowering::InfixPair<lowering::ExpressionId>],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut infix_type = infer_expression(state, context, head)?;

    for lowering::InfixPair { tick, element } in tail.iter() {
        let Some(tick) = tick else { return Ok(context.prim.unknown) };
        let Some(element) = element else { return Ok(context.prim.unknown) };

        let tick_type = infer_expression(state, context, *tick)?;

        let applied_tick = check_function_application_core(
            state,
            context,
            tick_type,
            infix_type,
            |state, context, infix_type, expected_type| {
                let _ = unification::subtype(state, context, infix_type, expected_type)?;
                Ok(infix_type)
            },
        )?;

        infix_type = check_function_term_application(state, context, applied_tick, *element)?;
    }

    Ok(infix_type)
}

fn infer_lambda<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binders: &[lowering::BinderId],
    expression: Option<lowering::ExpressionId>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut argument_types = vec![];

    for &binder_id in binders.iter() {
        let argument_type = state.fresh_unification_type(context);
        let _ = binder::check_binder(state, context, binder_id, argument_type)?;
        argument_types.push(argument_type);
    }

    let result_type = if let Some(body) = expression {
        let body_type = infer_expression(state, context, body)?;
        toolkit::instantiate_constrained(state, body_type)
    } else {
        state.fresh_unification_type(context)
    };

    Ok(state.make_function(&argument_types, result_type))
}

fn infer_case_of<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    trunk: &[lowering::ExpressionId],
    branches: &[lowering::CaseBranch],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let inferred_type = state.fresh_unification_type(context);

    let mut trunk_types = vec![];
    for trunk in trunk.iter() {
        let trunk_type = infer_expression(state, context, *trunk)?;
        trunk_types.push(trunk_type);
    }

    for branch in branches.iter() {
        for (binder, trunk) in branch.binders.iter().zip(&trunk_types) {
            let _ = binder::check_binder(state, context, *binder, *trunk)?;
        }
        if let Some(guarded) = &branch.guarded_expression {
            let guarded_type = infer_guarded_expression(state, context, guarded)?;
            let _ = unification::subtype(state, context, inferred_type, guarded_type)?;
        }
    }

    let exhaustiveness =
        exhaustiveness::check_case_patterns(state, context, &trunk_types, branches)?;
    state.report_exhaustiveness(exhaustiveness);

    Ok(inferred_type)
}

/// Lookup `bind` from resolution, or synthesize `?m ?a -> (?a -> ?m ?b) -> ?m ?b`.
fn lookup_or_synthesise_bind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(resolution) = resolution {
        lookup_term_variable(state, context, resolution)
    } else {
        let m = state.fresh_unification_kinded(context.prim.type_to_type);
        let a = state.fresh_unification_type(context);
        let b = state.fresh_unification_type(context);
        let m_a = state.storage.intern(Type::Application(m, a));
        let m_b = state.storage.intern(Type::Application(m, b));
        let a_to_m_b = state.storage.intern(Type::Function(a, m_b));
        Ok(state.make_function(&[m_a, a_to_m_b], m_b))
    }
}

/// Lookup `discard` from resolution, or synthesize `?m ?a -> (?a -> ?m ?b) -> ?m ?b`.
fn lookup_or_synthesise_discard<Q>(
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
fn lookup_or_synthesise_map<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(resolution) = resolution {
        lookup_term_variable(state, context, resolution)
    } else {
        let f = state.fresh_unification_kinded(context.prim.type_to_type);
        let a = state.fresh_unification_type(context);
        let b = state.fresh_unification_type(context);
        let f_a = state.storage.intern(Type::Application(f, a));
        let f_b = state.storage.intern(Type::Application(f, b));
        let a_to_b = state.storage.intern(Type::Function(a, b));
        Ok(state.make_function(&[a_to_b, f_a], f_b))
    }
}

/// Lookup `apply` from resolution, or synthesize `?f (?a -> ?b) -> ?f ?a -> ?f ?b`.
fn lookup_or_synthesise_apply<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(resolution) = resolution {
        lookup_term_variable(state, context, resolution)
    } else {
        let f = state.fresh_unification_kinded(context.prim.type_to_type);
        let a = state.fresh_unification_type(context);
        let b = state.fresh_unification_type(context);
        let a_to_b = state.storage.intern(Type::Function(a, b));
        let f_a_to_b = state.storage.intern(Type::Application(f, a_to_b));
        let f_a = state.storage.intern(Type::Application(f, a));
        let f_b = state.storage.intern(Type::Application(f, b));
        Ok(state.make_function(&[f_a_to_b, f_a], f_b))
    }
}

/// Lookup `pure` from resolution, or synthesize `?a -> ?f ?a`.
fn lookup_or_synthesise_pure<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<lowering::TermVariableResolution>,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(resolution) = resolution {
        lookup_term_variable(state, context, resolution)
    } else {
        let f = state.fresh_unification_kinded(context.prim.type_to_type);
        let a = state.fresh_unification_type(context);
        let f_a = state.storage.intern(Type::Application(f, a));
        Ok(state.storage.intern(Type::Function(a, f_a)))
    }
}

#[tracing::instrument(skip_all, name = "infer_do")]
fn infer_do<Q>(
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
                    state.fresh_unification_type(context)
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
        context.prim.unknown
    };

    let discard_type = if has_discard_step {
        lookup_or_synthesise_discard(state, context, discard)?
    } else {
        context.prim.unknown
    };

    let pure_expression = match steps.iter().last() {
        Some(statement) => match statement {
            // Technically valid, syntactically disallowed. This allows
            // partially-written do expressions to infer, with a friendly
            // warning to nudge the user that `bind` is prohibited.
            DoStep::Bind { statement, expression, .. } => {
                state.with_error_step(ErrorStep::InferringDoBind(*statement), |state| {
                    state.insert_error(ErrorKind::InvalidFinalBind);
                });
                expression
            }
            DoStep::Discard { expression, .. } => expression,
            DoStep::Let { statement, .. } => {
                state.with_error_step(ErrorStep::CheckingDoLet(*statement), |state| {
                    state.insert_error(ErrorKind::InvalidFinalLet);
                });
                return Ok(context.prim.unknown);
            }
        },
        None => {
            state.insert_error(ErrorKind::EmptyDoBlock);
            return Ok(context.prim.unknown);
        }
    };

    // If either don't actually have expressions, it's empty!
    let Some(pure_expression) = *pure_expression else {
        state.insert_error(ErrorKind::EmptyDoBlock);
        return Ok(context.prim.unknown);
    };

    // Create unification variables that each statement in the do expression
    // will unify against. The next section will get into more detail how
    // these are used. These unification variables are used to enable GHC-like
    // left-to-right checking of do expressions while maintaining the same
    // semantics as rebindable `do` in PureScript.
    let continuation_types = iter::repeat_with(|| state.fresh_unification_type(context))
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
                let error_step = ErrorStep::CheckingDoLet(*statement);
                state.with_error_step(error_step, |state| {
                    check_let_chunks(state, context, statements)
                })?;
            }
            DoStep::Bind { statement, binder_type, expression } => {
                let Some((&now_type, &next_type)) = continuations.next() else {
                    continue;
                };
                let Some(expression) = *expression else {
                    continue;
                };
                let arguments = InferDoBind {
                    statement: *statement,
                    bind_type,
                    now_type,
                    next_type,
                    expression,
                    binder_type: *binder_type,
                };
                infer_do_bind(state, context, arguments)?;
            }
            DoStep::Discard { statement, expression } => {
                let Some((&now_type, &next_type)) = continuations.next() else {
                    continue;
                };
                let Some(expression) = *expression else {
                    continue;
                };
                let arguments = InferDoDiscard {
                    statement: *statement,
                    discard_type,
                    now_type,
                    next_type,
                    expression,
                };
                infer_do_discard(state, context, arguments)?;
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

    check_expression(state, context, pure_expression, final_continuation)?;

    Ok(first_continuation)
}

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

enum AdoStep<'a> {
    Action {
        statement: lowering::DoStatementId,
        binder_type: TypeId,
        expression: lowering::ExpressionId,
    },
    Let {
        statement: lowering::DoStatementId,
        statements: &'a [lowering::LetBindingChunk],
    },
}

#[tracing::instrument(skip_all, name = "infer_ado")]
fn infer_ado<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    map: Option<lowering::TermVariableResolution>,
    apply: Option<lowering::TermVariableResolution>,
    pure: Option<lowering::TermVariableResolution>,
    statement_ids: &[lowering::DoStatementId],
    expression: Option<lowering::ExpressionId>,
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
                    state.fresh_unification_type(context)
                };
                let Some(expression) = *expression else { continue };
                steps.push(AdoStep::Action { statement: statement_id, binder_type, expression });
            }
            lowering::DoStatement::Let { statements } => {
                steps.push(AdoStep::Let { statement: statement_id, statements });
            }
            lowering::DoStatement::Discard { expression } => {
                let binder_type = state.fresh_unification_type(context);
                let Some(expression) = *expression else { continue };
                steps.push(AdoStep::Action { statement: statement_id, binder_type, expression });
            }
        }
    }

    let binder_types = steps.iter().filter_map(|step| match step {
        AdoStep::Action { binder_type, .. } => Some(*binder_type),
        AdoStep::Let { .. } => None,
    });

    let binder_types = binder_types.collect_vec();

    // For ado blocks with no bindings, we check let statements and then
    // apply pure to the expression.
    //
    //   pure_type  := a -> f a
    //   expression := t
    if binder_types.is_empty() {
        for step in &steps {
            if let AdoStep::Let { statement, statements } = step {
                state.with_error_step(ErrorStep::CheckingAdoLet(*statement), |state| {
                    check_let_chunks(state, context, statements)
                })?;
            }
        }
        return if let Some(expression) = expression {
            let pure_type = lookup_or_synthesise_pure(state, context, pure)?;
            check_function_term_application(state, context, pure_type, expression)
        } else {
            state.insert_error(ErrorKind::EmptyAdoBlock);
            Ok(context.prim.unknown)
        };
    }

    // Create a fresh unification variable for the in_expression.
    // Inferring expression directly may solve the unification variables
    // introduced in the first pass. This is undesirable, because the
    // errors would be attributed incorrectly to the ado statements
    // rather than the in-expression itself.
    //
    //   ado
    //     a <- pure "Hello!"
    //     _ <- pure 42
    //     in Message a
    //
    //   in_expression      :: Effect Message
    //   in_expression_type := ?in_expression
    //   lambda_type        := ?a -> ?b -> ?in_expression
    let in_expression_type = state.fresh_unification_type(context);
    let lambda_type = state.make_function(&binder_types, in_expression_type);

    // The desugared form of an ado-expression is a forward applicative
    // pipeline, unlike do-notation which works inside-out. The example
    // above desugars to the following expression:
    //
    //   (\a _ -> Message a) <$> (pure "Hello!") <*> (pure 42)
    //
    // To emulate this, we process steps in source order. Let bindings
    // are checked inline between map/apply operations. The first action
    // uses infer_ado_map, and subsequent actions use infer_ado_apply.
    //
    //   map_type        :: (a -> b) -> f a -> f b
    //   lambda_type     := ?a -> ?b -> ?in_expression
    //
    //   expression_type         := Effect String
    //   map(lambda, expression) := Effect (?b -> ?in_expression)
    //                           >>
    //                           >> ?a := String
    //
    //   continuation_type := Effect (?b -> ?in_expression)

    // Lazily compute map_type and apply_type only when needed.
    // - 1 action: only map is needed
    // - 2+ actions: map and apply are needed
    let action_count = binder_types.len();

    let map_type = lookup_or_synthesise_map(state, context, map)?;

    let apply_type = if action_count > 1 {
        lookup_or_synthesise_apply(state, context, apply)?
    } else {
        context.prim.unknown
    };

    let mut continuation_type = None;

    for step in &steps {
        match step {
            AdoStep::Let { statement, statements } => {
                let error_step = ErrorStep::CheckingAdoLet(*statement);
                state.with_error_step(error_step, |state| {
                    check_let_chunks(state, context, statements)
                })?;
            }
            AdoStep::Action { statement, expression, .. } => {
                let statement_type = if let Some(continuation_type) = continuation_type {
                    // Then, the infer_ado_apply rule applies `apply` to the inferred
                    // expression type and the continuation type that is a function
                    // contained within some container, like Effect.
                    //
                    //   apply_type        := f (x -> y) -> f x -> f y
                    //   continuation_type := Effect (?b -> ?in_expression)
                    //
                    //   expression_type                 := Effect Int
                    //   apply(continuation, expression) := Effect ?in_expression
                    //                                   >>
                    //                                   >> ?b := Int
                    //
                    //   continuation_type := Effect ?in_expression
                    let arguments = InferAdoApply {
                        statement: *statement,
                        apply_type,
                        continuation_type,
                        expression: *expression,
                    };
                    infer_ado_apply(state, context, arguments)?
                } else {
                    let arguments = InferAdoMap {
                        statement: *statement,
                        map_type,
                        lambda_type,
                        expression: *expression,
                    };
                    infer_ado_map(state, context, arguments)?
                };
                continuation_type = Some(statement_type);
            }
        }
    }

    // Finally, check the in-expression against in_expression.
    // At this point the binder unification variables have been solved
    // to concrete types, so errors are attributed to the in-expression.
    //
    //   in_expression      :: Effect Message
    //   in_expression_type := Effect ?in_expression
    //                      >>
    //                      >> ?in_expression := Message
    if let Some(expression) = expression {
        check_expression(state, context, expression, in_expression_type)?;
    }

    let Some(continuation_type) = continuation_type else {
        unreachable!("invariant violated: impossible empty steps");
    };

    Ok(continuation_type)
}

fn infer_array<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    array: &[lowering::ExpressionId],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let inferred_type = state.fresh_unification_type(context);

    for expression in array.iter() {
        let element_type = infer_expression(state, context, *expression)?;
        unification::subtype(state, context, element_type, inferred_type)?;
    }

    let array_type = state.storage.intern(Type::Application(context.prim.array, inferred_type));

    Ok(array_type)
}

fn infer_record<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: &[lowering::ExpressionRecordItem],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut fields = vec![];

    for field in record.iter() {
        match field {
            lowering::ExpressionRecordItem::RecordField { name, value } => {
                let Some(name) = name else { continue };
                let Some(value) = value else { continue };

                let label = SmolStr::clone(name);
                let id = infer_expression(state, context, *value)?;

                // Instantiate to avoid polymorphic types in record fields.
                let id = toolkit::instantiate_forall(state, id);
                let id = toolkit::collect_constraints(state, id);

                fields.push(RowField { label, id });
            }
            lowering::ExpressionRecordItem::RecordPun { name, resolution } => {
                let Some(name) = name else { continue };
                let Some(resolution) = resolution else { continue };

                let label = SmolStr::clone(name);
                let id = lookup_term_variable(state, context, *resolution)?;

                // Instantiate to avoid polymorphic types in record fields.
                let id = toolkit::instantiate_forall(state, id);
                let id = toolkit::collect_constraints(state, id);

                fields.push(RowField { label, id });
            }
        }
    }

    let row_type = RowType::from_unsorted(fields, None);
    let row_type = state.storage.intern(Type::Row(row_type));

    let record_type = state.storage.intern(Type::Application(context.prim.record, row_type));

    Ok(record_type)
}

fn infer_record_access<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: lowering::ExpressionId,
    labels: &[SmolStr],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut current_type = infer_expression(state, context, record)?;

    for label in labels.iter() {
        let label = SmolStr::clone(label);

        let field_type = state.fresh_unification_type(context);

        let tail_type = state.fresh_unification_kinded(context.prim.row_type);

        let row_type =
            RowType::from_unsorted(vec![RowField { label, id: field_type }], Some(tail_type));

        let row_type = state.storage.intern(Type::Row(row_type));
        let record_type = state.storage.intern(Type::Application(context.prim.record, row_type));

        unification::subtype(state, context, current_type, record_type)?;
        current_type = field_type;
    }

    Ok(current_type)
}

fn infer_record_update<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: lowering::ExpressionId,
    updates: &[lowering::RecordUpdate],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let (input_fields, output_fields, tail) = infer_record_updates(state, context, updates)?;

    let input_row = RowType::from_unsorted(input_fields, Some(tail));
    let input_row = state.storage.intern(Type::Row(input_row));
    let input_record = state.storage.intern(Type::Application(context.prim.record, input_row));

    let output_row = RowType::from_unsorted(output_fields, Some(tail));
    let output_row = state.storage.intern(Type::Row(output_row));
    let output_record = state.storage.intern(Type::Application(context.prim.record, output_row));

    check_expression(state, context, record, input_record)?;

    Ok(output_record)
}

fn infer_record_updates<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    updates: &[lowering::RecordUpdate],
) -> QueryResult<(Vec<RowField>, Vec<RowField>, TypeId)>
where
    Q: ExternalQueries,
{
    let mut input_fields = vec![];
    let mut output_fields = vec![];

    for update in updates {
        match update {
            lowering::RecordUpdate::Leaf { name, expression } => {
                let Some(name) = name else { continue };
                let label = SmolStr::clone(name);

                let input_id = state.fresh_unification_type(context);
                let output_id = if let Some(expression) = expression {
                    infer_expression(state, context, *expression)?
                } else {
                    context.prim.unknown
                };

                input_fields.push(RowField { label: label.clone(), id: input_id });
                output_fields.push(RowField { label, id: output_id });
            }
            lowering::RecordUpdate::Branch { name, updates } => {
                let Some(name) = name else { continue };
                let label = SmolStr::clone(name);

                let (in_f, out_f, tail) = infer_record_updates(state, context, updates)?;

                let in_row = RowType::from_unsorted(in_f, Some(tail));
                let in_row = state.storage.intern(Type::Row(in_row));
                let in_id = state.storage.intern(Type::Application(context.prim.record, in_row));

                let out_row = RowType::from_unsorted(out_f, Some(tail));
                let out_row = state.storage.intern(Type::Row(out_row));
                let out_id = state.storage.intern(Type::Application(context.prim.record, out_row));

                input_fields.push(RowField { label: label.clone(), id: in_id });
                output_fields.push(RowField { label, id: out_id });
            }
        }
    }

    let tail = state.fresh_unification_kinded(context.prim.row_type);

    Ok((input_fields, output_fields, tail))
}

fn lookup_term_variable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: lowering::TermVariableResolution,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match resolution {
        lowering::TermVariableResolution::Binder(binder_id) => {
            Ok(state.term_scope.lookup_binder(binder_id).unwrap_or(context.prim.unknown))
        }
        lowering::TermVariableResolution::Let(let_binding_id) => {
            Ok(state.term_scope.lookup_let(let_binding_id).unwrap_or(context.prim.unknown))
        }
        lowering::TermVariableResolution::RecordPun(pun_id) => {
            Ok(state.term_scope.lookup_pun(pun_id).unwrap_or(context.prim.unknown))
        }
        lowering::TermVariableResolution::Reference(file_id, term_id) => {
            lookup_file_term(state, context, file_id, term_id)
        }
    }
}

struct InferAdoMap {
    statement: lowering::DoStatementId,
    map_type: TypeId,
    lambda_type: TypeId,
    expression: lowering::ExpressionId,
}

#[tracing::instrument(skip_all, name = "infer_ado_map")]
fn infer_ado_map<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: InferAdoMap,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let InferAdoMap { statement, map_type, lambda_type, expression } = arguments;
    state.with_error_step(ErrorStep::InferringAdoMap(statement), |state| {
        infer_ado_map_core(state, context, map_type, lambda_type, expression)
    })
}

fn infer_ado_map_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    map_type: TypeId,
    lambda_type: TypeId,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    // expression_type := f a
    let expression_type = infer_expression(state, context, expression)?;

    // map_type    := (a -> b) -> f a -> f b
    // lambda_type := (a -> b)
    let map_applied = check_function_application_core(
        state,
        context,
        map_type,
        lambda_type,
        |state, context, lambda_type, expected_type| {
            let _ = unification::subtype(state, context, lambda_type, expected_type)?;
            Ok(lambda_type)
        },
    )?;

    // map_applied     := f a -> f b
    // expression_type := f a
    // final_type      := f b
    check_function_application_core(
        state,
        context,
        map_applied,
        expression_type,
        |state, context, expression_type, expected_type| {
            let _ = unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )
}

struct InferAdoApply {
    statement: lowering::DoStatementId,
    apply_type: TypeId,
    continuation_type: TypeId,
    expression: lowering::ExpressionId,
}

#[tracing::instrument(skip_all, name = "infer_ado_apply")]
fn infer_ado_apply<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: InferAdoApply,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let InferAdoApply { statement, apply_type, continuation_type, expression } = arguments;
    state.with_error_step(ErrorStep::InferringAdoApply(statement), |state| {
        infer_ado_apply_core(state, context, apply_type, continuation_type, expression)
    })
}

fn infer_ado_apply_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    apply_type: TypeId,
    continuation_type: TypeId,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    // expression_type := ?f ?a
    let expression_type = infer_expression(state, context, expression)?;

    // apply_type        := f (a -> b) -> f a -> f b
    // continuation_type := f (a -> b)
    let apply_applied = check_function_application_core(
        state,
        context,
        apply_type,
        continuation_type,
        |state, context, continuation_type, expected_type| {
            let _ = unification::subtype(state, context, continuation_type, expected_type)?;
            Ok(continuation_type)
        },
    )?;

    // apply_applied   := f a -> f b
    // expression_type := f a
    // final_type      := f b
    check_function_application_core(
        state,
        context,
        apply_applied,
        expression_type,
        |state, context, expression_type, expected_type| {
            let _ = unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )
}

struct InferDoBind {
    statement: lowering::DoStatementId,
    bind_type: TypeId,
    now_type: TypeId,
    next_type: TypeId,
    expression: lowering::ExpressionId,
    binder_type: TypeId,
}

#[tracing::instrument(skip_all, name = "infer_do_bind")]
fn infer_do_bind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: InferDoBind,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let InferDoBind { statement, bind_type, now_type, next_type, expression, binder_type } =
        arguments;
    state.with_error_step(ErrorStep::InferringDoBind(statement), move |state| {
        let statement_type =
            infer_do_bind_core(state, context, bind_type, next_type, expression, binder_type)?;
        let _ = unification::subtype(state, context, statement_type, now_type)?;
        Ok(())
    })
}

fn infer_do_bind_core<Q>(
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
    // expression_type := m a
    let expression_type = infer_expression(state, context, expression)?;
    // lambda_type := a -> m b
    let lambda_type = state.make_function(&[binder_type], continuation_type);

    // bind_type       := m a -> (a -> m b) -> m b
    // expression_type := m a
    let bind_applied = check_function_application_core(
        state,
        context,
        bind_type,
        expression_type,
        |state, context, expression_type, expected_type| {
            let _ = unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )?;

    // bind_applied := (a -> m b) -> m b
    // lambda_type  := a -> m b
    // final_type   := m b
    check_function_application_core(
        state,
        context,
        bind_applied,
        lambda_type,
        |state, context, lambda_type, expected_type| {
            let _ = unification::subtype(state, context, lambda_type, expected_type)?;
            Ok(lambda_type)
        },
    )
}

struct InferDoDiscard {
    statement: lowering::DoStatementId,
    discard_type: TypeId,
    now_type: TypeId,
    next_type: TypeId,
    expression: lowering::ExpressionId,
}

#[tracing::instrument(skip_all, name = "infer_do_discard")]
fn infer_do_discard<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: InferDoDiscard,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let InferDoDiscard { statement, discard_type, now_type, next_type, expression } = arguments;
    state.with_error_step(ErrorStep::InferringDoDiscard(statement), move |state| {
        let statement_type =
            infer_do_discard_core(state, context, discard_type, next_type, expression)?;
        let _ = unification::subtype(state, context, statement_type, now_type)?;
        Ok(())
    })
}

fn infer_do_discard_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    discard_type: TypeId,
    continuation_type: TypeId,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    // expression_type := m a
    let expression_type = infer_expression(state, context, expression)?;

    // discard_type    := m a -> (a -> m b) -> m b
    // expression_type := m a
    let discard_applied = check_function_application_core(
        state,
        context,
        discard_type,
        expression_type,
        |state, context, expression_type, expected_type| {
            let _ = unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )?;

    // Unlike `bind`, we have no binder to check the lambda type with.
    // Instead, we extract the result type and use the subtype rule.
    //
    // discard_applied := (a -> m b) -> m b
    // result_type     := m b
    let result_type = check_function_application_core(
        state,
        context,
        discard_applied,
        (),
        |_, _, _, continuation_type| Ok(continuation_type),
    )?;

    let _ = unification::subtype(state, context, continuation_type, result_type)?;

    Ok(continuation_type)
}

/// Looks up the type of a term.
pub fn lookup_file_term<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: files::FileId,
    term_id: indexing::TermItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let term_id = if file_id == context.id {
        if let Some(&k) = state.binding_group.terms.get(&term_id) {
            k
        } else if let Some(&k) = state.checked.terms.get(&term_id) {
            transfer::localize(state, context, k)
        } else {
            context.prim.unknown
        }
    } else {
        let checked = context.queries.checked(file_id)?;
        if let Some(id) = checked.terms.get(&term_id) {
            transfer::localize(state, context, *id)
        } else {
            context.prim.unknown
        }
    };
    Ok(term_id)
}

fn check_function_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    argument: &lowering::ExpressionArgument,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match argument {
        lowering::ExpressionArgument::Type(type_argument) => {
            let Some(type_argument) = type_argument else { return Ok(context.prim.unknown) };
            check_function_type_application(state, context, function_t, *type_argument)
        }
        lowering::ExpressionArgument::Term(term_argument) => {
            let Some(term_argument) = term_argument else { return Ok(context.prim.unknown) };
            check_function_term_application(state, context, function_t, *term_argument)
        }
    }
}

fn check_function_type_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    argument: lowering::TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let function_t = state.normalize_type(function_t);
    match state.storage[function_t] {
        Type::Forall(ref binder, inner) => {
            let binder_level = binder.level;
            let binder_kind = binder.kind;

            let (argument_type, _) =
                kind::check_surface_kind(state, context, argument, binder_kind)?;
            Ok(substitute::SubstituteBound::on(state, binder_level, argument_type, inner))
        }

        _ => Ok(context.prim.unknown),
    }
}

/// Generic function for application checking.
///
/// This function checks that the given `function_t` can be applied to a given
/// `argument_id`. This function is generic over the argument checking rule,
/// which is determined by the `check_argument` callback. You may refer to the
/// implementation commentary for more details.
///
/// This function is used to implement the following functions:
/// - [`check_function_term_application`]
/// - [`check_constructor_binder_application`]
///
/// This function is also used to implement the checking rules for
/// [`lowering::ExpressionKind::Do`] and [`lowering::ExpressionKind::Ado`].
///
/// [`check_constructor_binder_application`]: binder::check_constructor_binder_application
#[tracing::instrument(skip_all, name = "check_function_application")]
pub fn check_function_application_core<Q, A, F>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    argument_id: A,
    check_argument: F,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
    F: FnOnce(&mut CheckState, &CheckContext<Q>, A, TypeId) -> QueryResult<TypeId>,
{
    crate::trace_fields!(state, context, { function = function_t });
    let function_t = state.normalize_type(function_t);
    match state.storage[function_t] {
        // Check that `argument_id :: argument_type`
        Type::Function(argument_type, result_type) => {
            check_argument(state, context, argument_id, argument_type)?;
            Ok(result_type)
        }

        // If `function_t` is a unification variable, we synthesise a function
        // type using more unification variables to make progress on inference.
        //
        // This case allows the type checker to recursively create however many
        // unification variables it needs when checking an application.
        //
        // To visualise, applying a function `?0` against `Int` then `String`:
        //
        // ?0 := ?1 -> ?2
        // ?1 := Int
        //
        // ?2 := ?3 -> ?4
        // ?3 := String
        //
        // Int -> String -> ?4
        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification_type(context);
            let result_u = state.fresh_unification_type(context);
            let function_u = state.storage.intern(Type::Function(argument_u, result_u));

            state.unification.solve(unification_id, function_u);
            check_argument(state, context, argument_id, argument_u)?;

            Ok(result_u)
        }

        // Instantiation rule, like `toolkit::instantiate_forall`
        Type::Forall(ref binder, inner) => {
            let binder_level = binder.level;
            let binder_kind = binder.kind;

            let unification = state.fresh_unification_kinded(binder_kind);
            let function_t =
                substitute::SubstituteBound::on(state, binder_level, unification, inner);
            check_function_application_core(state, context, function_t, argument_id, check_argument)
        }

        // Constraint generation, like `toolkit::collect_constraints`
        Type::Constrained(constraint, constrained) => {
            state.constraints.push_wanted(constraint);
            check_function_application_core(
                state,
                context,
                constrained,
                argument_id,
                check_argument,
            )
        }

        _ => Ok(context.prim.unknown),
    }
}

pub fn check_function_term_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    expression_id: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    check_function_application_core(
        state,
        context,
        function_t,
        expression_id,
        check_expression_argument,
    )
}

pub(crate) fn check_let_chunks<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    chunks: &[lowering::LetBindingChunk],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for chunk in chunks {
        match chunk {
            lowering::LetBindingChunk::Pattern { binder, where_expression } => {
                check_pattern_let_binding(state, context, binder, where_expression)?;
            }
            lowering::LetBindingChunk::Names { bindings, scc } => {
                check_names_chunk(state, context, bindings, scc)?;
            }
        }
    }
    Ok(())
}

fn check_pattern_let_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binder: &Option<lowering::BinderId>,
    where_expression: &Option<lowering::WhereExpression>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(where_expression) = where_expression else {
        return Ok(());
    };

    let expression_type = infer_where_expression(state, context, where_expression)?;

    let Some(binder) = binder else {
        return Ok(());
    };

    let _ = binder::check_binder(state, context, *binder, expression_type)?;

    Ok(())
}

fn check_names_chunk<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &[lowering::LetBindingNameGroupId],
    scc: &[lowering::Scc<lowering::LetBindingNameGroupId>],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for &id in bindings {
        let Some(name) = context.lowered.info.get_let_binding(id) else {
            continue;
        };
        if let Some(signature_id) = name.signature {
            let signature_variables = inspect::collect_signature_variables(context, signature_id);
            state.surface_bindings.insert_let(id, signature_variables);

            let (name_type, _) =
                kind::check_surface_kind(state, context, signature_id, context.prim.t)?;
            state.term_scope.bind_let(id, name_type);
        } else {
            let name_type = state.fresh_unification_type(context);
            state.term_scope.bind_let(id, name_type);
        }
    }

    for item in scc {
        match item {
            lowering::Scc::Base(id) | lowering::Scc::Recursive(id) => {
                check_let_name_binding(state, context, *id)?;
            }
            lowering::Scc::Mutual(mutual) => {
                for id in mutual {
                    check_let_name_binding(state, context, *id)?;
                }
            }
        }
    }

    Ok(())
}

fn check_let_name_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::LetBindingNameGroupId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_local_givens(|state| {
        state.with_error_step(ErrorStep::CheckingLetName(id), |state| {
            check_let_name_binding_core(state, context, id)
        })
    })
}

fn check_let_name_binding_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::LetBindingNameGroupId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(name) = context.lowered.info.get_let_binding(id) else {
        return Ok(());
    };

    let Some(name_type) = state.term_scope.lookup_let(id) else {
        return Ok(());
    };

    if let Some(signature_id) = name.signature {
        let surface_bindings = state.surface_bindings.get_let(id);
        let surface_bindings = surface_bindings.as_deref().unwrap_or_default();

        let signature = inspect::inspect_signature(state, context, name_type, surface_bindings)?;

        equation::check_equations_core(state, context, signature_id, &signature, &name.equations)?;

        let origin = equation::ExhaustivenessOrigin::FromSignature(&signature.arguments);
        equation::patterns(state, context, origin, &name.equations)?;

        if let Some(variable) = signature.variables.first() {
            state.type_scope.unbind(variable.level);
        }
    } else {
        equation::infer_equations_core(state, context, name_type, &name.equations)?;

        let origin = equation::ExhaustivenessOrigin::FromType(name_type);
        equation::patterns(state, context, origin, &name.equations)?;
    };

    // PureScript does not have let generalisation; residuals are moved
    // to the parent scope's wanted constraints. Given constraints must
    // also be preserved across let bindings. This is demonstrated by
    // 274_givens_retained, 275_givens_scoped
    let residual = equation::constraints(state, context, equation::ConstraintsPolicy::Return)?;
    state.constraints.extend_wanted(&residual);

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
                return Ok(context.prim.unknown);
            };
            infer_where_expression(state, context, w)
        }
        lowering::GuardedExpression::Conditionals { pattern_guarded } => {
            let mut inferred_type = context.prim.unknown;
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

    let expression_type = infer_expression(state, context, expression)?;

    let Some(binder) = guard.binder else {
        return Ok(());
    };

    let _ = binder::check_binder(state, context, binder, expression_type)?;

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
    check_let_chunks(state, context, &where_expression.bindings)?;

    let Some(expression) = where_expression.expression else {
        return Ok(context.prim.unknown);
    };

    infer_expression(state, context, expression)
}
