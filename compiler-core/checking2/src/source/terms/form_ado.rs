use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, unification};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::source::binder;
use crate::source::terms::{application, form_do, form_let};
use crate::state::CheckState;

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

pub fn infer_ado<Q>(
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
                    state.fresh_unification(context.queries, context.prim.t)
                };
                let Some(expression) = *expression else { continue };
                steps.push(AdoStep::Action { statement: statement_id, binder_type, expression });
            }
            lowering::DoStatement::Let { statements } => {
                steps.push(AdoStep::Let { statement: statement_id, statements });
            }
            lowering::DoStatement::Discard { expression } => {
                let binder_type = state.fresh_unification(context.queries, context.prim.t);
                let Some(expression) = *expression else { continue };
                steps.push(AdoStep::Action { statement: statement_id, binder_type, expression });
            }
        }
    }

    let binder_types: Vec<_> = steps
        .iter()
        .filter_map(|step| match step {
            AdoStep::Action { binder_type, .. } => Some(*binder_type),
            AdoStep::Let { .. } => None,
        })
        .collect();

    // For ado blocks with no bindings, we check let statements and then
    // apply pure to the expression.
    //
    //   pure_type  := a -> f a
    //   expression := t
    if binder_types.is_empty() {
        for step in &steps {
            if let AdoStep::Let { statement, statements } = step {
                state.with_error_crumb(ErrorCrumb::CheckingAdoLet(*statement), |state| {
                    form_let::check_let_chunks(state, context, statements)
                })?;
            }
        }
        return if let Some(expression) = expression {
            let pure_type = form_do::lookup_or_synthesise_pure(state, context, pure)?;
            application::check_function_term_application(state, context, pure_type, expression)
        } else {
            state.insert_error(ErrorKind::EmptyAdoBlock);
            Ok(context.unknown("empty ado block"))
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
    let in_expression_type = state.fresh_unification(context.queries, context.prim.t);
    let lambda_type = context.intern_function_chain(&binder_types, in_expression_type);

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

    let map_type = form_do::lookup_or_synthesise_map(state, context, map)?;

    let apply_type = if action_count > 1 {
        form_do::lookup_or_synthesise_apply(state, context, apply)?
    } else {
        context.unknown("unused apply")
    };

    let mut continuation_type = None;

    for step in &steps {
        match step {
            AdoStep::Let { statement, statements } => {
                state.with_error_crumb(ErrorCrumb::CheckingAdoLet(*statement), |state| {
                    form_let::check_let_chunks(state, context, statements)
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
                    state.with_error_crumb(ErrorCrumb::InferringAdoApply(*statement), |state| {
                        infer_ado_apply_core(
                            state,
                            context,
                            apply_type,
                            continuation_type,
                            *expression,
                        )
                    })?
                } else {
                    state.with_error_crumb(ErrorCrumb::InferringAdoMap(*statement), |state| {
                        infer_ado_map_core(state, context, map_type, lambda_type, *expression)
                    })?
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
        super::check_expression(state, context, expression, in_expression_type)?;
    }

    let Some(continuation_type) = continuation_type else {
        unreachable!("invariant violated: impossible empty steps");
    };

    Ok(continuation_type)
}

pub fn infer_ado_map_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    map_type: TypeId,
    lambda_type: TypeId,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let expression_type = super::infer_expression(state, context, expression)?;

    let map_applied = application::check_function_application_core(
        state,
        context,
        map_type,
        lambda_type,
        |state, context, lambda_type, expected_type| {
            unification::subtype(state, context, lambda_type, expected_type)?;
            Ok(lambda_type)
        },
    )?;

    application::check_function_application_core(
        state,
        context,
        map_applied,
        expression_type,
        |state, context, expression_type, expected_type| {
            unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )
}

pub fn infer_ado_apply_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    apply_type: TypeId,
    continuation_type: TypeId,
    expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let expression_type = super::infer_expression(state, context, expression)?;

    let apply_applied = application::check_function_application_core(
        state,
        context,
        apply_type,
        continuation_type,
        |state, context, continuation_type, expected_type| {
            unification::subtype(state, context, continuation_type, expected_type)?;
            Ok(continuation_type)
        },
    )?;

    application::check_function_application_core(
        state,
        context,
        apply_applied,
        expression_type,
        |state, context, expression_type, expected_type| {
            unification::subtype(state, context, expression_type, expected_type)?;
            Ok(expression_type)
        },
    )
}
