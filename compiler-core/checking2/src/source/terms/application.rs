use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{Type, TypeId, normalise, unification};
use crate::source::types;
use crate::state::CheckState;

pub fn infer_infix_chain<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    head: lowering::ExpressionId,
    tail: &[lowering::InfixPair<lowering::ExpressionId>],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut infix_type = super::infer_expression(state, context, head)?;

    for lowering::InfixPair { tick, element } in tail.iter() {
        let Some(tick) = tick else { return Ok(context.unknown("missing infix tick")) };
        let Some(element) = element else { return Ok(context.unknown("missing infix element")) };

        let tick_type = super::infer_expression(state, context, *tick)?;
        let applied_tick = check_function_application_core(
            state,
            context,
            tick_type,
            infix_type,
            |state, context, infix_type, expected_type| {
                unification::subtype(state, context, infix_type, expected_type)?;
                Ok(infix_type)
            },
        )?;

        infix_type = check_function_term_application(state, context, applied_tick, *element)?;
    }

    Ok(infix_type)
}

/// Generic function for application checking.
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
    let function_t = normalise::expand(state, context, function_t)?;

    match context.lookup_type(function_t) {
        Type::Function(argument_type, result_type) => {
            check_argument(state, context, argument_id, argument_type)?;
            Ok(result_type)
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification(context.queries, context.prim.t);
            let result_u = state.fresh_unification(context.queries, context.prim.t);
            let function_u = context.intern_function(argument_u, result_u);

            unification::solve(state, context, function_t, unification_id, function_u)?;
            check_argument(state, context, argument_id, argument_u)?;

            Ok(result_u)
        }

        Type::Forall(binder_id, inner) => {
            let binder = context.lookup_forall_binder(binder_id);
            let binder_kind = normalise::normalise(state, context, binder.kind)?;

            let replacement = state.fresh_unification(context.queries, binder_kind);
            let function_t = SubstituteName::one(state, context, binder.name, replacement, inner)?;
            check_function_application_core(state, context, function_t, argument_id, check_argument)
        }

        Type::Constrained(constraint, constrained) => {
            state.push_wanted(constraint);
            check_function_application_core(
                state,
                context,
                constrained,
                argument_id,
                check_argument,
            )
        }

        Type::Application(partial, result_type) => {
            let partial = normalise::expand(state, context, partial)?;
            match context.lookup_type(partial) {
                Type::Application(constructor, argument_type) => {
                    let constructor = normalise::expand(state, context, constructor)?;
                    if constructor == context.prim.function {
                        check_argument(state, context, argument_id, argument_type)?;
                        return Ok(result_type);
                    }
                    if let Type::Unification(unification_id) = context.lookup_type(constructor) {
                        unification::solve(
                            state,
                            context,
                            constructor,
                            unification_id,
                            context.prim.function,
                        )?;
                        check_argument(state, context, argument_id, argument_type)?;
                        return Ok(result_type);
                    }
                    Ok(context.unknown("invalid function application"))
                }
                _ => Ok(context.unknown("invalid function application")),
            }
        }

        _ => Ok(context.unknown("invalid function application")),
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
        super::check_expression,
    )
}

pub fn check_function_application<Q>(
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
            let Some(type_argument) = type_argument else {
                return Ok(context.unknown("missing type argument"));
            };
            check_function_type_application(state, context, function_t, *type_argument)
        }
        lowering::ExpressionArgument::Term(term_argument) => {
            let Some(term_argument) = term_argument else {
                return Ok(context.unknown("missing term argument"));
            };
            check_function_term_application(state, context, function_t, *term_argument)
        }
    }
}

pub fn check_function_type_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_t: TypeId,
    argument: lowering::TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let function_t = normalise::expand(state, context, function_t)?;
    match context.lookup_type(function_t) {
        Type::Forall(binder_id, inner) => {
            let binder = context.lookup_forall_binder(binder_id);
            let binder_kind = normalise::normalise(state, context, binder.kind)?;

            let (argument_type, _) = types::check_kind(state, context, argument, binder_kind)?;
            SubstituteName::one(state, context, binder.name, argument_type, inner)
        }
        _ => Ok(context.unknown("invalid type application")),
    }
}
