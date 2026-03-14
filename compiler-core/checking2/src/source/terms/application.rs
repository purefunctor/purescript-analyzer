use std::mem;
use std::ops::ControlFlow;

use building_types::QueryResult;

use crate::context::CheckContext;
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::{ForallBinder, Type, TypeId, normalise, signature, unification};
use crate::error::ErrorKind;
use crate::source::types;
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

pub struct ApplicationAnalysis {
    pub constraints: Vec<TypeId>,
    pub argument: TypeId,
    pub result: TypeId,
}

pub struct GenericApplication {
    pub argument: TypeId,
    pub result: TypeId,
}

fn analyse_function_application_step<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: TypeId,
    constraints: &mut Vec<TypeId>,
) -> QueryResult<ControlFlow<Option<ApplicationAnalysis>, TypeId>>
where
    Q: ExternalQueries,
{
    match context.lookup_type(function) {
        Type::Function(argument, result) => {
            let analysis =
                ApplicationAnalysis { constraints: mem::take(constraints), argument, result };
            Ok(ControlFlow::Break(Some(analysis)))
        }

        Type::Unification(unification_id) => {
            let argument = state.fresh_unification(context.queries, context.prim.t);
            let result = state.fresh_unification(context.queries, context.prim.t);
            let function = context.intern_function(argument, result);

            unification::solve(state, context, function, unification_id, function)?;

            let analysis =
                ApplicationAnalysis { constraints: mem::take(constraints), argument, result };

            Ok(ControlFlow::Break(Some(analysis)))
        }

        Type::Forall(binder_id, inner) => {
            let binder = context.lookup_forall_binder(binder_id);
            let binder_kind = normalise::expand(state, context, binder.kind)?;

            let replacement = state.fresh_unification(context.queries, binder_kind);
            let function = SubstituteName::one(state, context, binder.name, replacement, inner)?;
            Ok(ControlFlow::Continue(function))
        }

        Type::Constrained(constraint, constrained) => {
            constraints.push(constraint);
            Ok(ControlFlow::Continue(constrained))
        }

        Type::Application(function_argument, result) => {
            let function_argument = normalise::expand(state, context, function_argument)?;

            let Type::Application(constructor, argument) = context.lookup_type(function_argument)
            else {
                return Ok(ControlFlow::Break(None));
            };

            let constructor = normalise::expand(state, context, constructor)?;
            if constructor == context.prim.function {
                let analysis =
                    ApplicationAnalysis { constraints: mem::take(constraints), argument, result };
                return Ok(ControlFlow::Break(Some(analysis)));
            }

            if let Type::Unification(unification_id) = context.lookup_type(constructor) {
                unification::solve(
                    state,
                    context,
                    constructor,
                    unification_id,
                    context.prim.function,
                )?;

                let analysis =
                    ApplicationAnalysis { constraints: mem::take(constraints), argument, result };

                return Ok(ControlFlow::Break(Some(analysis)));
            }

            Ok(ControlFlow::Break(None))
        }

        _ => Ok(ControlFlow::Break(None)),
    }
}

pub fn analyse_function_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut function: TypeId,
) -> QueryResult<Option<ApplicationAnalysis>>
where
    Q: ExternalQueries,
{
    let mut constraints = vec![];
    safe_loop! {
        function = normalise::expand(state, context, function)?;
        match analyse_function_application_step(state, context, function, &mut constraints)? {
            ControlFlow::Continue(next) => function = next,
            ControlFlow::Break(analysis) => return Ok(analysis),
        };
    }
}

pub fn check_generic_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: TypeId,
) -> QueryResult<Option<GenericApplication>>
where
    Q: ExternalQueries,
{
    let Some(ApplicationAnalysis { constraints, argument, result }) =
        analyse_function_application(state, context, function)?
    else {
        return Ok(None);
    };

    for constraint in constraints {
        state.push_wanted(constraint);
    }

    Ok(Some(GenericApplication { argument, result }))
}

pub fn check_function_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_type: TypeId,
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
            check_function_type_application(state, context, function_type, *type_argument)
        }
        lowering::ExpressionArgument::Term(term_argument) => {
            let Some(term_argument) = term_argument else {
                return Ok(context.unknown("missing term argument"));
            };
            check_function_term_application(state, context, function_type, *term_argument)
        }
    }
}

pub fn check_function_term_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: TypeId,
    expression_id: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let Some(GenericApplication { argument, result }) =
        check_generic_application(state, context, function)?
    else {
        return Ok(context.unknown("invalid function application"));
    };
    super::check_expression(state, context, expression_id, argument)?;
    Ok(result)
}

pub fn check_function_type_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: TypeId,
    argument: lowering::TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let signature::DecomposedSignature { binders, constraints, arguments, result } =
        signature::decompose_signature(
            state,
            context,
            function,
            signature::DecomposeSignatureMode::Full,
        )?;

    let Some(index) = binders.iter().position(|binder| binder.visible) else {
        let function_type = state.pretty_id(context, function)?;
        state.insert_error(ErrorKind::NoVisibleTypeVariable { function_type });
        return Ok(context.unknown("invalid visible type application"));
    };

    let mut substitution = NameToType::default();

    for binder in binders.iter().take(index) {
        let binder_kind = SubstituteName::many(state, context, &substitution, binder.kind)?;
        let binder_kind = normalise::expand(state, context, binder_kind)?;
        let replacement = state.fresh_unification(context.queries, binder_kind);
        substitution.insert(binder.name, replacement);
    }

    let ForallBinder { name: visible_name, kind: visible_kind, .. } = binders[index];

    let visible_kind = SubstituteName::many(state, context, &substitution, visible_kind)?;
    let visible_kind = normalise::expand(state, context, visible_kind)?;

    let (argument_type, _) = types::check_kind(state, context, argument, visible_kind)?;
    substitution.insert(visible_name, argument_type);

    let binders = binders.iter().skip(index + 1).copied();

    let function = context.intern_function_chain(&arguments, result);
    let constrained = context.intern_constrained_list(&constraints, function);
    let quantified = context.intern_forall_iter(binders, constrained);

    SubstituteName::many(state, context, &substitution, quantified)
}

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
        let Some(GenericApplication { argument, result }) =
            check_generic_application(state, context, tick_type)?
        else {
            return Ok(context.unknown("invalid function application"));
        };
        unification::subtype(state, context, infix_type, argument)?;
        let applied_tick = result;

        infix_type = check_function_term_application(state, context, applied_tick, *element)?;
    }

    Ok(infix_type)
}
