use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, exhaustive, toolkit, unification};
use crate::source::terms::{equations, form_let};
use crate::source::{binder, terms};
use crate::state::CheckState;

pub fn infer_if_then_else<Q>(
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
        super::check_expression(state, context, if_, context.prim.boolean)?;
    }

    let result_type = state.fresh_unification(context.queries, context.prim.t);

    if let Some(then) = then {
        super::check_expression(state, context, then, result_type)?;
    }

    if let Some(else_) = else_ {
        super::check_expression(state, context, else_, result_type)?;
    }

    Ok(result_type)
}

pub fn check_if_then_else<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    if_: Option<lowering::ExpressionId>,
    then: Option<lowering::ExpressionId>,
    else_: Option<lowering::ExpressionId>,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if let Some(if_) = if_ {
        super::check_expression(state, context, if_, context.prim.boolean)?;
    }

    if let Some(then) = then {
        super::check_expression(state, context, then, expected)?;
    }

    if let Some(else_) = else_ {
        super::check_expression(state, context, else_, expected)?;
    }

    Ok(expected)
}

pub fn infer_lambda<Q>(
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
        let argument_type = state.fresh_unification(context.queries, context.prim.t);
        binder::check_binder(state, context, binder_id, argument_type)?;
        argument_types.push(argument_type);
    }

    let result_type = if let Some(body) = expression {
        let body_type = super::infer_expression(state, context, body)?;
        toolkit::instantiate_constrained(state, context, body_type)?
    } else {
        state.fresh_unification(context.queries, context.prim.t)
    };

    let function_type = context.intern_function_list(&argument_types, result_type);

    let exhaustiveness =
        exhaustive::check_lambda_patterns(state, context, &argument_types, binders)?;

    let has_missing = exhaustiveness.missing.is_some();
    state.report_exhaustiveness(context, exhaustiveness);

    if has_missing {
        Ok(context.intern_constrained(context.prim.partial, function_type))
    } else {
        Ok(function_type)
    }
}

pub fn check_lambda<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binders: &[lowering::BinderId],
    expression: Option<lowering::ExpressionId>,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut arguments = vec![];
    let mut remaining = expected;

    for &binder_id in binders.iter() {
        let decomposed = toolkit::decompose_function(state, context, remaining)?;
        if let Some((argument, result)) = decomposed {
            binder::check_binder(state, context, binder_id, argument)?;
            arguments.push(argument);
            remaining = result;
        } else {
            let argument_type = state.fresh_unification(context.queries, context.prim.t);
            binder::check_binder(state, context, binder_id, argument_type)?;
            arguments.push(argument_type);
        }
    }

    let result_type = if let Some(body) = expression {
        super::check_expression(state, context, body, remaining)?
    } else {
        state.fresh_unification(context.queries, context.prim.t)
    };

    let function_type = context.intern_function_list(&arguments, result_type);

    let exhaustiveness = exhaustive::check_lambda_patterns(state, context, &arguments, binders)?;

    let has_missing = exhaustiveness.missing.is_some();
    state.report_exhaustiveness(context, exhaustiveness);

    if has_missing {
        state.push_wanted(context.prim.partial);
    }

    Ok(function_type)
}

pub fn instantiate_trunk_types<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    trunk_types: &mut [TypeId],
    branches: &[lowering::CaseBranch],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for (position, trunk_type) in trunk_types.iter_mut().enumerate() {
        let should_instantiate = branches.iter().any(|branch| {
            let binder = branch.binders.get(position);
            binder.is_some_and(|&binder_id| binder::requires_instantiation(context, binder_id))
        });
        if should_instantiate {
            *trunk_type = toolkit::instantiate_unifications(state, context, *trunk_type)?;
        }
    }
    Ok(())
}

pub fn infer_case_of<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    trunk: &[lowering::ExpressionId],
    branches: &[lowering::CaseBranch],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let result_type = state.fresh_unification(context.queries, context.prim.t);

    let mut trunk_types = vec![];
    for trunk in trunk.iter() {
        let trunk_type = super::infer_expression(state, context, *trunk)?;
        trunk_types.push(trunk_type);
    }

    instantiate_trunk_types(state, context, &mut trunk_types, branches)?;

    for branch in branches.iter() {
        for (binder, trunk) in branch.binders.iter().zip(&trunk_types) {
            binder::check_binder(state, context, *binder, *trunk)?;
        }
        if let Some(guarded) = &branch.guarded_expression {
            let guarded_type = equations::infer_guarded_expression(state, context, guarded)?;
            unification::subtype(state, context, guarded_type, result_type)?;
        }
    }

    let exhaustiveness = exhaustive::check_case_patterns(state, context, &trunk_types, branches)?;

    let has_missing = exhaustiveness.missing.is_some();
    state.report_exhaustiveness(context, exhaustiveness);

    if has_missing {
        Ok(context.intern_constrained(context.prim.partial, result_type))
    } else {
        Ok(result_type)
    }
}

pub fn check_case_of<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    trunk: &[lowering::ExpressionId],
    branches: &[lowering::CaseBranch],
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut trunk_types = vec![];
    for trunk in trunk.iter() {
        let trunk_type = super::infer_expression(state, context, *trunk)?;
        trunk_types.push(trunk_type);
    }

    instantiate_trunk_types(state, context, &mut trunk_types, branches)?;

    for branch in branches.iter() {
        for (binder, trunk) in branch.binders.iter().zip(&trunk_types) {
            binder::check_binder(state, context, *binder, *trunk)?;
        }
        if let Some(guarded) = &branch.guarded_expression {
            equations::check_guarded_expression(state, context, guarded, expected)?;
        }
    }

    let exhaustiveness = exhaustive::check_case_patterns(state, context, &trunk_types, branches)?;

    let has_missing = exhaustiveness.missing.is_some();
    state.report_exhaustiveness(context, exhaustiveness);

    if has_missing {
        state.push_wanted(context.prim.partial);
    }

    Ok(expected)
}

pub fn check_let_in<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &[lowering::LetBindingChunk],
    expression: Option<lowering::ExpressionId>,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    form_let::check_let_chunks(state, context, bindings)?;

    let Some(expression) = expression else {
        return Ok(context.unknown("missing let expression"));
    };

    terms::check_expression(state, context, expression, expected)
}
