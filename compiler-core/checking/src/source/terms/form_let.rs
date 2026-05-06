use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{Type, exhaustive, normalise, toolkit, unification};
use crate::error::ErrorCrumb;
use crate::source::terms::equations;
use crate::source::{binder, types};
use crate::state::CheckState;

pub fn check_let_chunks<Q>(
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

pub fn check_pattern_let_binding<Q>(
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

    let expression_type = equations::infer_where_expression(state, context, where_expression)?;

    let Some(binder) = *binder else {
        return Ok(());
    };

    let expression_type = if binder::requires_instantiation(context, binder) {
        toolkit::instantiate_constrained(state, context, expression_type)?
    } else {
        toolkit::collect_wanteds(state, context, expression_type)?
    };

    let binder_type = binder::check_binder(state, context, binder, expression_type)?;

    let exhaustiveness =
        exhaustive::check_lambda_patterns(state, context, &[binder_type], &[binder])?;

    let has_missing = exhaustiveness.missing.is_some();
    state.report_exhaustiveness(context, exhaustiveness);

    if has_missing {
        state.push_wanted(context.prim.partial);
    }

    Ok(())
}

pub fn check_names_chunk<Q>(
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
            let (name_type, _) = types::check_kind(state, context, signature_id, context.prim.t)?;
            state.checked.nodes.lets.insert(id, name_type);
        } else {
            let name_type = state.fresh_unification(context.queries, context.prim.t);
            state.checked.nodes.lets.insert(id, name_type);
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

pub fn check_let_name_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::LetBindingNameGroupId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_implication(|state| {
        state.with_error_crumb(ErrorCrumb::CheckingLetName(id), |state| {
            check_let_name_binding_core(state, context, id)
        })
    })
}

pub fn check_let_name_binding_core<Q>(
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

    let Some(name_type) = state.checked.nodes.lookup_let(id) else {
        return Ok(());
    };

    if let Some(signature_id) = name.signature {
        let equation_set = equations::analyse_equation_set(
            state,
            context,
            equations::EquationMode::Check {
                origin: equations::EquationTypeOrigin::Explicit(signature_id),
                expected_type: name_type,
            },
            &name.equations,
        )?;

        let exhaustiveness = equations::compute_equation_exhaustiveness(
            state,
            context,
            &equation_set,
            &name.equations,
        )?;
        state.report_exhaustiveness(context, exhaustiveness);
    } else {
        if let [equation] = name.equations.as_ref()
            && equation.binders.is_empty()
            && let Some(guarded) = &equation.guarded
        {
            let inferred_type = equations::infer_guarded_expression(state, context, guarded)?;
            // Keep simple let bindings e.g. `appendLocal = append` polymorphic.
            let name_type = normalise::expand(state, context, name_type)?;
            if let Type::Unification(unification_id) = context.lookup_type(name_type) {
                unification::solve(state, context, name_type, unification_id, inferred_type)?;
            } else {
                unification::subtype(state, context, inferred_type, name_type)?;
            }
        } else {
            let equation_set = equations::analyse_equation_set(
                state,
                context,
                equations::EquationMode::Infer { group_type: name_type },
                &name.equations,
            )?;
            let exhaustiveness = equations::compute_equation_exhaustiveness(
                state,
                context,
                &equation_set,
                &name.equations,
            )?;
            state.report_exhaustiveness(context, exhaustiveness);
        }
    }

    Ok(())
}
