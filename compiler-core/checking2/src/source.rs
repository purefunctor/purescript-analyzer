//! Implements syntax-driven checking rules for source files.

pub mod synonym;
pub mod terms;
pub mod types;

use building_types::QueryResult;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{generalise, zonk};
use crate::state::CheckState;

/// Checks all type items in topological order.
pub fn check_type_items<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.type_scc {
        let items = filter_type_items(context, scc);

        for &item in &items {
            check_type_signature(state, context, item)?;
        }

        if scc.is_recursive() {
            prepare_binding_group(state, context, &items);
        }

        for &item in &items {
            check_type_equation(state, context, item)?;
        }

        finalise_binding_group(state, context, &items)?;
    }
    Ok(())
}

fn prepare_binding_group<Q>(state: &mut CheckState, context: &CheckContext<Q>, items: &[TypeItemId])
where
    Q: ExternalQueries,
{
    for &item_id in items {
        if state.checked.types.contains_key(&item_id) {
            continue;
        }
        let kind = state.fresh_unification(context.queries, context.prim.t);
        state.checked.types.insert(item_id, kind);
    }
}

fn finalise_binding_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    items: &[TypeItemId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for &item_id in items {
        let Some(kind) = state.checked.types.get(&item_id).copied() else {
            continue;
        };

        let kind = zonk::Zonk::on(state, context, kind)?;
        let kind = generalise::generalise(state, context, kind)?;
        state.checked.types.insert(item_id, kind);
    }

    Ok(())
}

fn filter_type_items<Q>(
    context: &CheckContext<Q>,
    scc: &lowering::Scc<TypeItemId>,
) -> Vec<TypeItemId>
where
    Q: ExternalQueries,
{
    scc.as_slice()
        .iter()
        .copied()
        .filter(|&item_id| {
            if has_recursive_kind_error(context, item_id) {
                return false;
            }

            // Recursive groups should only contain proper equations.
            !(scc.is_recursive() && is_foreign_item(context, item_id))
        })
        .collect()
}

fn has_recursive_kind_error<Q>(context: &CheckContext<Q>, item_id: TypeItemId) -> bool
where
    Q: ExternalQueries,
{
    context.grouped.cycle_errors.iter().any(|error| {
        if let lowering::LoweringError::RecursiveKinds(recursive) = error {
            recursive.group.contains(&item_id)
        } else {
            false
        }
    })
}

fn is_foreign_item<Q>(context: &CheckContext<Q>, item_id: TypeItemId) -> bool
where
    Q: ExternalQueries,
{
    matches!(
        context.lowered.info.get_type_item(item_id),
        Some(lowering::TypeItemIr::Foreign { .. })
    )
}

fn check_type_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(item) = context.lowered.info.get_type_item(item_id) else {
        return Ok(());
    };

    match item {
        lowering::TypeItemIr::DataGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_data_signature(state, context, item_id, *signature)?;
        }
        lowering::TypeItemIr::NewtypeGroup { .. } => todo!(),
        lowering::TypeItemIr::SynonymGroup { .. } => todo!(),
        lowering::TypeItemIr::ClassGroup { .. } => todo!(),
        lowering::TypeItemIr::Foreign { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_foreign_signature(state, context, item_id, *signature)?;
        }
        lowering::TypeItemIr::Operator { .. } => todo!(),
    }

    Ok(())
}

fn check_data_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let (inferred_type, _) = types::check_kind(state, context, signature, context.prim.t)?;
    let inferred_type = generalise::generalise(state, context, inferred_type)?;
    state.checked.types.insert(item_id, inferred_type);
    Ok(())
}

fn check_foreign_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let (inferred_type, _) = types::check_kind(state, context, signature, context.prim.t)?;
    let inferred_type = generalise::generalise(state, context, inferred_type)?;
    state.checked.types.insert(item_id, inferred_type);
    Ok(())
}

fn check_type_equation<Q>(
    _state: &mut CheckState,
    _context: &CheckContext<Q>,
    _item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    Ok(())
}
