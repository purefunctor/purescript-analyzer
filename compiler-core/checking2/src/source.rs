//! Implements syntax-driven checking rules for source files.

pub mod synonym;
pub mod terms;
pub mod types;

use building_types::QueryResult;
use indexing::TypeItemId;
use lowering::Scc;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::generalise;
use crate::state::CheckState;

/// Checks all type items in topological order.
pub fn check_type_items<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.type_scc {
        match scc {
            Scc::Base(id) => {
                check_type_signature(state, context, *id)?;
                check_type_equation(state, context, *id)?;
            }
            Scc::Recursive(id) => {
                check_type_signature(state, context, *id)?;
                check_type_equation(state, context, *id)?;
            }
            Scc::Mutual(mutual) => {
                for id in mutual {
                    check_type_signature(state, context, *id)?;
                }
                for &id in mutual {
                    check_type_equation(state, context, id)?;
                }
            }
        }
    }
    Ok(())
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
