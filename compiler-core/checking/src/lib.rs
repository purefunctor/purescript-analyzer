pub mod check;
pub mod core;

use std::sync::Arc;

use building_types::{QueryProxy, QueryResult};
use files::FileId;
use indexing::{IndexedModule, TypeItemKind};
use lowering::LoweredModule;
use resolving::ResolvedModule;

use crate::{
    check::{CheckContext, CheckState, PrimCore, kind},
    core::{TypeStorage, pretty::print},
};

pub trait ExternalQueries:
    QueryProxy<
        Indexed = Arc<IndexedModule>,
        Lowered = Arc<LoweredModule>,
        Resolved = Arc<ResolvedModule>,
    >
{
}

pub fn check_module(
    queries: &impl ExternalQueries,
    storage: &mut impl TypeStorage,
    id: FileId,
) -> QueryResult<()> {
    let indexed = queries.indexed(id)?;
    let lowered = queries.lowered(id)?;

    let prim_id = queries.prim_id();
    let prim_indexed = queries.indexed(prim_id)?;

    let mut state = CheckState::new(storage);

    let prim = PrimCore::collect(queries, &mut state)?;
    let context = CheckContext::new(prim, &indexed, &lowered, &prim_indexed);

    let foreign = indexed.items.iter_types().filter_map(|(id, item)| {
        if let TypeItemKind::Foreign { .. } = item.kind { Some(id) } else { None }
    });

    for id in foreign {
        if let Some(lowering::TypeItemIr::Foreign { signature, .. }) =
            lowered.info.get_type_item(id)
        {
            let result = signature.map(|id| kind::infer_surface_kind(&mut state, &context, id));
            if let Some((t, k)) = result {
                println!(
                    "{} :: {}",
                    print(queries, &state, t),
                    print(queries, &state, k)
                )
            }
        }
    }

    Ok(())
}
