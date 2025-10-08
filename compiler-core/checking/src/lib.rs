pub mod check;
pub mod core;

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, TypeItemKind};
use lowering::LoweredModule;
use resolving::ResolvedModule;

use crate::{
    check::{CheckContext, CheckState, PrimCore, kind},
    core::{TypeStorage, pretty::pretty_print},
};

pub trait External {
    fn indexed(&self, id: FileId) -> QueryResult<Arc<IndexedModule>>;

    fn resolved(&self, id: FileId) -> QueryResult<Arc<ResolvedModule>>;

    fn lowered(&self, id: FileId) -> QueryResult<Arc<LoweredModule>>;

    fn prim_id(&self) -> FileId;
}

pub fn check_module(
    external: &impl External,
    storage: &mut impl TypeStorage,
    id: FileId,
) -> QueryResult<()> {
    let indexed = external.indexed(id)?;
    let lowered = external.lowered(id)?;

    let prim_id = external.prim_id();
    let prim_indexed = external.indexed(prim_id)?;

    let mut state = CheckState::new(storage);

    let prim = PrimCore::collect(external, &mut state)?;
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
                    pretty_print(external, &state, t),
                    pretty_print(external, &state, k)
                )
            }
        }
    }

    Ok(())
}
