pub mod check;
pub mod core;

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, TermItemKind};
use lowering::LoweredModule;
use resolving::ResolvedModule;

use crate::{
    check::{CheckContext, CheckState, PrimCore, convert},
    core::TypeStorage,
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

    let mut state = CheckState::new(storage);
    let prim = PrimCore::collect(external, &mut state)?;
    let env = CheckContext::new(prim, &indexed, &lowered);

    let foreign = indexed.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Foreign { .. } = item.kind { Some(id) } else { None }
    });

    let instance = indexed.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Instance { .. } = item.kind { Some(id) } else { None }
    });

    for id in foreign {
        if let Some(lowering::TermItemIr::Foreign { signature }) = lowered.info.get_term_item(id) {
            let _ = signature.map(|id| convert::type_to_core(&mut state, &env, id));
        }
    }

    for id in instance {
        if let Some(lowering::TermItemIr::Instance { arguments, constraints, members, .. }) =
            lowered.info.get_term_item(id)
        {
            arguments.iter().for_each(|id| {
                convert::type_to_core(&mut state, &env, *id);
            });
            constraints.iter().for_each(|id| {
                convert::type_to_core(&mut state, &env, *id);
            });
            members.iter().for_each(|group| {
                if let Some(id) = group.signature {
                    convert::type_to_core(&mut state, &env, id);
                }
            });
        }
    }

    Ok(())
}
