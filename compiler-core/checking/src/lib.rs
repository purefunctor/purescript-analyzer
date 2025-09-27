pub mod check;
pub mod core;

use std::{fmt::Debug, sync::Arc};

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, TermItemKind};
use lowering::LoweredModule;

use crate::{
    check::{CheckContext, CheckState, convert},
    core::TypeStorage,
};

pub trait External {
    fn lowered(&self, id: FileId) -> QueryResult<Arc<LoweredModule>>;

    fn prim_id(&self) -> FileId;
}

pub fn check_module<S>(storage: &mut S, indexed: &IndexedModule, lowered: &LoweredModule)
where
    S: TypeStorage + Debug,
{
    let foreign = indexed.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Foreign { .. } = item.kind { Some(id) } else { None }
    });

    let instance = indexed.items.iter_terms().filter_map(|(id, item)| {
        if let TermItemKind::Instance { .. } = item.kind { Some(id) } else { None }
    });

    let mut state = CheckState::new(storage);
    let env = CheckContext::new(indexed, lowered);

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

    dbg!(storage);
}
