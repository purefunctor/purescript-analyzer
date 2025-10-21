pub mod check;
pub mod core;

pub use core::{Type, TypeId, TypeInterner};

use std::sync::Arc;

use building_types::{QueryProxy, QueryResult};
use files::FileId;
use indexing::{IndexedModule, TypeItemKind};
use lowering::LoweredModule;
use resolving::ResolvedModule;

use crate::{
    check::{CheckContext, CheckState, kind, unification::UnificationState},
    core::pretty,
};

pub trait ExternalQueries:
    QueryProxy<
        Indexed = Arc<IndexedModule>,
        Lowered = Arc<LoweredModule>,
        Resolved = Arc<ResolvedModule>,
    >
{
    fn intern_type(&self, t: Type) -> TypeId;

    fn lookup_type(&self, id: TypeId) -> Type;
}

pub fn check_module(queries: &impl ExternalQueries, id: FileId) -> QueryResult<()> {
    let mut state = CheckState::default();
    let context = CheckContext::new(queries, &mut state, id)?;

    let foreign = context.indexed.items.iter_types().filter_map(|(id, item)| {
        if let TypeItemKind::Foreign { .. } = item.kind { Some(id) } else { None }
    });

    for id in foreign {
        if let Some(lowering::TypeItemIr::Foreign { signature, .. }) =
            context.lowered.info.get_type_item(id)
        {
            let result = signature.map(|id| kind::infer_surface_kind(&mut state, &context, id));
            if let Some((t, k)) = result {
                let t = pretty::print(&context, &state, t);
                let k = pretty::print(&context, &state, k);
                println!("{t} :: {k}",)
            }
        }
    }

    for (index, entry) in state.unification.iter().enumerate() {
        let domain = entry.domain;
        let kind = state.normalize_type(entry.kind);
        let kind = pretty::print(&context, &state, kind);
        if let UnificationState::Solved(solution) = entry.state {
            let solution = pretty::print(&context, &state, solution);
            eprintln!("?{index}[{domain}] :: {kind} := {solution}");
        } else {
            eprintln!("?{index}[{domain}] :: {kind} := ?");
        };
    }

    Ok(())
}
