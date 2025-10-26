pub mod check;
pub mod core;

pub use core::{Type, TypeId, TypeInterner};

use std::sync::Arc;

use building_types::{QueryProxy, QueryResult};
use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use lowering::{LoweredModule, Scc};
use resolving::ResolvedModule;
use rustc_hash::FxHashMap;

use crate::{
    check::{CheckContext, CheckState, kind, transfer},
    core::{ForallBinder, Variable, debruijn},
};

pub trait ExternalQueries:
    QueryProxy<
        Indexed = Arc<IndexedModule>,
        Lowered = Arc<LoweredModule>,
        Resolved = Arc<ResolvedModule>,
        Checked = Arc<CheckedModule>,
    >
{
    fn intern_type(&self, t: Type) -> TypeId;

    fn lookup_type(&self, id: TypeId) -> Type;
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct CheckedModule {
    terms: FxHashMap<TermItemId, TypeId>,
    types: FxHashMap<TypeItemId, TypeId>,
}

impl CheckedModule {
    pub fn lookup_term(&self, id: TermItemId) -> Option<TypeId> {
        self.terms.get(&id).copied()
    }

    pub fn lookup_type(&self, id: TypeItemId) -> Option<TypeId> {
        self.types.get(&id).copied()
    }
}

pub fn check_module(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let prim_id = queries.prim_id();
    if file_id == prim_id {
        prim_check_module(queries, prim_id)
    } else {
        source_check_module(queries, file_id)
    }
}

fn source_check_module(
    queries: &impl ExternalQueries,
    file_id: FileId,
) -> QueryResult<CheckedModule> {
    let mut state = CheckState::default();
    let context = CheckContext::new(queries, &mut state, file_id)?;

    for scc in &context.lowered.type_scc {
        match scc {
            Scc::Base(id) => {
                check_type_item(&mut state, &context, *id);
            }
            Scc::Recursive(id) => {
                check_type_item(&mut state, &context, *id);
            }
            Scc::Mutual(id) => {
                for id in id {
                    check_type_item(&mut state, &context, *id);
                }
            }
        }
    }

    Ok(state.checked)
}

fn check_type_item<Q>(state: &mut CheckState, context: &CheckContext<Q>, item_id: TypeItemId)
where
    Q: ExternalQueries,
{
    let Some(item) = context.lowered.info.get_type_item(item_id) else { return };
    match item {
        lowering::TypeItemIr::DataGroup { .. } => (),

        lowering::TypeItemIr::NewtypeGroup { .. } => (),

        lowering::TypeItemIr::SynonymGroup { .. } => (),

        lowering::TypeItemIr::ClassGroup { .. } => (),

        lowering::TypeItemIr::Foreign { signature, .. } => {
            let Some(signature_id) = signature else { return };
            let (inferred_type, _) =
                kind::check_surface_kind(state, context, *signature_id, context.prim.t);
            let inferred_type = transfer::globalize(state, context, inferred_type);
            state.checked.types.insert(item_id, inferred_type);
        }

        lowering::TypeItemIr::Operator { .. } => (),
    }
}

fn prim_check_module(
    queries: &impl ExternalQueries,
    file_id: FileId,
) -> QueryResult<CheckedModule> {
    let mut checked_module = CheckedModule::default();
    let resolved = queries.resolved(file_id)?;

    let lookup_type = |name: &str| {
        let prim_type = resolved.exports.lookup_type(name);
        prim_type.unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim"))
    };

    let type_core = {
        let (file_id, item_id) = lookup_type("Type");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let row_core = {
        let (file_id, item_id) = lookup_type("Row");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let constraint_core = {
        let (file_id, item_id) = lookup_type("Constraint");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let type_to_type_core = queries.intern_type(Type::Function(type_core, type_core));
    let function_core = queries.intern_type(Type::Function(type_core, type_to_type_core));

    let row_type_core = queries.intern_type(Type::Application(row_core, type_core));
    let record_core = queries.intern_type(Type::Function(row_type_core, type_core));

    let mut insert_type = |name: &str, id: TypeId| {
        let (_, item_id) = lookup_type(name);
        checked_module.types.insert(item_id, id)
    };

    insert_type("Type", type_core);
    insert_type("Function", function_core);
    insert_type("Array", type_to_type_core);
    insert_type("Record", record_core);
    insert_type("Number", type_core);
    insert_type("Int", type_core);
    insert_type("String", type_core);
    insert_type("Char", type_core);
    insert_type("Boolean", type_core);
    insert_type("Partial", constraint_core);
    insert_type("Constraint", type_core);
    insert_type("Symbol", type_core);
    insert_type("Row", type_to_type_core);

    let proxy_core = {
        let variable = queries.intern_type(Type::Variable(Variable::Bound(debruijn::Index(0))));
        let function = queries.intern_type(Type::Function(variable, type_core));

        let forall = queries.intern_type(Type::Forall(
            ForallBinder {
                visible: false,
                name: "t".into(),
                level: debruijn::Level(1),
                kind: variable,
            },
            function,
        ));

        queries.intern_type(Type::Forall(
            ForallBinder {
                visible: false,
                name: "k".into(),
                level: debruijn::Level(0),
                kind: type_core,
            },
            forall,
        ))
    };

    insert_type("Proxy", proxy_core);

    Ok(checked_module)
}
