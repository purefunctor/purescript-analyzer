pub mod context;
pub mod core;
pub mod kind;

use std::sync::Arc;

use building_types::{QueryProxy, QueryResult};
use files::FileId;
use indexing::TypeItemId;
use resolving::ResolvedModule;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::core::{
    ForallBinder, ForallBinderId, Role, RowType, RowTypeId, Synonym, SynonymId, Type, TypeId,
};

pub trait ExternalQueries:
    QueryProxy<
        Parsed = parsing::FullParsedModule,
        Stabilized = Arc<stabilizing::StabilizedModule>,
        Indexed = Arc<indexing::IndexedModule>,
        Lowered = Arc<lowering::LoweredModule>,
        Grouped = Arc<lowering::GroupedModule>,
        Resolved = Arc<ResolvedModule>,
        Bracketed = Arc<sugar::Bracketed>,
        Sectioned = Arc<sugar::Sectioned>,
        Checked = Arc<CheckedModule>,
    >
{
    fn intern_type(&self, t: Type) -> TypeId;
    fn lookup_type(&self, id: TypeId) -> Type;

    fn intern_forall_binder(&self, b: ForallBinder) -> ForallBinderId;
    fn lookup_forall_binder(&self, id: ForallBinderId) -> ForallBinder;

    fn intern_row_type(&self, r: RowType) -> RowTypeId;
    fn lookup_row_type(&self, id: RowTypeId) -> RowType;

    fn intern_synonym(&self, s: Synonym) -> SynonymId;
    fn lookup_synonym(&self, id: SynonymId) -> Synonym;

    fn intern_smol_str(&self, s: SmolStr) -> core::SmolStrId;
    fn lookup_smol_str(&self, id: core::SmolStrId) -> SmolStr;
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct CheckedModule {
    pub types: FxHashMap<TypeItemId, TypeId>,
    pub roles: FxHashMap<TypeItemId, Arc<[Role]>>,
}

impl CheckedModule {
    pub fn lookup_type(&self, id: TypeItemId) -> Option<TypeId> {
        self.types.get(&id).copied()
    }

    pub fn lookup_roles(&self, id: TypeItemId) -> Option<Arc<[Role]>> {
        self.roles.get(&id).cloned()
    }
}

pub fn check_module(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let prim_id = queries.prim_id();
    if file_id == prim_id {
        check_prim(queries, prim_id)
    } else {
        Ok(CheckedModule::default())
    }
}

fn check_prim(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let mut checked = CheckedModule::default();
    let resolved = queries.resolved(file_id)?;

    let lookup_type = |name: &str| {
        let prim_type = resolved.exports.lookup_type(name);
        prim_type.unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim"))
    };

    let lookup_class = |name: &str| {
        let prim_class = resolved.exports.lookup_class(name);
        prim_class.unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim"))
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

    let type_to_type = queries.intern_type(Type::Function(type_core, type_core));
    let function_kind = queries.intern_type(Type::Function(type_core, type_to_type));

    let row_type = queries.intern_type(Type::Application(row_core, type_core));
    let record_kind = queries.intern_type(Type::Function(row_type, type_core));

    let mut insert_type = |name: &str, id: TypeId| {
        let (_, item_id) = lookup_type(name);
        checked.types.insert(item_id, id);
    };

    insert_type("Type", type_core);
    insert_type("Function", function_kind);
    insert_type("Array", type_to_type);
    insert_type("Record", record_kind);
    insert_type("Number", type_core);
    insert_type("Int", type_core);
    insert_type("String", type_core);
    insert_type("Char", type_core);
    insert_type("Boolean", type_core);
    insert_type("Constraint", type_core);
    insert_type("Symbol", type_core);
    insert_type("Row", type_to_type);

    let (_, partial_id) = lookup_class("Partial");
    checked.types.insert(partial_id, constraint_core);

    let mut insert_roles = |name: &str, roles: &[Role]| {
        let (_, item_id) = lookup_type(name);
        checked.roles.insert(item_id, Arc::from(roles));
    };

    insert_roles("Type", &[]);
    insert_roles("Function", &[Role::Representational, Role::Representational]);
    insert_roles("Array", &[Role::Representational]);
    insert_roles("Record", &[Role::Representational]);
    insert_roles("Number", &[]);
    insert_roles("Int", &[]);
    insert_roles("String", &[]);
    insert_roles("Char", &[]);
    insert_roles("Boolean", &[]);
    insert_roles("Constraint", &[]);
    insert_roles("Symbol", &[]);
    insert_roles("Row", &[Role::Representational]);

    Ok(checked)
}
