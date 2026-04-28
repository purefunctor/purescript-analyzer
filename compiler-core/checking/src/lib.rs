pub mod context;
pub mod core;
pub mod error;
pub mod implication;
pub mod safety;
pub mod source;
pub mod state;

pub mod interners;
pub use interners::CoreInterners;

use std::sync::Arc;

use building_types::{QueryProxy, QueryResult};
use files::FileId;
use indexing::{DeriveId, InstanceId, TermItemId, TypeItemId};
use resolving::ResolvedModule;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::core::{
    CheckedClass, CheckedInstance, CheckedSynonym, ForallBinder, ForallBinderId, Name, Role,
    RowType, RowTypeId, SmolStrId, Type, TypeId,
};
use crate::error::CheckError;

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
    >
{
    fn checked(&self, id: FileId) -> QueryResult<Arc<CheckedModule>>;

    fn intern_type(&self, t: Type) -> TypeId;
    fn lookup_type(&self, id: TypeId) -> Type;

    fn intern_forall_binder(&self, b: ForallBinder) -> ForallBinderId;
    fn lookup_forall_binder(&self, id: ForallBinderId) -> ForallBinder;

    fn intern_row_type(&self, r: RowType) -> RowTypeId;
    fn lookup_row_type(&self, id: RowTypeId) -> RowType;

    fn intern_smol_str(&self, s: SmolStr) -> core::SmolStrId;
    fn lookup_smol_str(&self, id: core::SmolStrId) -> SmolStr;
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct CheckedModule {
    pub types: FxHashMap<TypeItemId, TypeId>,
    pub terms: FxHashMap<TermItemId, TypeId>,
    pub synonyms: FxHashMap<TypeItemId, CheckedSynonym>,
    pub classes: FxHashMap<TypeItemId, CheckedClass>,
    pub instances: FxHashMap<InstanceId, CheckedInstance>,
    pub derived: FxHashMap<DeriveId, CheckedInstance>,
    pub roles: FxHashMap<TypeItemId, Arc<[Role]>>,
    pub nodes: CheckedNodes,
    pub errors: Vec<CheckError>,
    pub names: FxHashMap<Name, SmolStrId>,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct CheckedNodes {
    pub types: FxHashMap<lowering::TypeId, TypeId>,
    pub expressions: FxHashMap<lowering::ExpressionId, TypeId>,
    pub binders: FxHashMap<lowering::BinderId, TypeId>,
    pub lets: FxHashMap<lowering::LetBindingNameGroupId, TypeId>,
    pub puns: FxHashMap<lowering::RecordPunId, TypeId>,
    pub sections: FxHashMap<lowering::ExpressionId, TypeId>,
    pub term_operator: FxHashMap<lowering::TermOperatorId, OperatorBranchTypes>,
    pub type_operator: FxHashMap<lowering::TypeOperatorId, OperatorBranchTypes>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OperatorBranchTypes {
    pub left: TypeId,
    pub right: TypeId,
    pub result: TypeId,
}

impl CheckedModule {
    pub fn lookup_type(&self, id: TypeItemId) -> Option<TypeId> {
        self.types.get(&id).copied()
    }

    pub fn lookup_term(&self, id: TermItemId) -> Option<TypeId> {
        self.terms.get(&id).copied()
    }

    pub fn lookup_synonym(&self, id: TypeItemId) -> Option<CheckedSynonym> {
        self.synonyms.get(&id).cloned()
    }

    pub fn lookup_class(&self, id: TypeItemId) -> Option<CheckedClass> {
        self.classes.get(&id).cloned()
    }

    pub fn lookup_instance(&self, id: InstanceId) -> Option<CheckedInstance> {
        self.instances.get(&id).cloned()
    }

    pub fn lookup_derived(&self, id: DeriveId) -> Option<CheckedInstance> {
        self.derived.get(&id).cloned()
    }

    pub fn lookup_roles(&self, id: TypeItemId) -> Option<Arc<[Role]>> {
        self.roles.get(&id).cloned()
    }

    pub fn lookup_name(&self, name: Name) -> Option<SmolStrId> {
        self.names.get(&name).copied()
    }
}

impl CheckedNodes {
    pub fn lookup_expression(&self, id: lowering::ExpressionId) -> Option<TypeId> {
        self.expressions.get(&id).copied()
    }

    pub fn lookup_type(&self, id: lowering::TypeId) -> Option<TypeId> {
        self.types.get(&id).copied()
    }

    pub fn lookup_binder(&self, id: lowering::BinderId) -> Option<TypeId> {
        self.binders.get(&id).copied()
    }

    pub fn lookup_let(&self, id: lowering::LetBindingNameGroupId) -> Option<TypeId> {
        self.lets.get(&id).copied()
    }

    pub fn lookup_pun(&self, id: lowering::RecordPunId) -> Option<TypeId> {
        self.puns.get(&id).copied()
    }

    pub fn lookup_section(&self, id: lowering::ExpressionId) -> Option<TypeId> {
        self.sections.get(&id).copied()
    }

    pub fn lookup_type_operator(
        &self,
        id: lowering::TypeOperatorId,
    ) -> Option<OperatorBranchTypes> {
        self.type_operator.get(&id).copied()
    }

    pub fn lookup_term_operator(
        &self,
        id: lowering::TermOperatorId,
    ) -> Option<OperatorBranchTypes> {
        self.term_operator.get(&id).copied()
    }
}

pub fn check_module(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let prim_id = queries.prim_id();
    if file_id == prim_id { check_prim(queries, prim_id) } else { check_source(queries, file_id) }
}

fn check_source(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let mut state = state::CheckState::new(file_id);
    let context = context::CheckContext::new(queries, file_id)?;

    source::check_type_items(&mut state, &context)?;
    source::check_term_items(&mut state, &context)?;
    core::zonk::zonk_nodes(&mut state, &context)?;

    Ok(state.checked)
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
