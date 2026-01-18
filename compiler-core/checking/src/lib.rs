pub mod algorithm;
pub mod error;
pub mod trace;

pub mod core;
pub use core::{Type, TypeId, TypeInterner};

use std::sync::Arc;

use building_types::{QueryProxy, QueryResult};
use files::FileId;
use indexing::{DeriveId, IndexedModule, InstanceId, TermItemId, TypeItemId};
use lowering::{GroupedModule, LoweredModule};
use resolving::ResolvedModule;
use rustc_hash::FxHashMap;
use stabilizing::StabilizedModule;

use crate::error::CheckError;

pub trait ExternalQueries:
    QueryProxy<
        Parsed = parsing::FullParsedModule,
        Stabilized = Arc<StabilizedModule>,
        Indexed = Arc<IndexedModule>,
        Lowered = Arc<LoweredModule>,
        Grouped = Arc<GroupedModule>,
        Resolved = Arc<ResolvedModule>,
        Bracketed = Arc<sugar::Bracketed>,
        Sectioned = Arc<sugar::Sectioned>,
        Checked = Arc<CheckedModule>,
    >
{
    fn intern_type(&self, t: Type) -> TypeId;

    fn lookup_type(&self, id: TypeId) -> Type;
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct CheckedModule {
    pub terms: FxHashMap<TermItemId, TypeId>,
    pub types: FxHashMap<TypeItemId, TypeId>,

    pub operators: FxHashMap<TypeItemId, core::Operator>,
    pub synonyms: FxHashMap<TypeItemId, core::Synonym>,
    pub instances: FxHashMap<InstanceId, core::Instance>,
    pub derived: FxHashMap<DeriveId, core::Instance>,
    pub classes: FxHashMap<TypeItemId, core::Class>,
    pub data: FxHashMap<TypeItemId, core::DataLike>,
    pub roles: FxHashMap<TypeItemId, Arc<[core::Role]>>,

    pub errors: Vec<CheckError>,
    pub custom_messages: Vec<String>,
}

impl CheckedModule {
    pub fn lookup_term(&self, id: TermItemId) -> Option<TypeId> {
        self.terms.get(&id).copied()
    }

    pub fn lookup_type(&self, id: TypeItemId) -> Option<TypeId> {
        self.types.get(&id).copied()
    }

    pub fn lookup_synonym(&self, id: TypeItemId) -> Option<core::Synonym> {
        self.synonyms.get(&id).copied()
    }

    pub fn lookup_class(&self, id: TypeItemId) -> Option<core::Class> {
        self.classes.get(&id).cloned()
    }

    pub fn lookup_data(&self, id: TypeItemId) -> Option<core::DataLike> {
        self.data.get(&id).copied()
    }

    pub fn lookup_roles(&self, id: TypeItemId) -> Option<Arc<[core::Role]>> {
        self.roles.get(&id).cloned()
    }
}

pub fn check_module(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let _span = trace::check_module(queries, file_id)?;
    let prim_id = queries.prim_id();
    if file_id == prim_id {
        algorithm::check_prim(queries, prim_id)
    } else {
        algorithm::check_source(queries, file_id)
    }
}
