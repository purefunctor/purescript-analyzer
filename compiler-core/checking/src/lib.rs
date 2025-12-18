pub mod algorithm;
pub mod error;

pub mod core;
pub use core::{Type, TypeId, TypeInterner};

use std::sync::Arc;

use building_types::{QueryProxy, QueryResult};
use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use lowering::LoweredModule;
use resolving::ResolvedModule;
use rustc_hash::FxHashMap;

use crate::error::CheckError;

pub trait ExternalQueries:
    QueryProxy<
        Indexed = Arc<IndexedModule>,
        Lowered = Arc<LoweredModule>,
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

    pub errors: Vec<CheckError>,
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
}

pub fn check_module(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let prim_id = queries.prim_id();
    if file_id == prim_id {
        algorithm::check_prim(queries, prim_id)
    } else {
        algorithm::check_source(queries, file_id)
    }
}
