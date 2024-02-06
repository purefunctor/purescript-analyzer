//! Queries pertaining to type inference.
mod rules;
pub mod tree;

use std::sync::Arc;

use files::FileId;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{id::AstId, ScopeDatabase};

pub use tree::*;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct InferenceResult {
    pub of_constructor: FxHashMap<AstId<ast::DataConstructor>, CoreTypeId>,
}

#[salsa::query_group(InferenceStorage)]
pub trait InferenceDatabase: ScopeDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: CoreType) -> CoreTypeId;

    #[salsa::invoke(rules::file_infer_query)]
    fn file_infer(&self, file_id: FileId) -> Arc<InferenceResult>;
}
