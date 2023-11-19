//! Database for type inference.
mod constraint;
mod lower;
mod system;
mod trees;

use crate::{id::InFile, resolver::ValueGroupId, ScopeDatabase};

use system::InferContext;
pub use trees::*;

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: ScopeDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: Type) -> TypeId;

    #[salsa::invoke(InferContext::infer_value_query)]
    fn infer_value(&self, id: InFile<ValueGroupId>) -> TypeId;
}
