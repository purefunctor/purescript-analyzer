//! Database for type inference.
mod constraint;
mod lower;
mod rules;
mod trees;

#[cfg(test)]
mod tests;

use std::sync::Arc;

use crate::{
    id::InFile,
    sugar::{BindingGroupId, SugarDatabase},
    ScopeDatabase,
};

pub use trees::*;

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: ScopeDatabase + SugarDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: Type) -> TypeId;

    #[salsa::invoke(rules::infer_binding_group_query)]
    fn infer_binding_group(&self, id: InFile<BindingGroupId>) -> Arc<InferBindingGroup>;
}
