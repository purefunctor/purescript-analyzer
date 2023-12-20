//! Database for type inference.
mod constraint;
mod rules;
mod trees;

#[cfg(test)]
mod tests;

use std::sync::Arc;

use crate::{
    id::InFile,
    resolver::DataGroupId,
    sugar::{BindingGroupId, SugarDatabase},
    ScopeDatabase,
};

pub use rules::{BindingGroupTypes, DataGroupTypes, ValueGroupTypes};
pub use trees::*;

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: ScopeDatabase + SugarDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: Type) -> TypeId;

    #[salsa::invoke(rules::infer_binding_group_query)]
    fn infer_binding_group(&self, id: InFile<BindingGroupId>) -> Arc<BindingGroupTypes>;

    #[salsa::invoke(rules::infer_data_group_query)]
    fn infer_data_group(&self, id: InFile<DataGroupId>) -> Arc<DataGroupTypes>;
}
