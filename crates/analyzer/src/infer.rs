//! Database for type inference.
mod constraint;
mod context;
mod lower;
mod trees;

use std::sync::Arc;

use crate::{id::InFile, resolver::ValueGroupId, ScopeDatabase};

pub use trees::*;

use self::constraint::Constraint;

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: ScopeDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: Type) -> TypeId;

    #[salsa::invoke(context::infer_value_query)]
    fn infer_value(&self, id: InFile<ValueGroupId>) -> (TypeId, Arc<Vec<Constraint>>);
}
