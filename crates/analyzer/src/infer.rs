//! Database for type inference.
mod constraint;
mod context;
mod lower;
mod trees;

#[cfg(test)]
mod tests;

use std::sync::Arc;

use crate::{id::InFile, resolver::ValueGroupId, sugar::SugarDatabase, ScopeDatabase};

pub use trees::*;

use self::constraint::Constraint;

#[derive(Debug, PartialEq, Eq)]
pub enum InferResult {
    Complete(TypeId),
    Incomplete(TypeId, Vec<Constraint>),
    Recursive,
}

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: ScopeDatabase + SugarDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: Type) -> TypeId;

    #[salsa::invoke(context::infer_value_query)]
    #[salsa::cycle(context::infer_value_query_recover)]
    fn infer_value(&self, id: InFile<ValueGroupId>) -> Arc<InferResult>;
}
