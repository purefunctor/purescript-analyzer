//! Database for type inference.
mod constraint;
mod lower;
mod system;
mod trees;

use crate::{id::InFile, resolver::ValueGroupId, SurfaceDatabase};

use system::InferValueDeclarationContext;

pub use trees::*;

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: SurfaceDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: Type) -> TypeId;

    #[salsa::invoke(InferValueDeclarationContext::infer_value_declaration_group_query)]
    fn infer_value_declaration_group(&self, id: InFile<ValueGroupId>) -> TypeId;
}
