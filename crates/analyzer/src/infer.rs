//! Database for type inference.
mod constraint;
mod lower;
mod system;
mod trees;

use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    SurfaceDatabase,
};

use system::InferValueDeclarationContext;

pub use trees::*;

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: SurfaceDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: Type) -> TypeId;

    #[salsa::invoke(InferValueDeclarationContext::infer_value_declaration_query)]
    fn infer_value_declaration(&self, id: InFile<AstId<ast::ValueDeclaration>>) -> TypeId;

    #[salsa::invoke(InferValueDeclarationContext::infer_value_annotation_declaration_query)]
    fn infer_value_annotation_declaration(
        &self,
        id: InFile<AstId<ast::ValueAnnotationDeclaration>>,
    ) -> TypeId;

    #[salsa::invoke(InferValueDeclarationContext::infer_value_declaration_group_query)]
    fn infer_value_declaration_group(&self, id: InFile<ValueGroupId>) -> TypeId;
}
