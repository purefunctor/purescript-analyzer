//! Database for lowering the CST.

mod source_map;
pub mod surface;

use std::sync::Arc;

use syntax::ast;

pub use source_map::*;
pub use surface::*;

use crate::{
    id::{AstId, InFile},
    ResolverDatabase, SourceDatabase,
};

#[salsa::query_group(LowerStorage)]
pub trait LowerDatabase: SourceDatabase + ResolverDatabase {
    fn lower_value_declaration(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> Arc<ValueDeclarationData>;

    fn lower_value_declaration_with_source_map(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> (Arc<ValueDeclarationData>, Arc<SourceMap>);
}

fn lower_value_declaration(
    db: &dyn LowerDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
) -> Arc<ValueDeclarationData> {
    db.lower_value_declaration_with_source_map(id).0
}

fn lower_value_declaration_with_source_map(
    _db: &dyn LowerDatabase,
    _id: InFile<AstId<ast::ValueDeclaration>>,
) -> (Arc<ValueDeclarationData>, Arc<SourceMap>) {
    todo!()
}
