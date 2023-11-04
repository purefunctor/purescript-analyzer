//! Database for local scope information.
mod data;
mod traversals;

use std::sync::Arc;

use syntax::ast;

use crate::{
    id::{AstId, InFile},
    SurfaceDatabase,
};

pub use data::{ScopeData, ScopeId, ScopeKind, ValueDeclarationScope};

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: SurfaceDatabase {
    #[salsa::invoke(value_declaration_scope_query)]
    fn value_declaration_scope(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> Arc<ValueDeclarationScope>;
}

fn value_declaration_scope_query(
    db: &dyn ScopeDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
) -> Arc<ValueDeclarationScope> {
    todo!()
    // let value_declaration = db.surface_value_declaration(id);

    // let mut context =
    //     ScopeCollectorContext::new(&value_declaration.expr_arena, &value_declaration.binder_arena);

    // context.visit_value_declaration(&value_declaration);

    // Arc::new(context.into_value_declaration_scope())
}
