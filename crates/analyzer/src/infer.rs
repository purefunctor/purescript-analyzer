mod traversals;

use syntax::ast;

use crate::{
    id::{AstId, InFile},
    lower::visitor::Visitor,
    LowerDatabase, ResolverDatabase, ScopeDatabase, Upcast,
};

use self::traversals::InferValueDeclarationContext;

#[salsa::query_group(InferStorage)]
pub trait InferDatabase:
    LowerDatabase + ResolverDatabase + ScopeDatabase + Upcast<dyn ResolverDatabase>
{
    #[salsa::invoke(infer_value_declaration_query)]
    fn infer_value_declaration(&self, id: InFile<AstId<ast::ValueDeclaration>>) -> ();
}

fn infer_value_declaration_query(db: &dyn InferDatabase, id: InFile<AstId<ast::ValueDeclaration>>) {
    let value_declaration_data = db.lower_value_declaration(id);
    let value_declaration_scope = db.value_declaration_scope(id);

    let mut context =
        InferValueDeclarationContext::new(&value_declaration_data, &value_declaration_scope);
    context.visit_value_declaration(&value_declaration_data);

    dbg!(context.type_per_expr);
}
