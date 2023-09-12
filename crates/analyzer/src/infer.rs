use syntax::ast;

use crate::{
    id::{AstId, InFile},
    lower, LowerDatabase, ResolverDatabase, Upcast,
};

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: LowerDatabase + ResolverDatabase + Upcast<dyn ResolverDatabase> {
    fn infer_value_declaration(&self, id: InFile<AstId<ast::ValueDeclaration>>) -> ();
}

fn infer_value_declaration(db: &dyn InferDatabase, id: InFile<AstId<ast::ValueDeclaration>>) {
    let lowered = db.lower_value_declaration(id);
    match &lowered.binding {
        lower::Binding::Unconditional { where_expr } => {
            let expr = &lowered.expr_arena[where_expr.expr_id];
            match expr {
                lower::Expr::Lit(literal) => match literal {
                    lower::Lit::Array(_) => (),
                    lower::Lit::Record(_) => (),
                    lower::Lit::Int(_) => println!("Int!"),
                    lower::Lit::Number(_) => println!("Number!"),
                    lower::Lit::String(_) => println!("String!"),
                    lower::Lit::Char(_) => println!("Char!"),
                    lower::Lit::Boolean(_) => println!("Boolean!"),
                },
                lower::Expr::Var(qualified) => {
                    if let Some(prefix) = &qualified.prefix {
                        if let Some(values) = db
                            .qualified_imports(id.file_id)
                            .import_id(prefix)
                            .and_then(|import_id| {
                                let import_declaration =
                                    db.qualified_imports(id.file_id).import_declaration(import_id);
                                db.exports(import_declaration.file_id)
                                    .lookup_value(db.upcast(), &qualified.value)
                            })
                        {
                            for value_id in values.iter().cloned() {
                                db.infer_value_declaration(value_id);
                            }
                        } else {
                            println!("Nothing!")
                        }
                    }
                }
            }
        }
    }
}
