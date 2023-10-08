use syntax::ast;

use crate::{
    id::{AstId, InFile},
    lower, LowerDatabase, ResolverDatabase, ScopeDatabase, Upcast,
};

#[salsa::query_group(InferStorage)]
pub trait InferDatabase:
    LowerDatabase + ResolverDatabase + ScopeDatabase + Upcast<dyn ResolverDatabase>
{
    fn infer_value_declaration(&self, id: InFile<AstId<ast::ValueDeclaration>>) -> ();
}

fn infer_value_declaration(db: &dyn InferDatabase, id: InFile<AstId<ast::ValueDeclaration>>) {
    let lowered = db.lower_value_declaration(id);
    let scope_data = db.value_declaration_scope(id);

    match &lowered.binding {
        lower::Binding::Unconditional { where_expr } => {
            let expr = &lowered.expr_arena[where_expr.expr_id];
            dbg!(scope_data.expr_scope(where_expr.expr_id));
            match expr {
                lower::Expr::LetIn(_, _) => (),
                lower::Expr::Literal(literal) => match literal {
                    lower::Literal::Array(_) => (),
                    lower::Literal::Record(_) => (),
                    lower::Literal::Int(_) => println!("Int!"),
                    lower::Literal::Number(_) => println!("Number!"),
                    lower::Literal::String(_) => println!("String!"),
                    lower::Literal::Char(_) => println!("Char!"),
                    lower::Literal::Boolean(_) => println!("Boolean!"),
                },
                lower::Expr::Variable(qualified) => {
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
                    } else {
                        // dbg!(scope_data.expr_scope(where_expr.expr_id));

                        // let expr_scope = scope_data.expr_scope(where_expr.expr_id);

                        // if scope_data.resolve(expr_scope, &qualified.value) {
                        //     dbg!("Found locally!");
                        //     return;
                        // }

                        for (_, import_declaration) in db.unqualified_imports(id.file_id).iter() {
                            if let Some(values) = db
                                .exports(import_declaration.file_id)
                                .lookup_value(db.upcast(), &qualified.value)
                            {
                                for value_id in values.iter().cloned() {
                                    db.infer_value_declaration(value_id);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
