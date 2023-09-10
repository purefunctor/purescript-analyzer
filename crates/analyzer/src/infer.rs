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
                        let qualified_imports = db.qualified_imports(id.file_id);
                        let module_map = db.module_map();

                        // Step 1.
                        // Given the prefix, figure out the module being imported.
                        let import_id = qualified_imports.import_id(prefix).unwrap();
                        let import_declaration = qualified_imports.import_declaration(import_id);
                        let import_name = &import_declaration.module_name;

                        // Step 2.
                        // Given the module being imported, find its file id.
                        let module_id = module_map.module_id(import_name).unwrap();
                        let file_id = module_map.file_id(module_id).unwrap();

                        // Step 3.
                        // Finally, use the `exports` query to resolve it.
                        let values = db
                            .exports(file_id)
                            .lookup_value(db.upcast(), &qualified.value)
                            .unwrap();

                        for value_id in values.iter().cloned() {
                            db.infer_value_declaration(value_id);
                        }
                    }
                }
            }
        }
    }
}
