//! Database for type inference.
mod constraint;
mod system;
mod trees;

use syntax::ast;

use crate::{
    id::{AstId, InFile},
    SurfaceDatabase,
};

// pub use system::InferValueDeclarationResult;
use system::InferValueDeclarationContext;

pub use trees::*;

#[salsa::query_group(InferStorage)]
pub trait InferDatabase: SurfaceDatabase {
    #[salsa::interned]
    fn intern_type(&self, t: Type) -> TypeId;

    #[salsa::invoke(InferValueDeclarationContext::infer_value_declaration_query)]
    fn infer_value_declaration(&self, id: InFile<AstId<ast::ValueDeclaration>>) -> ();

    // #[salsa::invoke(infer_data_constructor_query)]
    // fn infer_data_constructor(&self, id: InFile<AstId<ast::DataConstructor>>) -> TypeId;

    // #[salsa::invoke(infer_foreign_data_query)]
    // fn infer_foreign_data_query(&self, id: InFile<AstId<ast::ForeignDataDeclaration>>) -> TypeId;

    // #[salsa::invoke(infer_value_declaration_query)]
    // fn infer_value_declaration(
    //     &self,
    //     id: InFile<AstId<ast::ValueDeclaration>>,
    // ) -> Arc<InferValueDeclarationResult>;

    // #[salsa::invoke(unknown_for_value_declaration_query)]
    // fn unknown_for_value_declaration(&self, id: InFile<AstId<ast::ValueDeclaration>>) -> TypeId;

    // #[salsa::interned]
    // fn intern_type(&self, t: Type) -> TypeId;
}

// fn infer_data_constructor_query(
//     db: &dyn InferDatabase,
//     id: InFile<AstId<ast::DataConstructor>>,
// ) -> TypeId {
//     let data_id = db.nominal_map(id.file_id).data_of_constructor(id);
//     let data_data = db.surface_data(data_id);

//     if let Some(constructor) = data_data.constructors.get(&id) {
//         dbg!(constructor);
//         db.intern_type(Type::NotImplemented)
//     } else {
//         panic!("Invariant violated: constructor does not belong to declaration");
//     }
// }

// fn infer_foreign_data_query(
//     db: &dyn InferDatabase,
//     id: InFile<AstId<ast::ForeignDataDeclaration>>,
// ) -> TypeId {
//     let _ = db.surface_foreign_data(id);
//     db.intern_type(Type::NotImplemented)
// }

// fn infer_value_declaration_query(
//     db: &dyn InferDatabase,
//     id: InFile<AstId<ast::ValueDeclaration>>,
// ) -> Arc<InferValueDeclarationResult> {
//     let value_declaration = db.surface_value_declaration(id);
//     let mut context = system::InferValueDeclarationContext::new(
//         db,
//         id,
//         &value_declaration.expr_arena,
//         &value_declaration.binder_arena,
//     );
//     context.visit_value_declaration(&value_declaration);
//     Arc::new(context.into_result())
// }

// fn unknown_for_value_declaration_query(
//     db: &dyn InferDatabase,
//     id: InFile<AstId<ast::ValueDeclaration>>,
// ) -> TypeId {
//     db.intern_type(Type::Unification(Unification::Global(id)))
// }
