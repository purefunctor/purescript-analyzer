//! The core of the analyzer.

pub mod id;
pub mod infer;
pub mod names;
pub mod resolver;
pub mod scope;
pub mod source;
pub mod surface;

pub use infer::InferDatabase;
pub use resolver::ResolverDatabase;
pub use scope::ScopeDatabase;
pub use source::SourceDatabase;
pub use surface::SurfaceDatabase;

/// The analyzer's core database.
#[derive(Default)]
#[salsa::database(
    infer::InferStorage,
    resolver::ResolverStorage,
    scope::ScopeStorage,
    source::SourceStorage,
    surface::SurfaceStorage
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

impl Upcast<dyn ResolverDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn ResolverDatabase + 'static) {
        self
    }
}

impl Upcast<dyn InferDatabase> for RootDatabase {
    fn upcast(&self) -> &(dyn InferDatabase + 'static) {
        self
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::{ChangedFile, Files};
    use salsa::Durability;

    use crate::{ResolverDatabase, RootDatabase, ScopeDatabase, SourceDatabase, SurfaceDatabase};

    #[test]
    fn api() {
        // Initialization Code
        let mut db = RootDatabase::default();
        let mut files = Files::default();

        // Given the source file glob, we take all purs files and load them onto the file system.
        files.set_file_contents(
            "./Main.purs".into(),
            Some(
                "
module Main where

x :: Int -> Int -> Int
x _ = 't'
  where
  f :: Int -> Int
  f _ = 0
  
  g :: Int -> Int
  g _ = 1
"
                .into(),
            ),
        );
        // Then, we feed it to the database through the `take_changes` method.
        for ChangedFile { file_id, .. } in files.take_changes() {
            let contents = files.file_contents(file_id);
            db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
        }
        // Finally, we provide the file paths to the database for use by queries like `module_map`.
        // Note that we're setting the durability to medium as we don't expect file paths to change
        // as often as something like editing a file would with the file contents.
        db.set_file_paths_with_durability(files.iter().collect(), Durability::MEDIUM);

        let file_id = files.file_id("./Main.purs".into()).unwrap();
        let nominal_map = db.nominal_map(file_id);
        let x_group_id = nominal_map.value_group_id("x").unwrap();
        db.value_surface(x_group_id);

        // let pp = infer::PrettyPrinter::new(db.upcast());
        // println!("\nx :: {}\n", pp.ty(db.infer_value(x_group_id)).pretty(80));

        let root = db.parse_file(file_id);

        dbg!(db.value_scope(x_group_id));
        // dbg!(&db.value_surface_with_source_map(x_group_id).1.expr_to_cst.iter().next().unwrap().1.to_node(&root));
        for (expr, cst) in db.value_surface_with_source_map(x_group_id).1.expr_to_cst.iter() {
            println!("{} - {:#?}", expr.into_raw(), cst.to_node(&root).text());
        }
    }
}
