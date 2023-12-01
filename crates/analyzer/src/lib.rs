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

    use crate::{infer, InferDatabase, ResolverDatabase, RootDatabase, SourceDatabase};

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

const = let a = 0 in a
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
        let const_group_id = nominal_map.value_group_id("const").unwrap();

        // db.value_recursive_lets(const_group_id);

        // dbg!(db.value_resolved(const_group_id));

        let pp = infer::PrettyPrinter::new(&db);
        let (ty, _) = db.infer_value(const_group_id);
        println!("\nconst :: {}\n", pp.ty(ty).pretty(80));
    }
}
