//! The core of the analyzer.

pub mod id;
pub mod infer;
pub mod lower;
pub mod names;
pub mod resolver;
pub mod scope;
pub mod source;

use std::hash::BuildHasherDefault;

use indexmap::{IndexMap, IndexSet};
pub use lower::LowerDatabase;
pub use resolver::ResolverDatabase;
use rustc_hash::FxHasher;
pub use scope::ScopeDatabase;
pub use source::SourceDatabase;

/// The analyzer's core database.
#[derive(Default)]
#[salsa::database(
    infer::InferStorage,
    resolver::ResolverStorage,
    lower::LowerStorage,
    scope::ScopeStorage,
    source::SourceStorage
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

pub(crate) type FxIndexSet<T> = IndexSet<T, BuildHasherDefault<FxHasher>>;
pub(crate) type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::{ChangedFile, Files};
    use salsa::Durability;

    use crate::{LowerDatabase, ResolverDatabase, RootDatabase, SourceDatabase};

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

foreign import data Hello :: Function Int Int

hello = 0 1 2
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
        let foreign_hello_id = db.nominal_map(file_id).get_foreign_data("Hello").unwrap();
        let value_hello_id = db.nominal_map(file_id).get_value("hello").unwrap()[0];

        dbg!((foreign_hello_id, value_hello_id));

        dbg!(db.lower_foreign_data(foreign_hello_id));
    }
}
