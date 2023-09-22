//! The core of the analyzer.

pub mod id;
pub mod infer;
pub mod lower;
pub mod names;
pub mod resolver;
pub mod scope;
pub mod source;

pub use lower::LowerDatabase;
pub use resolver::ResolverDatabase;
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
        &*self
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::{ChangedFile, Files};
    use salsa::Durability;

    use crate::{ResolverDatabase, RootDatabase, ScopeDatabase, SourceDatabase, LowerDatabase};

    #[test]
    fn api() {
        // Initialization Code
        let mut db = RootDatabase::default();
        let mut files = Files::default();

        // Given the source file glob, we take all purs files and load them onto the file system.
        // files.set_file_contents(
        //     "./Main.purs".into(),
        //     Some("module Main where\n\nimport Hello\n\nmain = hello".into()),
        // );
        // files.set_file_contents(
        //     "./Hello.purs".into(),
        //     Some("module Hello (hello) where\n\nhello = 0".into()),
        // );
        files.set_file_contents(
            "./Main.purs".into(),
            Some(
                "
module Main where

hello =
  let
    a = 0
    b = 0
  in
    a
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
        let hello_id = db.nominal_map(file_id).get_value("hello").unwrap()[0];
        dbg!(&db.lower_value_declaration(hello_id).expr_arena);
        dbg!(db.value_declaration_scope(hello_id));
    }
}
