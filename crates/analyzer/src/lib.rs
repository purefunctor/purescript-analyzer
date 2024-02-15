//! The core of the analyzer as a library.

pub mod id;
pub mod index;
pub mod infer;
pub mod interner;
pub mod scope;
pub mod source;
pub mod surface;

pub use index::IndexDatabase;
pub use infer::InferenceDatabase;
pub use interner::InternerDatabase;
pub use scope::ScopeDatabase;
pub use source::SourceDatabase;
pub use surface::SurfaceDatabase;

#[salsa::database(
    index::IndexStorage,
    infer::InferenceStorage,
    interner::InternerStorage,
    scope::ScopeStorage,
    source::SourceStorage,
    surface::SurfaceStorage
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
}

impl salsa::Database for RootDatabase {}

impl Default for RootDatabase {
    fn default() -> RootDatabase {
        let mut db = RootDatabase { storage: Default::default() };
        db.set_interner(Default::default());
        db
    }
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use files::{ChangedFile, FileId, Files};
    use salsa::Durability;

    use crate::{
        infer::{pretty_print, Hint},
        InferenceDatabase, RootDatabase, SourceDatabase, SurfaceDatabase,
    };

    fn default_db(source: &str) -> (RootDatabase, Files, FileId) {
        let mut db = RootDatabase::default();
        let mut files = Files::default();

        files.set_file_contents("./Main.purs".into(), Some(source.into()));

        for ChangedFile { file_id, .. } in files.take_changes() {
            let contents = files.file_contents(file_id);
            db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
        }

        db.set_file_paths_with_durability(files.iter().collect(), Durability::HIGH);

        let file_id = files.file_id("./Main.purs".into()).unwrap();

        (db, files, file_id)
    }

    fn add_file(db: &mut RootDatabase, files: &mut Files, path: &str, source: &str) -> FileId {
        files.set_file_contents(path.into(), Some(source.into()));

        for ChangedFile { file_id, .. } in files.take_changes() {
            let contents = files.file_contents(file_id);
            db.set_file_contents(file_id, Arc::from(std::str::from_utf8(contents).unwrap()));
        }

        db.set_file_paths_with_durability(files.iter().collect(), Durability::HIGH);

        files.file_id(path.into()).unwrap()
    }

    #[test]
    fn api_test() {
        let (mut db, mut files, main_file_id) = default_db(
            "module Main where

const a _ = const a
",
        );

        let _ = add_file(
            &mut db,
            &mut files,
            "./Lib.purs",
            "module Lib where

data List a = Cons a (List a) | Nil

a = 0
b = a

f _ = g 0
  where
  g _ = h 0
  h _ = g 0
  [] = []
  [] = []
  i _ = j 0
  j _ = i 0
",
        );

        let node = db.parse_file(main_file_id);
        let (_, arena, source_map) = db.file_surface_map(main_file_id);
        let result = db.file_infer(main_file_id);
        for (b, t) in &result.map.of_binder {
            println!("{:?} : {}", &arena[*b], pretty_print(&db, *t));
        }
        for (e, t) in &result.map.of_expr {
            println!("{:?} : {:?} : {}", *e, &arena[*e], pretty_print(&db, *t));
        }
        // dbg!(&result.errors);
        for error in &result.errors {
            if let Some(Hint::Expression(expr_id)) = error.hints.last() {
                println!("{:?}", &error.kind);
                let ptr = source_map.expr_to_cst.get(expr_id).unwrap();
                println!("{}", ptr.to_node(&node));
            }
        }
    }
}
