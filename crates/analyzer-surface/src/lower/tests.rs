//! Snapshot tests for lowering.
//!
//! These tests ensure that CST nodes are lowered correctly as well as
//! consistently, especially for refactorings like [#9].
//!
//! [#9]: https://github.com/purefunctor/purescript-analyzer/issues/9

use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use analyzer_index::IndexStorage;
use analyzer_interner::{InternerDatabase, InternerStorage};
use analyzer_source::{SourceDatabase, SourceStorage};
use files::{ChangedFile, FileId, Files};
use itertools::Itertools;
use salsa::Durability;

use crate::tree::*;
use crate::{SurfaceDatabase, SurfaceStorage};

#[salsa::database(IndexStorage, InternerStorage, SourceStorage, SurfaceStorage)]
pub struct TestDatabase {
    storage: salsa::Storage<TestDatabase>,
}

impl salsa::Database for TestDatabase {}

impl Default for TestDatabase {
    fn default() -> TestDatabase {
        let mut db = TestDatabase { storage: Default::default() };
        db.set_interner(Default::default());
        db
    }
}

fn default_db(source: &str) -> (TestDatabase, Files, FileId) {
    let mut db = TestDatabase::default();
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

fn file_surface(source: &str) -> (Arc<Module>, Arc<SurfaceArena>) {
    let (db, _, file_id) = default_db(source);
    db.file_surface(file_id)
}

#[test_each::path(glob = "crates/analyzer-surface/src/lower/inputs/*.purs")]
fn test_file(path: PathBuf) {
    let content = fs::read_to_string(&path).unwrap();
    let (surface, arena) = file_surface(&content);

    let name = path.file_name().unwrap().to_str().unwrap();
    let expressions = arena.iter_expr().collect_vec();
    let let_names = arena.iter_let_name().collect_vec();
    let binders = arena.iter_binder().collect_vec();
    let types = arena.iter_ty().collect_vec();

    insta::assert_debug_snapshot!(name, (surface, expressions, let_names, binders, types));
}
