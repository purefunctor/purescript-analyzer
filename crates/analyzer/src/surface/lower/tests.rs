//! Snapshot tests for lowering.
//! 
//! These tests ensure that CST nodes are lowered correctly as well as
//! consistently, especially for refactorings like [#9].
//! 
//! [#9]: https://github.com/purefunctor/purescript-analyzer/issues/9

use std::sync::Arc;

use files::{ChangedFile, FileId, Files};
use salsa::Durability;

use crate::{RootDatabase, SourceDatabase, SurfaceDatabase};

use super::Module;

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

fn file_surface(source: &str) -> Arc<Module> {
    let (db, _, file_id) = default_db(source);
    let (surface, _) = db.file_surface(file_id);
    surface
}

#[test]
fn module_imports() {
    let surface = file_surface(
        "module Main where
    
import Lib
import Lib as Qualified
import Lib (Type, Data(..), List(Cons, List), value, class Class)
import Lib (Type, Data(..), List(Cons, List), value, class Class) as Qualified
import Lib hiding (Type, Data(..), List(Cons, List), value, class Class)
import Lib hiding (Type, Data(..), List(Cons, List), value, class Class) as Qualified",
    );

    insta::assert_debug_snapshot!(surface.imports);
}
