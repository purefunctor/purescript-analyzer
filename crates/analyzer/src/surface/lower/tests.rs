//! Snapshot tests for lowering.
//!
//! These tests ensure that CST nodes are lowered correctly as well as
//! consistently, especially for refactorings like [#9].
//!
//! [#9]: https://github.com/purefunctor/purescript-analyzer/issues/9

use std::sync::Arc;

use files::{ChangedFile, FileId, Files};
use itertools::Itertools;
use salsa::Durability;

use crate::{RootDatabase, SourceDatabase, SurfaceDatabase};

use super::{Module, SurfaceArena};

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

fn file_surface(source: &str) -> (Arc<Module>, Arc<SurfaceArena>) {
    let (db, _, file_id) = default_db(source);
    db.file_surface(file_id)
}

#[test]
fn module_none_export_list() {
    let (surface, _) = file_surface(
        "
module Main where    
",
    );

    insta::assert_debug_snapshot!(surface.header.export_list);
}

#[test]
fn module_some_export_list() {
    let (surface, _) = file_surface(
        "
module Main (Type, Data(..), List(Cons, List), value, class Class) where    
",
    );

    insta::assert_debug_snapshot!(surface.header.export_list);
}

#[test]
fn module_imports() {
    let (surface, _) = file_surface(
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

#[test]
fn module_body() {
    let (surface, _) = file_surface(
        "module Main where

valueGroup = 0
class ClassGroup
data DataGroup
",
    );

    insta::assert_debug_snapshot!(surface.body.declarations);
}

#[test]
fn binder_list() {
    let (surface, arena) = file_surface(
        "module Main where

const a b = a
inLet = let const a b = a in const
",
    );

    let value_declarations = surface.body.iter_value_declarations().collect_vec();
    let let_names = arena.iter_let_name().collect_vec();

    insta::assert_debug_snapshot!((value_declarations, let_names));
}
