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

use super::{Binding, Module, SurfaceArena};

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
fn expressions() {
    let (surface, arena) = file_surface(
        "module Main where

application = function argument
constructor = Constructor
prefixedConstructor = A.Constructor
lambda = \\a b -> a
letIn = let const a b = a in const
literalInt = 0
literalNumber = 0.0
literalChar = 'a'
literalString = \"hello\"
literalArray = [ 0, 0 ]
literalRecord = { a: 0, b }
variable = a
prefixedVariable = A.a
",
    );

    let expressions = surface
        .body
        .iter_value_declarations()
        .flat_map(|value_declaration| {
            let equation = &value_declaration.equations[0];
            match &equation.binding {
                Binding::Unconditional { where_expr } => vec![&arena[where_expr.expr_id]],
                Binding::Guarded { guarded_exprs } => guarded_exprs
                    .iter()
                    .map(|guarded_expr| &arena[guarded_expr.where_expr.expr_id])
                    .collect_vec(),
            }
        })
        .collect_vec();

    insta::assert_debug_snapshot!(expressions);
}

#[test]
fn types() {
    let (surface, arena) = file_surface(
        "module Main where

application :: Function Argument
arrow :: Argument -> Result
constrained :: Constraint a => a
constructor :: Constructor
qualified :: forall a (b :: Type). a -> b -> a
parenthesized :: (Constructor Argument)
variable :: a
    ",
    );

    let types = surface
        .body
        .iter_value_declarations()
        .filter_map(|value_declaration| {
            let signature = value_declaration.annotation?;
            Some(&arena[signature])
        })
        .collect_vec();

    insta::assert_debug_snapshot!(types);
}

#[test]
fn value_declarations() {
    let (surface, _) = file_surface(
        "module Main where

const :: forall a b. a -> b -> a
const a b = a

basicGuards
  | 1909 = 'm'
  | 99.0 = \"a\"

bindGuard
  | a <- b = 0
  | c <- d = 1

mixedGuard
  | a <- b
  , b = 0
  where
  c = d
    ",
    );

    let value_declarations = surface.body.iter_value_declarations().collect_vec();

    insta::assert_debug_snapshot!(value_declarations);
}
