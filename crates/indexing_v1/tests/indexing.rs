use std::fmt::Write;

use indexing_v1::{IndexingErrors, IndexingResult};
use rowan::ast::AstNode;
use syntax::cst;

fn index_source<'s>(
    source: impl AsRef<[&'s str]>,
) -> (cst::Module, IndexingResult, IndexingErrors) {
    let source = source.as_ref().join("\n");

    let lexed = lexing::lex(&source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let (index, errors) = indexing_v1::index(&module);
    (module, index, errors)
}

fn index<'s>(lines: impl AsRef<[&'s str]>) -> (cst::Module, IndexingResult, IndexingErrors) {
    let source = format!("module Main where\n{}", lines.as_ref().join("\n"));
    index_source([source.as_str()])
}

#[test]
fn well_formed_module() {
    let (_, index, _) = index([
        "import Prelude as Prelude",
        "import Data.List as Data.List",
        "import Halogen as H",
        "import Halogen.HTML as H",
        "id :: forall a. a -> a",
        "id x = x",
        "data Maybe :: Type -> Type",
        "data Maybe a = Just a | Nothing",
        "type role Maybe representational",
        "class Eq :: Type -> Constraint",
        "class Eq a where",
        "  eq :: a -> a -> Bool",
        "instance eqInt :: Eq Int where",
        "  eq :: Int -> Int -> Bool",
        "  eq = primEqInt",
        "newtype Identity :: Type -> Type",
        "newtype Identity a = Identity a",
        "type role Identity representational",
        "type IdentityInt :: Type",
        "type IdentityInt = Identity Int",
        "foreign import primEqInt :: Int -> Int -> Bool",
        "foreign import data Functor :: Type -> Type",
        "type role Functor phantom",
        "derive newtype instance eqIdentity :: Eq (Identity a)",
        "infix 5 eq as ==",
        "infix 5 type Eq as ==",
    ]);

    let prelude = index.nominal.lookup_qualified("Prelude");
    let data_list = index.nominal.lookup_qualified("Data.List");
    let halogen = index.nominal.lookup_qualified("H");
    let id = index.nominal.lookup_expr_item("id");
    let maybe = index.nominal.lookup_type_item("Maybe");
    let just = index.nominal.lookup_expr_item("Just");
    let nothing = index.nominal.lookup_expr_item("Nothing");
    let eq_class = index.nominal.lookup_type_item("Eq");
    let eq = index.nominal.lookup_expr_item("eq");
    let eq_int = index.nominal.lookup_expr_item("eqInt");
    let identity = index.nominal.lookup_type_item("Identity");
    let identity_int = index.nominal.lookup_type_item("IdentityInt");
    let prim_eq_int = index.nominal.lookup_expr_item("primEqInt");
    let functor = index.nominal.lookup_type_item("Functor");
    let eq_identity = index.nominal.lookup_expr_item("eqIdentity");
    let expr_equal = index.nominal.lookup_expr_item("==");
    let type_equal = index.nominal.lookup_type_item("==");

    let mut snapshot = String::new();
    writeln!(snapshot, "prelude: {:?}", prelude).unwrap();
    writeln!(snapshot, "data_list: {:?}", data_list).unwrap();
    writeln!(snapshot, "halogen: {:?}", halogen).unwrap();
    writeln!(snapshot, "id: {:?}", id).unwrap();
    writeln!(snapshot, "maybe: {:?}", maybe).unwrap();
    writeln!(snapshot, "just: {:?}", just).unwrap();
    writeln!(snapshot, "nothing: {:?}", nothing).unwrap();
    writeln!(snapshot, "eq_class: {:?}", eq_class).unwrap();
    writeln!(snapshot, "eq: {:?}", eq).unwrap();
    writeln!(snapshot, "eq_int: {:?}", eq_int).unwrap();
    writeln!(snapshot, "identity: {:?}", identity).unwrap();
    writeln!(snapshot, "identity_int: {:?}", identity_int).unwrap();
    writeln!(snapshot, "prim_eq_int: {:?}", prim_eq_int).unwrap();
    writeln!(snapshot, "functor: {:?}", functor).unwrap();
    writeln!(snapshot, "eq_identity: {:?}", eq_identity).unwrap();
    writeln!(snapshot, "expr_equal: {:?}", expr_equal).unwrap();
    writeln!(snapshot, "type_equal: {:?}", type_equal).unwrap();
    insta::assert_snapshot!(snapshot);
}

#[test]
fn duplicate_expr_item() {
    let (_, _, errors) = index([
        "eq :: forall a. a -> a -> Bool",
        "class Eq a where",
        "  eq :: forall a. a -> a -> Bool",
        "instance eq :: Eq a",
        "foreign import eq :: forall a. a -> a -> Bool",
        "instance ord :: Ord a",
        "ord :: forall a. a -> a -> Comparison",
        "ord = ordImpl",
        "data Data = Id Int",
        "newtype Newtype = Id Int",
        "infix 5 eq as ==",
        "infix 5 eq as ==",
        "derive instance eq :: Eq a",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn duplicate_type_item() {
    let (_, _, errors) = index([
        "data Data = Constructor0",
        "newtype Data = Constructor1",
        "class Data",
        "foreign import data Data :: Type",
        "type Data = Type",
        "newtype Newtype = Constructor2",
        "data Newtype = Constructor3",
        "data Newtype :: Type",
        "data Id = Constructor4",
        "data Id = Constructor5",
        "infix 5 type Eq as ==",
        "infix 5 type Eq as ==",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn value_duplicate_signature() {
    let (_, _, errors) = index([
        "isJust :: forall a. Maybe a -> Bool",
        "isJust (Just _) = true",
        "isJust Nothing = false",
        "isJust :: forall a. Maybe a -> Bool",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn value_non_consecutive() {
    let (_, _, errors) = index([
        "isJust :: forall a. Maybe a -> Bool",
        "isJust (Just _) = true",
        "life = 42",
        "isJust Nothing = false",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn value_late_signature() {
    let (_, _, errors) = index([
        "isJust (Just _) = true",
        "isJust Nothing = false",
        "isJust :: forall a. Maybe a -> Bool",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn type_duplicate_signature() {
    let (_, _, errors) = index([
        "data Maybe :: Type -> Type",
        "data Maybe a = Just a | Nothing",
        "data Maybe :: Type -> Type",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn type_non_consecutive() {
    let (_, _, errors) = index([
        "data Maybe :: Type -> Type",
        "life :: Int",
        "life = 42",
        "data Maybe a = Just a | Nothing",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn type_late_signature() {
    let (_, _, errors) = index(["data Maybe a = Just a | Nothing", "data Maybe :: Type -> Type"]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn type_role_errors() {
    let (_, _, errors) = index([
        "class Eq a",
        "type role Eq representational",
        "type role Ord representational",
        "newtype Id :: Type -> Type",
        "type role Id nominal",
        "type role Id phantom",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn well_formed_export() {
    let (_, index, _) = index_source([
        "module Main (life, class Eq, Synonym, Data(..), List(Cons, Nil), (+), type (+), module Export) where",
        "import Lib as Export",
        "life = 42", 
        "class Eq", 
        "type Synonym = Int", 
        "data Data = A | B", 
        "data List = Cons | Nil",
        "infix 5 + as add",
        "infix 5 type + as Add",
    ]);

    let life = index.nominal.lookup_expr_item("life");
    let eq_class = index.nominal.lookup_type_item("Eq");
    let synonym = index.nominal.lookup_type_item("Synonym");
    let data = index.nominal.lookup_type_item("Data");
    let data_a = index.nominal.lookup_expr_item("A");
    let data_b = index.nominal.lookup_expr_item("B");
    let list = index.nominal.lookup_type_item("List");
    let list_cons = index.nominal.lookup_expr_item("Cons");
    let list_nil = index.nominal.lookup_expr_item("Nil");
    let plus = index.nominal.lookup_expr_item("+");
    let plus_type = index.nominal.lookup_type_item("+");
    let export = index.nominal.lookup_qualified("Export");

    let mut snapshot = String::default();
    writeln!(snapshot, "life: {:?}", life).unwrap();
    writeln!(snapshot, "eq_class: {:?}", eq_class).unwrap();
    writeln!(snapshot, "synonym: {:?}", synonym).unwrap();
    writeln!(snapshot, "data: {:?}", data).unwrap();
    writeln!(snapshot, "data_a: {:?}", data_a).unwrap();
    writeln!(snapshot, "data_b: {:?}", data_b).unwrap();
    writeln!(snapshot, "list: {:?}", list).unwrap();
    writeln!(snapshot, "list_cons: {:?}", list_cons).unwrap();
    writeln!(snapshot, "list_nil: {:?}", list_nil).unwrap();
    writeln!(snapshot, "plus: {:?}", plus).unwrap();
    writeln!(snapshot, "plus_type: {:?}", plus_type).unwrap();
    writeln!(snapshot, "export: {:?}", export).unwrap();
    insta::assert_snapshot!(snapshot);
}

#[test]
fn duplicate_export_item() {
    let (_, _, errors) = index_source([
        "module Main (life, life, class Eq, class Eq, Synonym, Synonym, Data(..), Data(..), (+), (+), type (+), type (+), module Export, module Export) where",
        "import Lib as Export",
        "life = 42", 
        "class Eq", 
        "type Synonym = Int", 
        "data Data = A | B", 
        "infix 5 + as add",
        "infix 5 type + as Add",
    ]);
    insta::assert_debug_snapshot!(errors);
}

#[test]
fn export_type_item_error() {
    let (_, _, errors) = index_source([
        "module Main (Id(..), Index(Index)) where",
        "type Id = Int",
        "type Index = Int",
    ]);
    insta::assert_debug_snapshot!(errors);
}
