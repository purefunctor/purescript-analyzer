use std::fmt::Write;

use indexing::{IndexingErrors, IndexingResult};
use rowan::ast::AstNode;
use syntax::cst;

fn index<'s>(lines: impl AsRef<[&'s str]>) -> (cst::Module, IndexingResult, IndexingErrors) {
    let source = format!("module Main where\n{}", lines.as_ref().join("\n"));

    let lexed = lexing::lex(&source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let (index, errors) = indexing::index(&module);
    (module, index, errors)
}

#[test]
fn well_formed_module() {
    let (_, index, _) = index([
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
    let (_, _, errors) = index([
        "data Maybe a = Just a | Nothing",
        "data Maybe :: Type -> Type",
    ]);
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
