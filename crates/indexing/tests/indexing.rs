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
fn duplicate_expr_item() {
    let (_, _, errors) = index([
        "eq :: forall a. a -> a -> Bool",
        "class Eq a where",
        "  eq :: forall a. a -> a -> Bool",
        "instance eq :: Eq a",
        "foreign import eq :: forall a. a -> a -> Bool",
        "instance ord :: Ord a",
        "ord :: forall a. a -> a -> Comparison",
        "data Data = Id Int",
        "newtype Newtype = Id Int",
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
        "data Newtype :: Type"
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
