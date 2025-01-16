use indexing::{
    Duplicate, ExprItem, FullIndexingResult, Id, IndexingError, TypeItem, ValueGroupId,
};
use rowan::ast::AstNode;
use syntax::cst;

fn index<'line>(source: impl AsRef<[&'line str]>) -> (cst::Module, FullIndexingResult) {
    let source = format!("module Main where\n{}", source.as_ref().join("\n"));

    let lexed = lexing::lex(&source);
    let tokens = lexing::layout(&lexed);
    let (node, _) = parsing::parse(&lexed, &tokens);

    let module = cst::Module::cast(node).unwrap();
    let index = indexing::index(&module);

    (module, index)
}

#[test]
fn value_declarations() {
    let (_, index) = index(&[
        "life :: Int",
        "life = 42",
        "isJust :: Maybe a -> Bool",
        "isJust (Just _) = true",
        "isJust Nothing = false",
    ]);

    assert!(index.errors.is_empty());

    assert!(matches!(
        index.nominal.lookup_expr_item("life"),
        Some((_, ExprItem::Value(ValueGroupId { signature, equations }))) if signature.is_some() && equations.len() == 1
    ));

    assert!(matches!(
        index.nominal.lookup_expr_item("isJust"),
        Some((_, ExprItem::Value(ValueGroupId { signature, equations }))) if signature.is_some() && equations.len() == 2
    ));
}

#[test]
fn early_value_declarations() {
    let (_, index) =
        index(&["isJust (Just _) = true", "isJust Nothing = false", "isJust :: Maybe a -> Bool"]);

    assert_eq!(
        &index.errors[..],
        &[
            IndexingError::EarlyDeclaration {
                declaration: Id::from_raw(0),
                signature: Id::from_raw(2)
            },
            IndexingError::EarlyDeclaration {
                declaration: Id::from_raw(1),
                signature: Id::from_raw(2)
            }
        ]
    )
}

#[test]
fn empty_value_signature() {
    let (_, index) = index(&["life :: Int"]);

    assert_eq!(&index.errors[..], &[IndexingError::EmptySignature { signature: Id::from_raw(0) }])
}

#[test]
fn synonym_declarations() {
    let (_, index) = index(&[
        "type Id :: Type",
        "type Id = Int",
        "type Const :: Type -> Type -> Type",
        "type Const a b = a",
    ]);

    assert!(index.errors.is_empty());

    assert!(matches!(index.nominal.lookup_type_item("Id"), Some((_, TypeItem::Synonym(_)))));

    assert!(matches!(index.nominal.lookup_type_item("Const"), Some((_, TypeItem::Synonym(_)))));
}

#[test]
fn early_synonym_declaration() {
    let (_, index) = index(&["type Id = Int", "type Id :: Type"]);

    assert_eq!(
        &index.errors[..],
        &[IndexingError::EarlyDeclaration {
            declaration: Id::from_raw(0),
            signature: Id::from_raw(1)
        }]
    )
}

#[test]
fn empty_synonym_signature() {
    let (_, index) = index(&["type Id :: Type"]);

    assert_eq!(&index.errors[..], &[IndexingError::EmptySignature { signature: Id::from_raw(0) }])
}

#[test]
fn class_declarations() {
    let (_, index) =
        index(&["class Eq :: Type -> Constraint", "class Eq a where", "  eq :: a -> a -> Bool"]);

    assert!(index.errors.is_empty());

    assert!(matches!(index.nominal.lookup_type_item("Eq"), Some((_, TypeItem::Class(_)))));

    assert!(matches!(index.nominal.lookup_expr_item("eq"), Some((_, ExprItem::Method(_)))));
}

#[test]
fn early_class_declaration() {
    let (_, index) =
        index(&["class Eq a where", "  eq :: a -> a -> Bool", "class Eq :: Type -> Constraint"]);

    assert_eq!(
        &index.errors[..],
        &[IndexingError::EarlyDeclaration {
            declaration: Id::from_raw(0),
            signature: Id::from_raw(1)
        }]
    )
}

#[test]
fn empty_class_signature() {
    let (_, index) = index(&["class Eq :: Type -> Signature"]);

    assert_eq!(&index.errors[..], &[IndexingError::EmptySignature { signature: Id::from_raw(0) }])
}

#[test]
fn duplicate_class_member() {
    let (_, index) =
        index(&["class Eq a where", "  eq :: a -> a -> Bool", "  eq :: a -> a -> Bool"]);

    assert!(matches!(index.nominal.lookup_expr_item("eq"), Some((_, ExprItem::Method(_)))));

    assert_eq!(
        &index.errors[..],
        &[IndexingError::DuplicateExprItem {
            item_id: Id::from_raw(0),
            duplicate: Duplicate::ClassMember(Id::from_raw(1))
        }]
    )
}
