use indexing::{
    Duplicate, ExprItem, FullIndexingResult, Id, IndexingError, TypeGroupId, TypeItem, ValueGroupId,
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
fn valid_module() {
    let (module, index) = index(&[
        "life :: Int",
        "life = 42",
        "data Boolean :: Type",
        "data Boolean = True | False",
        "newtype Id :: Type -> Type",
        "newtype Id a = Id a",
        "type Fn :: Type -> Type -> Type",
        "type Fn a b = a -> b",
        "class Eq :: Type -> Constraint",
        "class Eq a where",
        "  eq :: a -> a -> Boolean",
        "instance eqInt :: Eq Int where",
        "  eq = eqIntImpl",
    ]);

    assert!(index.errors.is_empty());

    assert!(matches!(index.nominal.lookup_type_item("Boolean"), Some((_, TypeItem::Data(_)))));
    assert!(matches!(index.nominal.lookup_type_item("Id"), Some((_, TypeItem::Newtype(_)))));
    assert!(matches!(index.nominal.lookup_type_item("Fn"), Some((_, TypeItem::Synonym(_)))));
    assert!(matches!(index.nominal.lookup_type_item("Eq"), Some((_, TypeItem::Class(_)))));

    if let Some((_, ExprItem::Value(ValueGroupId { signature: Some(id), .. }))) =
        index.nominal.lookup_expr_item("life")
    {
        let ptr = index.source_map.declaration_ptr(*id).unwrap();
        let node = ptr.to_node(module.syntax());

        assert_eq!(node.syntax().text(), "\nlife :: Int");
        assert_eq!(index.source_map.declaration_id(ptr), Some(*id));
    } else {
        unreachable!()
    }

    if let Some((_, ExprItem::Method(id))) = index.nominal.lookup_expr_item("eq") {
        let ptr = index.source_map.class_member_ptr(*id).unwrap();
        let node = ptr.to_node(module.syntax());

        assert_eq!(node.syntax().text(), "\n  eq :: a -> a -> Boolean");
        assert_eq!(index.source_map.class_member_id(ptr), Some(*id));
    } else {
        unreachable!()
    }

    if let Some((_, ExprItem::Instance(id))) = index.nominal.lookup_expr_item("eqInt") {
        let ptr = index.source_map.instance_ptr(*id).unwrap();
        let node = ptr.to_node(module.syntax());

        assert_eq!(node.syntax().text(), "\ninstance eqInt :: Eq Int where\n  eq = eqIntImpl");
        assert_eq!(index.source_map.instance_id(ptr), Some(*id));
    } else {
        unreachable!()
    }

    if let Some((_, ExprItem::Constructor(id))) = index.nominal.lookup_expr_item("True") {
        let ptr = index.source_map.constructor_ptr(*id).unwrap();
        let node = ptr.to_node(module.syntax());

        assert_eq!(node.syntax().text(), " True");
        assert_eq!(index.source_map.constructor_id(ptr), Some(*id));
    } else {
        unreachable!()
    }

    if let Some((_, ExprItem::Constructor(id))) = index.nominal.lookup_expr_item("Id") {
        if let Some(id) = index.relational.of_constructor(*id) {
            assert!(matches!(index.nominal.index_type_item(id), Some(TypeItem::Newtype(_))));
        } else {
            unreachable!()
        }
    } else {
        unreachable!()
    }

    if let Some((_, ExprItem::Method(id))) = index.nominal.lookup_expr_item("eq") {
        if let Some(id) = index.relational.of_method(*id) {
            assert!(matches!(index.nominal.index_type_item(id), Some(TypeItem::Class(_))));
        } else {
            unreachable!()
        }
    } else {
        unreachable!()
    }

    let (id, item) = index.nominal.lookup_expr_item("life").unwrap();
    assert_eq!(index.nominal.index_expr_item(id), Some(item));

    if let Some((id, _)) = index.nominal.lookup_type_item("Boolean") {
        let constructors: Vec<_> = index.relational.constructors_of(id).collect();
        if let &[t, f] = &constructors[..] {
            let t_ptr = index.source_map.constructor_ptr(t).unwrap();
            let t_node = t_ptr.to_node(module.syntax());

            assert_eq!(t_node.syntax().text(), " True");
            assert_eq!(index.source_map.constructor_id(t_ptr), Some(t));

            let f_ptr = index.source_map.constructor_ptr(f).unwrap();
            let f_node = f_ptr.to_node(module.syntax());

            assert_eq!(f_node.syntax().text(), " False");
            assert_eq!(index.source_map.constructor_id(f_ptr), Some(f));
        } else {
            unreachable!()
        }
    } else {
        unreachable!()
    }

    if let Some((id, _)) = index.nominal.lookup_type_item("Id") {
        let constructors: Vec<_> = index.relational.constructors_of(id).collect();
        if let &[id] = &constructors[..] {
            let ptr = index.source_map.constructor_ptr(id).unwrap();
            let node = ptr.to_node(module.syntax());

            assert_eq!(node.syntax().text(), " Id a");
            assert_eq!(index.source_map.constructor_id(ptr), Some(id));
        } else {
            unreachable!()
        }
    } else {
        unreachable!()
    }

    if let Some((id, _)) = index.nominal.lookup_type_item("Eq") {
        let methods: Vec<_> = index.relational.methods_of(id).collect();
        if let &[id] = &methods[..] {
            let ptr = index.source_map.class_member_ptr(id).unwrap();
            let node = ptr.to_node(module.syntax());

            assert_eq!(node.syntax().text(), "\n  eq :: a -> a -> Boolean");
            assert_eq!(index.source_map.class_member_id(ptr), Some(id));
        } else {
            unreachable!()
        }
    } else {
        unreachable!()
    }

    if let Some((_, TypeItem::Synonym(TypeGroupId { declaration: Some(id), .. }))) =
        index.nominal.lookup_type_item("Fn")
    {
        let ptr = index.source_map.declaration_ptr(*id).unwrap();
        let node = ptr.to_node(module.syntax());

        assert_eq!(node.syntax().text(), "\ntype Fn a b = a -> b");
        assert_eq!(index.source_map.declaration_id(ptr), Some(*id));
    } else {
        unreachable!()
    }
}

#[test]
fn late_signature_error() {
    let (_, index) = index(&[
        "life = 42",
        "life :: Int",
        "data Boolean = True | False",
        "data Boolean :: Type",
        "newtype Id a = Id a",
        "newtype Id :: Type -> Type",
        "type Fn a b = a -> b",
        "type Fn :: Type -> Type -> Type",
        "class Eq a",
        "class Eq :: Type -> Constraint",
    ]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::LateSignature {
                declaration: Id::from_raw(0),
                signature: Id::from_raw(1)
            },
            IndexingError::LateSignature {
                declaration: Id::from_raw(2),
                signature: Id::from_raw(3)
            },
            IndexingError::LateSignature {
                declaration: Id::from_raw(4),
                signature: Id::from_raw(5)
            },
            IndexingError::LateSignature {
                declaration: Id::from_raw(6),
                signature: Id::from_raw(7)
            },
            IndexingError::LateSignature {
                declaration: Id::from_raw(8),
                signature: Id::from_raw(9)
            },
        ]
    );
}

#[test]
fn duplicate_expr_item() {
    let (_, index) = index(&[
        "life :: Int",
        "life = 42",
        "instance life :: Life",
        "class Life where",
        "  life :: Int",
        "life :: Int",
        "data IdData a = Id a",
        "newtype IdNewtype a = Id a",
    ]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::DuplicateExprItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Instance(Id::from_raw(0)),
            },
            IndexingError::DuplicateExprItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::ClassMember(Id::from_raw(0)),
            },
            IndexingError::LateSignature {
                declaration: Id::from_raw(1),
                signature: Id::from_raw(4)
            },
            IndexingError::DuplicateExprItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(4)),
            },
            IndexingError::DuplicateExprItem {
                item_id: Id::from_raw(1),
                duplicate: Duplicate::Constructor(Id::from_raw(1)),
            },
        ]
    );
}

#[test]
fn value_is_duplicate() {
    let (_, index) = index([
        "class Eq a where",
        "  eq :: a -> a -> Boolean",
        "eq :: Int -> Int -> Boolean",
        "eq = eqIntImpl",
    ]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::DuplicateExprItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(1)),
            },
            IndexingError::DuplicateExprItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(2)),
            }
        ]
    );
}

#[test]
fn duplicate_data() {
    let (_, index) =
        index(&["data Id0 :: Type", "data Id0 :: Type", "data Id1 = Id2", "data Id1 = Id3"]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(1)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(1),
                duplicate: Duplicate::Declaration(Id::from_raw(3)),
            },
        ]
    );
}

#[test]
fn duplicate_newtype() {
    let (_, index) = index(&[
        "newtype Id0 :: Type",
        "newtype Id0 :: Type",
        "newtype Id1 = Id2 Int",
        "newtype Id1 = Id3 Int",
    ]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(1)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(1),
                duplicate: Duplicate::Declaration(Id::from_raw(3)),
            },
        ]
    );
}

#[test]
fn duplicate_class() {
    let (_, index) = index(&["class Id0 :: Type", "class Id0 :: Type", "class Id1", "class Id1"]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(1)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(1),
                duplicate: Duplicate::Declaration(Id::from_raw(3)),
            },
        ]
    );
}

#[test]
fn duplicate_synonym() {
    let (_, index) =
        index(&["type Id0 :: Type", "type Id0 :: Type", "type Id1 = Id0", "type Id1 = Id0"]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(1)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(1),
                duplicate: Duplicate::Declaration(Id::from_raw(3)),
            },
        ]
    );
}

#[test]
fn duplicate_type_item() {
    let (_, index) = index(&[
        "data Id0 :: Type",
        "data Id0 = Id10 Int",
        "newtype Id0 :: Type",
        "newtype Id0 = Id11 Int",
        "class Id0 :: Type",
        "class Id0",
        "type Id0 :: Type",
        "type Id0 = Id0",
    ]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(2)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(3)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(4)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(5)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(6)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(7)),
            },
        ]
    );
}

#[test]
fn data_is_duplicate() {
    let (_, index) = index(&["class Eq", "data Eq :: Type", "data Eq"]);
    assert_eq!(
        &index.errors,
        &[
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(1)),
            },
            IndexingError::DuplicateTypeItem {
                item_id: Id::from_raw(0),
                duplicate: Duplicate::Declaration(Id::from_raw(2)),
            }
        ]
    );
}
