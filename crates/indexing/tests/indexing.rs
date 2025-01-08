fn index<'line>(source: impl AsRef<[&'line str]>) -> indexing::FullIndexingResult {
    let source = format!("module Main where\n{}", source.as_ref().join("\n"));
    let lexed = lexing::lex(&source);
    let tokens = lexing::layout(&lexed);
    let (node, _) = parsing::parse(&lexed, &tokens);
    indexing::index(node)
}

macro_rules! idx {
    ($v:expr) => {
        la_arena::Idx::from_raw(la_arena::RawIdx::from_u32($v))
    };
}

#[test]
fn value_group() {
    let module_map = index(["main :: Effect Unit", "main = pure unit"]);
    let group = module_map.value.get("main");
    assert_eq!(
        group,
        Some(&indexing::ValueGroup { signature: Some(idx!(0)), equations: vec![idx!(1)] })
    );
}

#[test]
fn value_group_signature_conflict() {
    let module_map = index(["main :: Effect Unit", "main :: Effect Unit"]);
    assert_eq!(
        &module_map.errors,
        &[indexing::IndexingError::SignatureConflict { existing: idx!(0), duplicate: idx!(1) }]
    );
}

#[test]
fn value_group_late_signature() {
    let module_map = index(["main = pure unit", "main :: Effect Unit"]);
    let group = module_map.value.get("main");
    assert_eq!(
        group,
        Some(&indexing::ValueGroup { signature: Some(idx!(1)), equations: vec![idx!(0)] })
    );
    assert_eq!(
        &module_map.errors,
        &[indexing::IndexingError::SignatureIsLate { equation: idx!(0), signature: idx!(1) }]
    );
}

#[test]
fn value_group_late_signature_conflict() {
    let module_map = index(["main :: Effect Unit", "main = pure unit", "main :: Effect Unit"]);
    let group = module_map.value.get("main");
    assert_eq!(
        group,
        Some(&indexing::ValueGroup { signature: Some(idx!(0)), equations: vec![idx!(1)] })
    );
    assert_eq!(
        &module_map.errors,
        &[
            indexing::IndexingError::SignatureIsLate { equation: idx!(1), signature: idx!(2) },
            indexing::IndexingError::SignatureConflict { existing: idx!(0), duplicate: idx!(2) },
        ]
    )
}

#[test]
fn value_group_late_signatures_conflict() {
    let module_map = index(["main = pure unit", "main :: Effect Unit", "main :: Effect Unit"]);
    let group = module_map.value.get("main");
    assert_eq!(
        group,
        Some(&indexing::ValueGroup { signature: Some(idx!(1)), equations: vec![idx!(0)] })
    );
    assert_eq!(
        &module_map.errors,
        &[
            indexing::IndexingError::SignatureIsLate { equation: idx!(0), signature: idx!(1) },
            indexing::IndexingError::SignatureIsLate { equation: idx!(0), signature: idx!(2) },
            indexing::IndexingError::SignatureConflict { existing: idx!(1), duplicate: idx!(2) },
        ]
    )
}

#[test]
fn instance_chain() {
    let module_map = index([
        "instance eqInt :: Eq Int where",
        "  eq :: Int -> Int -> Boolean",
        "  eq = eqIntImpl",
        "else instance eqBoolean :: Eq Boolean where",
        "  eq :: Boolean -> Boolean -> Boolean",
        "  eq = eqBooleanImpl",
    ]);

    let eq_int = module_map.instance.by_name.get("eqInt");
    assert_eq!(eq_int, Some(&idx!(1)));

    let eq_boolean = module_map.instance.by_name.get("eqBoolean");
    assert_eq!(eq_boolean, Some(&idx!(4)));

    let eq_chains = module_map.instance.by_type.get("Eq");
    assert_eq!(eq_chains, Some(&vec![idx!(1), idx!(4)]));

    let eq_chain_declarations =
        module_map.instance.instance_graph.neighbors(idx!(0)).collect::<Vec<_>>();
    assert_eq!(eq_chain_declarations, vec![idx!(1), idx!(4)]);

    let eq_int_statements =
        module_map.instance.statement_graph.neighbors(idx!(1)).collect::<Vec<_>>();
    assert_eq!(eq_int_statements, vec![idx!(2), idx!(3)]);

    let eq_boolean_statements =
        module_map.instance.statement_graph.neighbors(idx!(4)).collect::<Vec<_>>();
    assert_eq!(eq_boolean_statements, vec![idx!(5), idx!(6)]);
}

#[test]
fn instance_chain_signature_conflict() {
    let module_map = index([
        "instance eqInt :: Eq Int where",
        "  eq :: Int -> Int -> Boolean",
        "  eq :: Int -> Int -> Boolean",
    ]);

    assert_eq!(
        &module_map.errors,
        &[indexing::IndexingError::SignatureConflict { existing: idx!(2), duplicate: idx!(3) }]
    );
}

#[test]
fn instance_chain_late_signature() {
    let module_map = index([
        "instance eqInt :: Eq Int where",
        "  eq = eqIntImpl",
        "  eq :: Int -> Int -> Boolean",
    ]);

    assert_eq!(
        &module_map.errors,
        &[indexing::IndexingError::SignatureIsLate { equation: idx!(2), signature: idx!(3) }]
    );
}

#[test]
fn instance_chain_late_signature_conflict() {
    let module_map = index([
        "instance eqInt :: Eq Int where",
        "  eq :: Int -> Int -> Boolean",
        "  eq = eqIntImpl",
        "  eq :: Int -> Int -> Boolean",
    ]);

    assert_eq!(
        &module_map.errors,
        &[
            indexing::IndexingError::SignatureIsLate { equation: idx!(3), signature: idx!(4) },
            indexing::IndexingError::SignatureConflict { existing: idx!(2), duplicate: idx!(4) },
        ]
    );
}

#[test]
fn instance_chain_late_signatures_conflict() {
    let module_map = index([
        "instance eqInt :: Eq Int where",
        "  eq = eqIntImpl",
        "  eq :: Int -> Int -> Boolean",
        "  eq :: Int -> Int -> Boolean",
    ]);

    assert_eq!(
        &module_map.errors,
        &[
            indexing::IndexingError::SignatureIsLate { equation: idx!(2), signature: idx!(3) },
            indexing::IndexingError::SignatureIsLate { equation: idx!(2), signature: idx!(4) },
            indexing::IndexingError::SignatureConflict { existing: idx!(3), duplicate: idx!(4) },
        ]
    );
}
