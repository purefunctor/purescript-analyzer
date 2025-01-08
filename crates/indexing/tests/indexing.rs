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
fn instance_chain() {
    let _ = index([
        "instance eqIntInt :: Eq Int Int where eq = 0",
        "else instance eqBooleanBoolean :: Eq Boolean Boolean where eq = 1",
    ]);
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
