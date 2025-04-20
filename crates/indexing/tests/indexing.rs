use indexing::{FullIndexedModule, IndexingError, ItemKind, TermItemKind, index_module};
use itertools::Itertools;
use rowan::ast::AstNode;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use std::fmt::Write;
use syntax::cst;

fn index_source(source: &str) -> (cst::Module, FullIndexedModule) {
    let lexed = lexing::lex(source);
    let tokens = lexing::layout(&lexed);

    let (module, _) = parsing::parse(&lexed, &tokens);
    let module = cst::Module::cast(module).unwrap();

    let indexed = index_module(&module);
    (module, indexed)
}

#[test]
fn test_0000_success() {
    let (module, indexed) = index_source(include_str!("./fixtures/0000_success.txt"));

    let (mut tm, mut ty) = module
        .header()
        .and_then(|cst| {
            let annotation = cst.annotation()?;
            let token = annotation.text()?;
            let text = token.text();
            let categories = text.trim().split("\n");
            categories
                .map(|category| {
                    category
                        .trim_start_matches("--")
                        .split(",")
                        .map(str::trim)
                        .map(SmolStr::from)
                        .collect::<FxHashSet<SmolStr>>()
                })
                .collect_tuple()
        })
        .unwrap_or_default();

    for (_, item) in indexed.items.terms {
        if matches!(item.kind, TermItemKind::Instance { .. }) {
            assert!(item.exported, "Instances are always exported");
        }
        if matches!(item.kind, TermItemKind::Derive { .. }) {
            assert!(item.exported, "Derives are always exported");
        }
        let Some(name) = &item.name else { continue };
        if tm.contains(name) {
            assert!(item.exported, "Expected term '{}' to be exported.", name);
            tm.remove(name);
        } else {
            assert!(!item.exported, "Expected term '{}' to be hidden.", name);
        }
    }

    for (_, item) in indexed.items.types {
        let Some(name) = &item.name else { continue };
        if ty.contains(name) {
            assert!(item.exported, "Expected type '{}' to be exported.", name);
            ty.remove(name);
        } else {
            assert!(!item.exported, "Expected type '{}' to be hidden.", name);
        }
    }

    assert!(tm.is_empty(), "Invalid terms: {}", tm.iter().join(","));
    assert!(ty.is_empty(), "Invalid types: {}", ty.iter().join(","));
}

#[test]
fn test_0001_failure() {
    let (module, indexed) = index_source(include_str!("./fixtures/0001_failure.txt"));

    let mut snapshot = String::new();
    for error in &indexed.errors {
        match error {
            IndexingError::DuplicateItem { kind, existing } => {
                let kind = indexed.item_kind_range(*kind);
                let kind_t = module.syntax().text().slice(kind).to_string();
                let existing = indexed.existing_kind_range(*existing);
                let existing_t = module.syntax().text().slice(existing).to_string();
                writeln!(
                    snapshot,
                    "DuplicateItem\n  {:?} - {}\n  {:?} - {}",
                    kind,
                    kind_t.trim(),
                    existing,
                    existing_t.trim(),
                )
                .unwrap();
            }
            IndexingError::MismatchedItem { kind, existing } => {
                let kind = indexed.item_kind_range(*kind);
                let kind_t = module.syntax().text().slice(kind).to_string();
                let existing = indexed.existing_kind_range(*existing);
                let existing_t = module.syntax().text().slice(existing).to_string();
                writeln!(
                    snapshot,
                    "MismatchedItem\n  {:?} - {}\n  {:?} - {}",
                    kind,
                    kind_t.trim(),
                    existing,
                    existing_t.trim(),
                )
                .unwrap();
            }
            IndexingError::InvalidRole { id, existing } => {
                let kind = indexed.item_kind_range(ItemKind::Role(*id));
                let kind_t = module.syntax().text().slice(kind).to_string();
                writeln!(snapshot, "InvalidRole\n  {:?} - {}", kind, kind_t.trim()).unwrap();
                if let Some(existing) = existing {
                    let existing = indexed.type_item_range(*existing);
                    let existing_t = module.syntax().text().slice(existing).to_string();
                    writeln!(snapshot, "  {:?} - {}", existing, existing_t.trim()).unwrap();
                }
            }
            IndexingError::InvalidExport { id } => {
                let export = indexed.export_range(*id);
                let export_t = module.syntax().text().slice(export).to_string();
                writeln!(snapshot, "InvalidExport\n  {:?} - {}", export, export_t.trim()).unwrap();
            }
            IndexingError::DuplicateExport { id, existing } => {
                let id = indexed.export_range(*id);
                let id_t = module.syntax().text().slice(id).to_string();
                let existing = indexed.export_range(*existing);
                let existing_t = module.syntax().text().slice(existing).to_string();
                writeln!(
                    snapshot,
                    "DuplicateExport\n  {:?} - {}\n  {:?} - {}",
                    id,
                    id_t.trim(),
                    existing,
                    existing_t.trim()
                )
                .unwrap();
            }
            IndexingError::DuplicateImport { id, existing } => {
                let id = indexed.import_range(*id);
                let id_t = module.syntax().text().slice(id).to_string();
                let existing = indexed.import_range(*existing);
                let existing_t = module.syntax().text().slice(existing).to_string();
                writeln!(
                    snapshot,
                    "DuplicateImport\n  {:?} - {}\n  {:?} - {}",
                    id,
                    id_t.trim(),
                    existing,
                    existing_t.trim()
                )
                .unwrap();
            }
        }
    }

    insta::assert_snapshot!(snapshot);
}
