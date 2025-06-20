use std::iter;

use async_lsp::lsp_types::Range;
use files::FileId;
use indexing::{FullIndexedModule, ImportKind, TermItemId, TermItemKind, TypeItemId, TypeItemKind};
use itertools::Itertools;
use rowan::ast::AstNode;
use smol_str::{SmolStrBuilder, ToSmolStr};

use crate::{State, completion::Context, locate};

pub(super) fn term_import_item(
    state: &mut State,
    context: &Context,
    module_name: &str,
    term_name: &str,
    file_id: FileId,
    term_id: TermItemId,
) -> (Option<String>, Option<Range>) {
    let import_containing_term =
        context.resolved.unqualified.iter().find_map(|import| {
            match import.lookup_term(term_name) {
                Some((f_id, t_id, kind)) if (f_id, t_id) == (file_id, term_id) => {
                    Some((import, kind))
                }
                _ => None,
            }
        });

    let import_indexed = state.runtime.indexed(file_id);

    if let Some((import, kind)) = import_containing_term {
        // Implicit or hidden imports are skipped.
        if !matches!(import.kind, ImportKind::Explicit) {
            return (None, None);
        }

        // Names that are already imported are skipped.
        if matches!(kind, ImportKind::Explicit) {
            return (None, None);
        }

        let ptr = &context.indexed.source[import.id];
        let root = context.parsed.syntax_node();
        let node = ptr.to_node(&root);

        let import_list =
            node.import_list().expect("invariant violated: ImportKind::Explicit w/ no ImportList");

        let Some(import_item_name) = term_import_name(&import_indexed, term_name, term_id) else {
            return (None, None);
        };

        let import_item_name = {
            let mut buffer = SmolStrBuilder::new();
            buffer.push(' ');
            buffer.push_str(&import_item_name);
            buffer.finish()
        };

        let import_list = import_list
            .children()
            .map(|cst| {
                let cst = cst.syntax();
                cst.text().to_smolstr()
            })
            .chain(iter::once(import_item_name))
            .join(",");

        let import_text = format!("import {module_name} ({import_list})");

        let import_range = {
            let text_range = locate::text_range_after_annotation(&ptr.syntax_node_ptr(), &root);
            text_range.map(|range| locate::text_range_to_range(context.content, range))
        };

        (Some(import_text), import_range)
    } else {
        if let Some(import_item_name) = term_import_name(&import_indexed, term_name, term_id) {
            let import_text = format!("import {module_name} ({import_item_name})\n");
            (Some(import_text), None)
        } else {
            (None, None)
        }
    }
}

pub(super) fn type_import_item(
    state: &mut State,
    context: &Context,
    module_name: &str,
    type_name: &str,
    file_id: FileId,
    type_id: TypeItemId,
) -> (Option<String>, Option<Range>) {
    let import_containing_type =
        context.resolved.unqualified.iter().find_map(|import| {
            match import.lookup_type(type_name) {
                Some((f_id, t_id, kind)) if (f_id, t_id) == (file_id, type_id) => {
                    Some((import, kind))
                }
                _ => None,
            }
        });

    let import_indexed = state.runtime.indexed(file_id);

    if let Some((import, kind)) = import_containing_type {
        // Implicit or hidden imports are skipped.
        if !matches!(import.kind, ImportKind::Explicit) {
            return (None, None);
        }

        // Names that are already imported are skipped.
        if matches!(kind, ImportKind::Explicit) {
            return (None, None);
        }

        let ptr = &context.indexed.source[import.id];
        let root = context.parsed.syntax_node();
        let node = ptr.to_node(&root);

        let import_list =
            node.import_list().expect("invariant violated: ImportKind::Explicit w/ no ImportList");

        let Some(import_item_name) = type_import_name(&import_indexed, type_name, type_id) else {
            return (None, None);
        };

        let import_item_name = {
            let mut buffer = SmolStrBuilder::new();
            buffer.push(' ');
            buffer.push_str(&import_item_name);
            buffer.finish()
        };

        let import_list = import_list
            .children()
            .map(|cst| {
                let cst = cst.syntax();
                cst.text().to_smolstr()
            })
            .chain(iter::once(import_item_name))
            .join(",");

        let import_text = format!("import {module_name} ({import_list})");

        let import_range = {
            let text_range = locate::text_range_after_annotation(&ptr.syntax_node_ptr(), &root);
            text_range.map(|range| locate::text_range_to_range(context.content, range))
        };

        (Some(import_text), import_range)
    } else {
        if let Some(import_item_name) = type_import_name(&import_indexed, type_name, type_id) {
            let import_text = format!("import {module_name} ({import_item_name})\n");
            (Some(import_text), None)
        } else {
            (None, None)
        }
    }
}

fn term_import_name(
    import_indexed: &FullIndexedModule,
    term_name: &str,
    term_id: TermItemId,
) -> Option<String> {
    let term_item = &import_indexed.items[term_id];
    if matches!(term_item.kind, TermItemKind::Constructor { .. }) {
        let type_id = import_indexed
            .pairs
            .constructor_type(term_id)
            .expect("invariant violated: floating data constructor");
        let type_item = &import_indexed.items[type_id];
        let Some(type_name) = &type_item.name else {
            tracing::trace!("Missing name for {:?}", type_id);
            return None;
        };
        Some(format!("{type_name}(..)"))
    } else {
        Some(format!("{term_name}"))
    }
}

fn type_import_name(
    import_indexed: &FullIndexedModule,
    type_name: &str,
    type_id: TypeItemId,
) -> Option<String> {
    let type_item = &import_indexed.items[type_id];
    if matches!(type_item.kind, TypeItemKind::Class { .. }) {
        Some(format!("class {type_name}"))
    } else {
        Some(format!("{type_name}"))
    }
}
