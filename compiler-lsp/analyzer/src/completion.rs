pub(crate) mod context;
pub(crate) mod edit;
pub mod resolve;

use async_lsp::lsp_types::*;
use building::QueryEngine;
use context::{CompletionFilter, CompletionLocation};
use files::{FileId, Files};
use indexing::{FullIndexedModule, ImportKind};
use parsing::ParsedModule;
use resolve::CompletionResolveData;
use resolving::{FullResolvedModule, ResolvedImport};
use rowan::{TokenAtOffset, ast::AstNode};

use crate::locate;

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Option<CompletionResponse> {
    let uri = uri.as_str();

    let id = files.id(uri)?;
    let content = engine.content(id);
    let (parsed, _) = engine.parsed(id).ok()?;

    let offset = locate::position_to_offset(&content, position)?;

    let node = parsed.syntax_node();
    let token = node.token_at_offset(offset);

    let token = match token {
        TokenAtOffset::None => return None,
        TokenAtOffset::Single(token) => token,
        TokenAtOffset::Between(token, _) => token,
    };

    let filter = CompletionFilter::new(&content, &token);
    let location = CompletionLocation::new(&content, position);

    let indexed = engine.indexed(id).ok()?;
    let resolved = engine.resolved(id).ok()?;

    let _span = tracing::info_span!("completion_items").entered();
    tracing::info!("Collecting {:?} items", location);

    let context = Context {
        id,
        content: &content,
        parsed: &parsed,
        indexed: &indexed,
        resolved: &resolved,
        location: &location,
        filter: &filter,
    };

    let items = collect(engine, files, &context);
    let is_incomplete = items.len() > 5;

    Some(CompletionResponse::List(CompletionList { is_incomplete, items }))
}

const ACCEPTANCE_THRESHOLD: f64 = 0.5;

struct Context<'a> {
    id: FileId,
    content: &'a str,
    parsed: &'a ParsedModule,
    indexed: &'a FullIndexedModule,
    resolved: &'a FullResolvedModule,
    location: &'a CompletionLocation,
    filter: &'a CompletionFilter,
}

impl Context<'_> {
    fn insert_import_range(&self) -> Option<Range> {
        let range = self.parsed.cst().imports().map_or_else(
            || Some(self.parsed.cst().header()?.syntax().text_range()),
            |cst| Some(cst.syntax().text_range()),
        )?;

        let mut position = locate::offset_to_position(self.content, range.end());

        position.line += 1;
        position.character = 0;

        Some(Range::new(position, position))
    }

    fn allow_prefix(&self, name: &str) -> bool {
        self.filter.prefix_score(name) >= ACCEPTANCE_THRESHOLD
    }

    fn allow_name(&self, name: &str) -> bool {
        self.filter.name_score(name) >= ACCEPTANCE_THRESHOLD
    }

    fn allow_full(&self, name: &str) -> bool {
        self.filter.full_score(name) >= ACCEPTANCE_THRESHOLD
    }
}

fn collect(engine: &QueryEngine, files: &Files, context: &Context) -> Vec<CompletionItem> {
    let mut items = vec![];

    if matches!(context.location, CompletionLocation::Module) {
        collect_module(engine, files, context, &mut items);
    } else if matches!(context.location, CompletionLocation::Term | CompletionLocation::Type) {
        collect_existing(engine, context, &mut items);
        collect_suggestions(engine, files, context, &mut items);
    }

    items
}

fn collect_module(
    engine: &QueryEngine,
    files: &Files,
    context: &Context,
    items: &mut Vec<CompletionItem>,
) {
    items.extend(files.iter_id().filter_map(|id| {
        let (parsed, _) = engine.parsed(id).ok()?;
        let name = parsed.module_name()?;

        // Limit suggestions to exact prefix matches before fuzzy matching.
        if let Some(p) = &context.filter.prefix {
            if !name.starts_with(p.as_str()) {
                return None;
            }
        } else if let Some(n) = &context.filter.name {
            if !name.starts_with(n.as_str()) {
                return None;
            }
        }

        if !context.allow_full(&name) {
            return None;
        }

        let description = Some(name.to_string());
        Some(completion_item(
            &name,
            &name,
            CompletionItemKind::MODULE,
            description,
            context.filter.range,
            CompletionResolveData::Import(id),
        ))
    }));
}

fn collect_existing(engine: &QueryEngine, context: &Context, items: &mut Vec<CompletionItem>) {
    if let Some(prefix) = &context.filter.prefix {
        collect_qualified(engine, context, items, prefix);
    } else {
        collect_unqualified(engine, context, items);
    }
}

fn collect_qualified(
    engine: &QueryEngine,
    context: &Context<'_>,
    items: &mut Vec<CompletionItem>,
    prefix: &str,
) {
    let module_name = prefix.trim_end_matches('.');

    if let Some(import) = context.resolved.qualified.get(module_name) {
        collect_imports(engine, context, import, items);
    }

    items.extend(context.resolved.qualified.iter().filter_map(|(import_name, import)| {
        if !import_name.starts_with(prefix) {
            return None;
        }

        let (parsed, _) = engine.parsed(import.file).ok()?;
        let description = parsed.module_name().map(|name| name.to_string());

        Some(completion_item(
            import_name,
            import_name,
            CompletionItemKind::MODULE,
            description,
            context.filter.range,
            CompletionResolveData::Import(import.file),
        ))
    }));
}

fn collect_unqualified(engine: &QueryEngine, context: &Context, items: &mut Vec<CompletionItem>) {
    items.extend(context.resolved.qualified.iter().filter_map(|(import_name, import)| {
        if !context.allow_name(import_name) {
            return None;
        }

        let (parsed, _) = engine.parsed(import.file).ok()?;
        let description = parsed.module_name().map(|name| name.to_string());

        Some(completion_item(
            import_name,
            import_name,
            CompletionItemKind::MODULE,
            description,
            context.filter.range,
            CompletionResolveData::Import(import.file),
        ))
    }));

    if matches!(context.location, CompletionLocation::Term) {
        items.extend(context.resolved.locals.iter_terms().filter_map(
            |(term_name, term_file_id, term_item_id)| {
                if !context.allow_name(term_name) {
                    return None;
                }
                let description = Some("Local".to_string());
                Some(completion_item(
                    term_name,
                    term_name,
                    CompletionItemKind::VALUE,
                    description,
                    context.filter.range,
                    CompletionResolveData::TermItem(term_file_id, term_item_id),
                ))
            },
        ));
    } else if matches!(context.location, CompletionLocation::Type) {
        items.extend(context.resolved.locals.iter_types().filter_map(
            |(type_name, type_file_id, type_item_id)| {
                if !context.allow_name(type_name) {
                    return None;
                }
                let description = Some("Local".to_string());
                Some(completion_item(
                    type_name,
                    type_name,
                    CompletionItemKind::STRUCT,
                    description,
                    context.filter.range,
                    CompletionResolveData::TypeItem(type_file_id, type_item_id),
                ))
            },
        ));
    }

    let prim_id = engine.prim_id();

    let mut has_explicit_prim = false;
    for import in context.resolved.unqualified.values().flatten() {
        has_explicit_prim = import.file == prim_id;
        collect_imports(engine, context, import, items);
    }

    if !has_explicit_prim {
        collect_implicit_prim(engine, context, items, prim_id);
    }
}

fn collect_implicit_prim(
    engine: &QueryEngine,
    context: &Context<'_>,
    items: &mut Vec<CompletionItem>,
    prim_id: FileId,
) {
    let Ok(prim_resolved) = engine.resolved(prim_id) else {
        return tracing::error!("Failed to resolve Prim@{prim_id:?}!");
    };

    if let CompletionLocation::Term = &context.location {
        items.extend(prim_resolved.exports.iter_terms().filter_map(
            |(term_name, term_file_id, term_item_id)| {
                if !context.allow_name(term_name) {
                    return None;
                }
                let description = Some("Prim".to_string());
                Some(completion_item(
                    term_name,
                    term_name,
                    CompletionItemKind::VALUE,
                    description,
                    context.filter.range,
                    CompletionResolveData::TermItem(term_file_id, term_item_id),
                ))
            },
        ));
    } else if let CompletionLocation::Type = &context.location {
        items.extend(prim_resolved.exports.iter_types().filter_map(
            |(type_name, type_file_id, type_item_id)| {
                if !context.allow_name(type_name) {
                    return None;
                }
                let description = Some("Prim".to_string());
                Some(completion_item(
                    type_name,
                    type_name,
                    CompletionItemKind::STRUCT,
                    description,
                    context.filter.range,
                    CompletionResolveData::TypeItem(type_file_id, type_item_id),
                ))
            },
        ));
    }
}

fn collect_suggestions(
    engine: &QueryEngine,
    files: &Files,
    context: &Context,
    items: &mut Vec<CompletionItem>,
) {
    let prim = engine.prim_id();
    if let Some(prefix) = &context.filter.prefix {
        for id in files.iter_id() {
            if id == context.id || id == prim {
                continue;
            }
            collect_qualified_suggestions(engine, context, items, (prefix, id));
        }
    } else if let Some(name) = &context.filter.name {
        for id in files.iter_id() {
            if id == context.id || id == prim {
                continue;
            }
            collect_unqualified_suggestions(engine, context, items, (name, id));
        }
    }
}

fn collect_qualified_suggestions(
    engine: &QueryEngine,
    context: &Context,
    items: &mut Vec<CompletionItem>,
    (prefix, id): (&str, FileId),
) {
    let Ok((import_parsed, _)) = engine.parsed(id) else {
        return;
    };
    let Some(module_name) = import_parsed.module_name() else {
        return;
    };

    let clean_prefix = prefix.trim_end_matches(".");
    if context.resolved.qualified.contains_key(clean_prefix) {
        return;
    }

    if !context.allow_prefix(&module_name) {
        return;
    }

    let Ok(import_resolved) = engine.resolved(id) else {
        return;
    };
    let insert_import_range = context.insert_import_range();

    if matches!(context.location, CompletionLocation::Term) {
        items.extend(import_resolved.exports.iter_terms().filter_map(
            |(term_name, term_file_id, term_item_id)| {
                if !context.allow_name(term_name) {
                    return None;
                }

                let edit = format!("{prefix}{term_name}");
                let description = Some(module_name.to_string());
                let mut completion_item = completion_item(
                    term_name,
                    edit,
                    CompletionItemKind::VALUE,
                    description,
                    context.filter.range,
                    CompletionResolveData::TermItem(term_file_id, term_item_id),
                );

                if let Some(label_details) = completion_item.label_details.as_mut() {
                    label_details.detail =
                        Some(format!(" (import {module_name} as {clean_prefix})"));
                }

                completion_item.sort_text = Some(module_name.to_string());
                completion_item.additional_text_edits = insert_import_range.map(|range| {
                    vec![TextEdit {
                        range,
                        new_text: format!("import {module_name} as {clean_prefix}\n"),
                    }]
                });

                Some(completion_item)
            },
        ));
    } else if matches!(context.location, CompletionLocation::Type) {
        items.extend(import_resolved.exports.iter_types().filter_map(
            |(type_name, type_file_id, type_item_id)| {
                if !context.allow_name(type_name) {
                    return None;
                }

                let edit = format!("{prefix}{type_name}");
                let description = Some(module_name.to_string());
                let mut completion_item = completion_item(
                    type_name,
                    edit,
                    CompletionItemKind::STRUCT,
                    description,
                    context.filter.range,
                    CompletionResolveData::TypeItem(type_file_id, type_item_id),
                );

                if let Some(label_details) = completion_item.label_details.as_mut() {
                    label_details.detail =
                        Some(format!(" (import {module_name} as {clean_prefix})"));
                }

                completion_item.sort_text = Some(module_name.to_string());
                completion_item.additional_text_edits = insert_import_range.map(|range| {
                    vec![TextEdit {
                        range,
                        new_text: format!("import {module_name} as {clean_prefix}\n"),
                    }]
                });

                Some(completion_item)
            },
        ))
    }
}

fn collect_unqualified_suggestions(
    engine: &QueryEngine,
    context: &Context,
    items: &mut Vec<CompletionItem>,
    (name, file_id): (&str, FileId),
) {
    let Ok((import_parsed, _)) = engine.parsed(file_id) else {
        return;
    };
    let Some(import_module_name) = import_parsed.module_name() else {
        return tracing::error!("Missing module name {:?}", file_id);
    };

    let Ok(import_resolved) = engine.resolved(file_id) else {
        return;
    };
    let insert_import_range = context.insert_import_range();

    if matches!(context.location, CompletionLocation::Term) {
        items.extend(import_resolved.exports.iter_terms().filter_map(
            |(term_name, term_file_id, term_item_id)| {
                if !term_name.starts_with(name) {
                    return None;
                }

                if term_file_id != file_id {
                    return None;
                }

                let description = import_parsed.module_name().map(|name| name.to_string());
                let mut completion_item = completion_item(
                    term_name,
                    term_name,
                    CompletionItemKind::VALUE,
                    description,
                    context.filter.range,
                    CompletionResolveData::TermItem(term_file_id, term_item_id),
                );

                let (import_text, import_range) = edit::term_import_item(
                    engine,
                    context,
                    &import_module_name,
                    term_name,
                    term_file_id,
                    term_item_id,
                );

                let text_edit = import_range.or(insert_import_range).zip(import_text);

                if let Some(label_details) = completion_item.label_details.as_mut() {
                    label_details.detail = Some(format!(" (import {import_module_name})"));
                }

                completion_item.sort_text = Some(import_module_name.to_string());
                completion_item.additional_text_edits =
                    text_edit.map(|(range, new_text)| vec![TextEdit { range, new_text }]);

                Some(completion_item)
            },
        ));
    } else if matches!(context.location, CompletionLocation::Type) {
        items.extend(import_resolved.exports.iter_types().filter_map(
            |(type_name, type_file_id, type_item_id)| {
                if !type_name.starts_with(name) {
                    return None;
                }
                if type_file_id != file_id {
                    return None;
                }

                let description = import_parsed.module_name().map(|name| name.to_string());
                let mut completion_item = completion_item(
                    type_name,
                    type_name,
                    CompletionItemKind::STRUCT,
                    description,
                    context.filter.range,
                    CompletionResolveData::TypeItem(type_file_id, type_item_id),
                );

                let (import_text, import_range) = edit::type_import_item(
                    engine,
                    context,
                    &import_module_name,
                    type_name,
                    type_file_id,
                    type_item_id,
                );

                let text_edit = import_range.or(insert_import_range).zip(import_text);

                if let Some(label_details) = completion_item.label_details.as_mut() {
                    label_details.detail = Some(format!(" (import {import_module_name})"));
                }

                completion_item.sort_text = Some(import_module_name.to_string());
                completion_item.additional_text_edits =
                    text_edit.map(|(range, new_text)| vec![TextEdit { range, new_text }]);

                Some(completion_item)
            },
        ));
    }
}

fn collect_imports(
    engine: &QueryEngine,
    context: &Context,
    import: &ResolvedImport,
    items: &mut Vec<CompletionItem>,
) {
    if matches!(context.location, CompletionLocation::Term) {
        items.extend(import.iter_terms().filter_map(
            |(term_name, term_file_id, term_item_id, term_kind)| {
                if matches!(term_kind, ImportKind::Hidden) {
                    return None;
                }
                if !context.allow_name(term_name) {
                    return None;
                }

                let (parsed, _) = engine.parsed(term_file_id).ok()?;
                let description = parsed.module_name().map(|name| name.to_string());

                let edit = if let Some(prefix) = &context.filter.prefix {
                    format!("{prefix}{term_name}")
                } else {
                    format!("{term_name}")
                };

                Some(completion_item(
                    term_name,
                    edit,
                    CompletionItemKind::VALUE,
                    description,
                    context.filter.range,
                    CompletionResolveData::TermItem(term_file_id, term_item_id),
                ))
            },
        ));
    } else if matches!(context.location, CompletionLocation::Type) {
        items.extend(import.iter_types().filter_map(
            |(type_name, type_file_id, type_item_id, type_kind)| {
                if matches!(type_kind, ImportKind::Hidden) {
                    return None;
                }

                if !context.allow_name(type_name) {
                    return None;
                }

                let (parsed, _) = engine.parsed(type_file_id).ok()?;
                let description = parsed.module_name().map(|name| name.to_string());

                let edit = if let Some(prefix) = &context.filter.prefix {
                    format!("{prefix}{type_name}")
                } else {
                    format!("{type_name}")
                };

                Some(completion_item(
                    type_name,
                    edit,
                    CompletionItemKind::STRUCT,
                    description,
                    context.filter.range,
                    CompletionResolveData::TypeItem(type_file_id, type_item_id),
                ))
            },
        ));
    }
}

fn completion_item(
    name: impl ToString,
    edit: impl ToString,
    kind: CompletionItemKind,
    description: Option<String>,
    range: Option<Range>,
    data: CompletionResolveData,
) -> CompletionItem {
    let data = serde_json::to_value(data).ok();
    CompletionItem {
        label: name.to_string(),
        label_details: Some(CompletionItemLabelDetails { detail: None, description }),
        kind: Some(kind),
        text_edit: range.map(|range| {
            let new_text = edit.to_string();
            CompletionTextEdit::Edit(TextEdit { range, new_text })
        }),
        data,
        ..Default::default()
    }
}
