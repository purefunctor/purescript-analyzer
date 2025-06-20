pub(crate) mod context;
pub(crate) mod resolve;

use async_lsp::lsp_types::*;
use context::{CompletionFilter, CompletionLocation};
use files::FileId;
use indexing::{FullIndexedModule, ImportKind, TermItemKind, TypeItemKind};
use parsing::ParsedModule;
use resolve::CompletionResolveData;
use resolving::{FullResolvedModule, ResolvedImport};
use rowan::{TokenAtOffset, ast::AstNode};
use syntax::SyntaxNodePtr;

use crate::{State, locate};

pub(super) fn implementation(
    state: &mut State,
    uri: Url,
    position: Position,
) -> Option<CompletionResponse> {
    let uri = uri.as_str();

    let id = state.files.id(uri)?;
    let content = state.runtime.content(id);
    let (parsed, _) = state.runtime.parsed(id);

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

    let indexed = state.runtime.indexed(id);
    let resolved = state.runtime.resolved(id);

    let _span = tracing::info_span!("completion_items").entered();
    tracing::info!("Collecting {:?} items", location);

    let context = Context {
        content: &content,
        parsed: &parsed,
        indexed: &indexed,
        resolved: &resolved,
        location: &location,
        filter: &filter,
    };

    let items = collect(state, &context);
    let is_incomplete = items.len() > 5;

    Some(CompletionResponse::List(CompletionList { is_incomplete, items }))
}

struct Context<'a> {
    content: &'a str,
    parsed: &'a ParsedModule,
    indexed: &'a FullIndexedModule,
    resolved: &'a FullResolvedModule,
    location: &'a CompletionLocation,
    filter: &'a CompletionFilter,
}

impl Context<'_> {
    fn insert_import_range(&self) -> Option<Range> {
        let cst = self.parsed.cst().imports()?;
        let offset = cst.syntax().text_range().end();
        let mut position = locate::offset_to_position(self.content, offset);
        position.line += 1;
        position.character = 0;
        Some(Range::new(position, position))
    }
}

const ACCEPTANCE_THRESHOLD: f64 = 0.5;

fn collect(state: &mut State, context: &Context) -> Vec<CompletionItem> {
    let mut items = vec![];

    if matches!(context.location, CompletionLocation::Module) {
        collect_module(state, context, &mut items);
    } else if matches!(context.location, CompletionLocation::Term | CompletionLocation::Type) {
        collect_existing(state, context, &mut items);
        collect_suggestions(state, context, &mut items);
    }

    items
}

fn collect_module(state: &mut State, context: &Context, items: &mut Vec<CompletionItem>) {
    items.extend(state.files.iter_id().filter_map(|id| {
        let (parsed, _) = state.runtime.parsed(id);
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

        if context.filter.full_score(&name) < ACCEPTANCE_THRESHOLD {
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

fn collect_existing(state: &mut State, context: &Context, items: &mut Vec<CompletionItem>) {
    if let Some(prefix) = &context.filter.prefix {
        collect_qualified(state, context, items, prefix);
    } else {
        collect_unqualified(state, context, items);
    }
}

fn collect_qualified(
    state: &mut State,
    context: &Context<'_>,
    items: &mut Vec<CompletionItem>,
    prefix: &str,
) {
    let module_name = prefix.trim_end_matches('.');

    if let Some(import) = context.resolved.qualified.get(module_name) {
        collect_imports(state, context, import, items);
    }

    items.extend(context.resolved.qualified.iter().filter_map(|(import_name, import)| {
        if !import_name.starts_with(prefix) {
            return None;
        }

        let (parsed, _) = state.runtime.parsed(import.file);
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

fn collect_unqualified(state: &mut State, context: &Context, items: &mut Vec<CompletionItem>) {
    items.extend(context.resolved.qualified.iter().filter_map(|(import_name, import)| {
        if context.filter.name_score(import_name) < ACCEPTANCE_THRESHOLD {
            return None;
        }

        let (parsed, _) = state.runtime.parsed(import.file);
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
                if context.filter.name_score(term_name) < ACCEPTANCE_THRESHOLD {
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
                if context.filter.name_score(type_name) < ACCEPTANCE_THRESHOLD {
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

    for import in &context.resolved.unqualified {
        collect_imports(state, context, import, items);
    }
}

fn collect_suggestions(state: &mut State, context: &Context, items: &mut Vec<CompletionItem>) {
    for id in state.files.iter_id() {
        if let Some(prefix) = &context.filter.prefix {
            collect_qualified_suggestions(state, context, items, (prefix, id));
        } else if let Some(name) = &context.filter.name {
            collect_unqualified_suggestions(state, context, items, (name, id));
        }
    }
}

fn collect_qualified_suggestions(
    state: &mut State,
    context: &Context,
    items: &mut Vec<CompletionItem>,
    (prefix, id): (&str, FileId),
) {
    let (import_parsed, _) = state.runtime.parsed(id);
    let Some(module_name) = import_parsed.module_name() else {
        return;
    };

    let clean_prefix = prefix.trim_end_matches(".");
    if context.resolved.qualified.contains_key(clean_prefix) {
        return;
    }

    if context.filter.prefix_score(&module_name) < ACCEPTANCE_THRESHOLD {
        return;
    }

    let import_resolved = state.runtime.resolved(id);
    let insert_import_range = context.insert_import_range();

    if matches!(context.location, CompletionLocation::Term) {
        items.extend(import_resolved.exports.iter_terms().filter_map(
            |(term_name, term_file_id, term_item_id)| {
                if context.filter.name_score(term_name) < ACCEPTANCE_THRESHOLD {
                    return None;
                }
                let edit = format!("{}{}", prefix, term_name);
                let mut completion_item = completion_item(
                    term_name,
                    edit,
                    CompletionItemKind::VALUE,
                    Some(module_name.to_string()),
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
                if context.filter.name_score(type_name) < ACCEPTANCE_THRESHOLD {
                    return None;
                }
                let edit = format!("{}{}", prefix, type_name);
                let mut completion_item = completion_item(
                    type_name,
                    edit,
                    CompletionItemKind::STRUCT,
                    Some(module_name.to_string()),
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
    state: &mut State,
    context: &Context,
    items: &mut Vec<CompletionItem>,
    (name, file_id): (&str, FileId),
) {
    if matches!(context.location, CompletionLocation::Term) {
        let (import_parsed, _) = state.runtime.parsed(file_id);
        let import_resolved = state.runtime.resolved(file_id);
        let import_module_name = import_parsed.module_name().unwrap();
        let insert_import_range = context.insert_import_range();

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

                if let Some(import) = context.resolved.unqualified.iter().find(|import| {
                    let Some((xf_id, xt_id, _)) = import.lookup_term(term_name) else {
                        return false;
                    };
                    (xf_id, xt_id) == (term_file_id, term_item_id)
                }) {
                    let import = &context.indexed.source[import.id];
                    let root = context.parsed.syntax_node();
                    let import = import.to_node(&root);

                    let mut buffer = String::default();
                    if let Some(import_list) = import.import_list() {
                        buffer.push('(');
                        let import_indexed = state.runtime.indexed(term_file_id);
                        let term_item = &import_indexed.items[term_item_id];
                        let item_name =
                            if matches!(term_item.kind, TermItemKind::Constructor { .. }) {
                                let type_item_id = import_indexed
                                    .pairs
                                    .constructor_type(term_item_id)
                                    .expect("invariant violated: unpaired data constructor");
                                let type_item = &import_indexed.items[type_item_id];
                                if let Some(name) = &type_item.name {
                                    format!("{name}(..)")
                                } else {
                                    return None;
                                }
                            } else {
                                format!("{term_name}")
                            };
                        buffer.push_str(&item_name);
                        for child in import_list.children() {
                            let text = child.syntax().text().to_string();
                            if text.trim().contains(&item_name) {
                                continue;
                            }
                            buffer.push_str(", ");
                            buffer.push_str(&text);
                        }
                        buffer.push(')');
                    }

                    let range = {
                        let node = import.syntax();
                        let ptr = SyntaxNodePtr::new(node);
                        let text_range = locate::text_range_after_annotation(&ptr, &root);
                        text_range.map(|range| locate::text_range_to_range(context.content, range))
                    };

                    if let Some(label_details) = completion_item.label_details.as_mut() {
                        label_details.detail = Some(format!(" (import {import_module_name})"));
                    }
                    completion_item.sort_text = Some(import_module_name.to_string());
                    completion_item.additional_text_edits = range.map(|range| {
                        vec![TextEdit {
                            range,
                            new_text: format!("import {import_module_name} {buffer}"),
                        }]
                    });
                } else {
                    if let Some(label_details) = completion_item.label_details.as_mut() {
                        label_details.detail = Some(format!(" (import {import_module_name})"));
                    }

                    let import_indexed = state.runtime.indexed(term_file_id);
                    let term_item = &import_indexed.items[term_item_id];
                    completion_item.sort_text = Some(import_module_name.to_string());
                    completion_item.additional_text_edits = insert_import_range.and_then(|range| {
                        let item_name =
                            if matches!(term_item.kind, TermItemKind::Constructor { .. }) {
                                let type_item_id = import_indexed
                                    .pairs
                                    .constructor_type(term_item_id)
                                    .expect("invariant violated: unpaired data constructor");
                                let type_item = &import_indexed.items[type_item_id];
                                if let Some(name) = &type_item.name {
                                    format!("{name}(..)")
                                } else {
                                    return None;
                                }
                            } else {
                                format!("{term_name}")
                            };
                        Some(vec![TextEdit {
                            range,
                            new_text: format!("import {import_module_name} ({item_name})\n"),
                        }])
                    });
                }

                Some(completion_item)
            },
        ));
    } else if matches!(context.location, CompletionLocation::Type) {
        let (import_parsed, _) = state.runtime.parsed(file_id);
        let import_resolved = state.runtime.resolved(file_id);
        let import_module_name = import_parsed.module_name().unwrap();
        let insert_import_range = context.insert_import_range();

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

                if let Some(import) = context.resolved.unqualified.iter().find(|import| {
                    let Some((xf_id, xt_id, _)) = import.lookup_type(type_name) else {
                        return false;
                    };
                    (xf_id, xt_id) == (type_file_id, type_item_id)
                }) {
                    let import = &context.indexed.source[import.id];
                    let root = context.parsed.syntax_node();
                    let import = import.to_node(&root);

                    let mut buffer = String::default();
                    if let Some(import_list) = import.import_list() {
                        buffer.push('(');
                        let import_indexed = state.runtime.indexed(type_file_id);
                        let type_item = &import_indexed.items[type_item_id];
                        let import_item = if matches!(type_item.kind, TypeItemKind::Class { .. }) {
                            format!("class {type_name}")
                        } else {
                            format!("{type_name}")
                        };
                        buffer.push_str(&import_item);
                        for child in import_list.children() {
                            let text = child.syntax().text().to_string();
                            if text.trim().contains(type_name.as_str()) {
                                continue;
                            }
                            buffer.push_str(", ");
                            buffer.push_str(&text);
                        }
                        buffer.push(')');
                    }

                    let range = {
                        let node = import.syntax();
                        let ptr = SyntaxNodePtr::new(node);
                        let text_range = locate::text_range_after_annotation(&ptr, &root);
                        text_range.map(|range| locate::text_range_to_range(context.content, range))
                    };

                    if let Some(label_details) = completion_item.label_details.as_mut() {
                        label_details.detail = Some(format!(" (import {import_module_name})"));
                    }
                    completion_item.sort_text = Some(import_module_name.to_string());
                    completion_item.additional_text_edits = range.map(|range| {
                        vec![TextEdit {
                            range,
                            new_text: format!("import {import_module_name} {buffer}"),
                        }]
                    });
                } else {
                    if let Some(label_details) = completion_item.label_details.as_mut() {
                        label_details.detail = Some(format!(" (import {import_module_name})"));
                    }

                    let import_indexed = state.runtime.indexed(type_file_id);
                    let type_item = &import_indexed.items[type_item_id];
                    completion_item.sort_text = Some(import_module_name.to_string());
                    completion_item.additional_text_edits = insert_import_range.map(|range| {
                        let import_item = if matches!(type_item.kind, TypeItemKind::Class { .. }) {
                            format!("class {type_name}")
                        } else {
                            format!("{type_name}")
                        };
                        vec![TextEdit {
                            range,
                            new_text: format!("import {import_module_name} ({import_item})\n"),
                        }]
                    });
                }

                Some(completion_item)
            },
        ));
    }
}

fn collect_imports(
    state: &mut State,
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
                if context.filter.name_score(term_name) < ACCEPTANCE_THRESHOLD {
                    return None;
                }
                let (parsed, _) = state.runtime.parsed(term_file_id);
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
                if context.filter.name_score(type_name) < ACCEPTANCE_THRESHOLD {
                    return None;
                }
                let (parsed, _) = state.runtime.parsed(type_file_id);
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
