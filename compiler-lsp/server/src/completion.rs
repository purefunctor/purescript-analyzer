pub(crate) mod context;
pub(crate) mod edit;
pub mod resolve;

use async_lsp::lsp_types::*;
use context::{CompletionFilter, CompletionLocation};
use files::FileId;
use indexing::{FullIndexedModule, ImportKind};
use parsing::ParsedModule;
use resolve::CompletionResolveData;
use resolving::{FullResolvedModule, ResolvedImport};
use rowan::{TokenAtOffset, ast::AstNode};

use crate::{Compiler, locate};

pub fn implementation(
    compiler: &mut Compiler,
    uri: Url,
    position: Position,
) -> Option<CompletionResponse> {
    let uri = uri.as_str();

    let id = compiler.files.id(uri)?;
    let content = compiler.runtime.content(id);
    let (parsed, _) = compiler.runtime.parsed(id);

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

    let indexed = compiler.runtime.indexed(id);
    let resolved = compiler.runtime.resolved(id);

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

    let items = collect(compiler, &context);
    let is_incomplete = items.len() > 5;

    Some(CompletionResponse::List(CompletionList { is_incomplete, items }))
}

const ACCEPTANCE_THRESHOLD: f64 = 0.5;

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

fn collect(compiler: &mut Compiler, context: &Context) -> Vec<CompletionItem> {
    let mut items = vec![];

    if matches!(context.location, CompletionLocation::Module) {
        collect_module(compiler, context, &mut items);
    } else if matches!(context.location, CompletionLocation::Term | CompletionLocation::Type) {
        collect_existing(compiler, context, &mut items);
        collect_suggestions(compiler, context, &mut items);
    }

    items
}

fn collect_module(compiler: &mut Compiler, context: &Context, items: &mut Vec<CompletionItem>) {
    items.extend(compiler.files.iter_id().filter_map(|id| {
        let (parsed, _) = compiler.runtime.parsed(id);
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

        if context.allow_full(&name) {
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

fn collect_existing(compiler: &mut Compiler, context: &Context, items: &mut Vec<CompletionItem>) {
    if let Some(prefix) = &context.filter.prefix {
        collect_qualified(compiler, context, items, prefix);
    } else {
        collect_unqualified(compiler, context, items);
    }
}

fn collect_qualified(
    compiler: &mut Compiler,
    context: &Context<'_>,
    items: &mut Vec<CompletionItem>,
    prefix: &str,
) {
    let module_name = prefix.trim_end_matches('.');

    if let Some(import) = context.resolved.qualified.get(module_name) {
        collect_imports(compiler, context, import, items);
    }

    items.extend(context.resolved.qualified.iter().filter_map(|(import_name, import)| {
        if !import_name.starts_with(prefix) {
            return None;
        }

        let (parsed, _) = compiler.runtime.parsed(import.file);
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

fn collect_unqualified(
    compiler: &mut Compiler,
    context: &Context,
    items: &mut Vec<CompletionItem>,
) {
    items.extend(context.resolved.qualified.iter().filter_map(|(import_name, import)| {
        if !context.allow_name(import_name) {
            return None;
        }

        let (parsed, _) = compiler.runtime.parsed(import.file);
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

    for import in &context.resolved.unqualified {
        collect_imports(compiler, context, import, items);
    }
}

fn collect_suggestions(
    compiler: &mut Compiler,
    context: &Context,
    items: &mut Vec<CompletionItem>,
) {
    if let Some(prefix) = &context.filter.prefix {
        for id in compiler.files.iter_id() {
            collect_qualified_suggestions(compiler, context, items, (prefix, id));
        }
    } else if let Some(name) = &context.filter.name {
        for id in compiler.files.iter_id() {
            collect_unqualified_suggestions(compiler, context, items, (name, id));
        }
    }
}

fn collect_qualified_suggestions(
    compiler: &mut Compiler,
    context: &Context,
    items: &mut Vec<CompletionItem>,
    (prefix, id): (&str, FileId),
) {
    let (import_parsed, _) = compiler.runtime.parsed(id);
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

    let import_resolved = compiler.runtime.resolved(id);
    let insert_import_range = context.insert_import_range();

    if matches!(context.location, CompletionLocation::Term) {
        items.extend(import_resolved.exports.iter_terms().filter_map(
            |(term_name, term_file_id, term_item_id)| {
                if !context.allow_name(term_name) {
                    return None;
                }

                let edit = format!("{}{}", prefix, term_name);
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

                let edit = format!("{}{}", prefix, type_name);
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
    compiler: &mut Compiler,
    context: &Context,
    items: &mut Vec<CompletionItem>,
    (name, file_id): (&str, FileId),
) {
    let (import_parsed, _) = compiler.runtime.parsed(file_id);
    let Some(import_module_name) = import_parsed.module_name() else {
        return tracing::error!("Missing module name {:?}", file_id);
    };

    let import_resolved = compiler.runtime.resolved(file_id);
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
                    compiler,
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
                    compiler,
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
    compiler: &mut Compiler,
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

                let (parsed, _) = compiler.runtime.parsed(term_file_id);
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

                let (parsed, _) = compiler.runtime.parsed(type_file_id);
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
