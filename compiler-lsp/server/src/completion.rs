use std::{iter::Chain, mem, str::Chars};

use async_lsp::lsp_types::*;
use files::FileId;
use indexing::{ImportKind, TermItemId, TypeItemId};
use parsing::ParsedModule;
use resolving::{FullResolvedModule, ResolvedImport};
use rowan::{TextRange, TokenAtOffset, ast::AstNode};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use strsim::{generic_jaro_winkler, jaro_winkler};
use syntax::{SyntaxNode, SyntaxToken, cst};

use crate::{State, hover, locate};

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

    let filter = CompletionFilter::try_qualified(&content, &token)
        .or_else(|| CompletionFilter::try_qualifier(&content, &token))
        .or_else(|| CompletionFilter::try_module_name(&content, &token))
        .unwrap_or_default();

    let context = CompletionContext::new(&content, position);
    let resolved = state.runtime.resolved(id);

    let _span = tracing::info_span!("completion_items").entered();
    tracing::info!("Collecting {:?} items", context);

    let items = collect(state, &parsed, &content, &filter, &context, &resolved);
    let is_incomplete = items.len() > 5;

    Some(CompletionResponse::List(CompletionList { is_incomplete, items }))
}

const ACCEPTANCE_THRESHOLD: f64 = 0.5;

fn collect(
    state: &mut State,
    parsed: &ParsedModule,
    content: &str,
    filter: &CompletionFilter,
    context: &CompletionContext,
    resolved: &FullResolvedModule,
) -> Vec<CompletionItem> {
    let mut items = vec![];

    if let Some(prefix) = filter.prefix.as_deref() {
        // Flag that determines if we found an exact match for this module.
        // If we have, we make sure to exclude it from the completion list.
        let mut module_match_found = false;
        let module = prefix.trim_end_matches('.');

        if let Some(import) = resolved.qualified.get(module) {
            module_match_found = true;
            collect_imports(state, &mut items, filter, context, import);
        }

        items.extend(resolved.qualified.iter().filter_map(|(name, import)| {
            if module_match_found && name == module {
                return None;
            }
            if !name.starts_with(prefix) {
                return None;
            }
            let (parsed, _) = state.runtime.parsed(import.file);
            let description = parsed.module_name().map(|name| name.to_string());
            Some(completion_item(
                name,
                name,
                CompletionItemKind::MODULE,
                description,
                filter.range,
                CompletionResolveData::Import(import.file),
            ))
        }));
    } else {
        items.extend(resolved.qualified.iter().filter_map(|(name, import)| {
            if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            let (parsed, _) = state.runtime.parsed(import.file);
            let description = parsed.module_name().map(|name| name.to_string());
            Some(completion_item(
                name,
                name,
                CompletionItemKind::MODULE,
                description,
                filter.range,
                CompletionResolveData::Import(import.file),
            ))
        }));

        if matches!(context, CompletionContext::Term) {
            items.extend(resolved.locals.iter_terms().filter_map(|(name, f_id, t_id)| {
                if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                    return None;
                }
                let description = Some("Local".to_string());
                Some(completion_item(
                    name,
                    name,
                    CompletionItemKind::VALUE,
                    description,
                    filter.range,
                    CompletionResolveData::TermItem(f_id, t_id),
                ))
            }));
        }

        if matches!(context, CompletionContext::Type) {
            items.extend(resolved.locals.iter_types().filter_map(|(name, f_id, t_id)| {
                if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                    return None;
                }
                let description = Some("Local".to_string());
                Some(completion_item(
                    name,
                    name,
                    CompletionItemKind::STRUCT,
                    description,
                    filter.range,
                    CompletionResolveData::TypeItem(f_id, t_id),
                ))
            }));
        }

        for import in &resolved.unqualified {
            collect_imports(state, &mut items, filter, context, import);
        }
    }

    if matches!(context, CompletionContext::Module) {
        collect_module(state, filter, &mut items);
    } else {
        collect_suggestions(state, (content, parsed), (filter, context), &mut items);
    }

    items
}

fn collect_module(state: &mut State, filter: &CompletionFilter, items: &mut Vec<CompletionItem>) {
    items.extend(state.files.iter_id().filter_map(|id| {
        let (parsed, _) = state.runtime.parsed(id);
        let name = parsed.module_name()?;

        // Limit suggestions to exact prefix matches before fuzzy matching.
        if let Some(p) = &filter.prefix {
            if !name.starts_with(p.as_str()) {
                return None;
            }
        } else if let Some(n) = &filter.name {
            if !name.starts_with(n.as_str()) {
                return None;
            }
        }

        if filter.full_score(&name) < ACCEPTANCE_THRESHOLD {
            return None;
        }

        let description = Some(name.to_string());
        Some(completion_item(
            &name,
            &name,
            CompletionItemKind::MODULE,
            description,
            filter.range,
            CompletionResolveData::Import(id),
        ))
    }));
}

fn collect_suggestions(
    state: &mut State,
    (content, parsed): (&str, &ParsedModule),
    (filter, context): (&CompletionFilter, &CompletionContext),
    items: &mut Vec<CompletionItem>,
) {
    for id in state.files.iter_id() {
        if let Some(prefix) = &filter.prefix {
            collect_qualified_suggestions(
                state,
                items,
                (content, parsed),
                (filter, context),
                (prefix, id),
            );
        } else if let Some(name) = &filter.name {
            collect_unqualified_suggestions(
                state,
                items,
                (content, parsed),
                (filter, context),
                (name, id),
            );
        }
    }
}

fn collect_qualified_suggestions(
    state: &mut State,
    items: &mut Vec<CompletionItem>,
    (content, parsed): (&str, &ParsedModule),
    (filter, context): (&CompletionFilter, &CompletionContext),
    (prefix, id): (&str, FileId),
) {
    let (import_parsed, _) = state.runtime.parsed(id);
    let Some(module_name) = import_parsed.module_name() else {
        return;
    };

    let clean_prefix = prefix.trim_end_matches(".");
    if !module_name.contains(clean_prefix) {
        return;
    };

    if filter.prefix_score(&module_name) < ACCEPTANCE_THRESHOLD {
        return;
    }

    let range = parsed.cst().imports().map(|cst| {
        let offset = cst.syntax().text_range().end();
        let mut position = locate::offset_to_position(content, offset);
        position.line += 1;
        position.character = 0;
        Range::new(position, position)
    });

    let import_resolved = state.runtime.resolved(id);
    if matches!(context, CompletionContext::Term) {
        items.extend(import_resolved.exports.iter_terms().filter_map(|(name, f_id, t_id)| {
            if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            let edit = format!("{}{}", prefix, name);
            let mut item = completion_item(
                name,
                edit,
                CompletionItemKind::VALUE,
                Some(format!("import {}", module_name)),
                filter.range,
                CompletionResolveData::TermItem(f_id, t_id),
            );
            item.additional_text_edits = range.map(|range| {
                vec![TextEdit {
                    range,
                    new_text: format!(
                        "import {} as {}\n",
                        module_name,
                        prefix.trim_end_matches(".")
                    ),
                }]
            });
            Some(item)
        }));
    } else if matches!(context, CompletionContext::Type) {
        items.extend(import_resolved.exports.iter_types().filter_map(|(name, f_id, t_id)| {
            if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            let edit = format!("{}{}", prefix, name);
            let mut item = completion_item(
                name,
                edit,
                CompletionItemKind::STRUCT,
                Some(format!("import {}", module_name)),
                filter.range,
                CompletionResolveData::TypeItem(f_id, t_id),
            );
            item.additional_text_edits = range.map(|range| {
                vec![TextEdit {
                    range,
                    new_text: format!(
                        "import {} as {}\n",
                        module_name,
                        prefix.trim_end_matches(".")
                    ),
                }]
            });
            Some(item)
        }))
    }
}

fn collect_unqualified_suggestions(
    state: &mut State,
    items: &mut Vec<CompletionItem>,
    (_, _): (&str, &ParsedModule),
    (filter, context): (&CompletionFilter, &CompletionContext),
    (name, id): (&str, FileId),
) {
    let (import_parsed, _) = state.runtime.parsed(id);
    let import_resolved = state.runtime.resolved(id);
    if matches!(context, CompletionContext::Term) {
        items.extend(import_resolved.exports.iter_terms().filter_map(|(import, f_id, t_id)| {
            if !import.starts_with(name) {
                return None;
            }
            let description = import_parsed.module_name().map(|name| name.to_string());
            Some(completion_item(
                import,
                import,
                CompletionItemKind::VALUE,
                description,
                filter.range,
                CompletionResolveData::TermItem(f_id, t_id),
            ))
        }));
    } else if matches!(context, CompletionContext::Type) {
        items.extend(import_resolved.exports.iter_types().filter_map(|(import, f_id, t_id)| {
            if !import.starts_with(name) {
                return None;
            }
            let description = import_parsed.module_name().map(|name| name.to_string());
            Some(completion_item(
                import,
                import,
                CompletionItemKind::STRUCT,
                description,
                filter.range,
                CompletionResolveData::TypeItem(f_id, t_id),
            ))
        }));
    }
}

fn collect_imports(
    state: &mut State,
    items: &mut Vec<CompletionItem>,
    filter: &CompletionFilter,
    context: &CompletionContext,
    import: &ResolvedImport,
) {
    if matches!(context, CompletionContext::Term) {
        items.extend(import.iter_terms().filter_map(|(name, f_id, t_id, kind)| {
            if matches!(kind, ImportKind::Hidden) {
                return None;
            }
            if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            let (parsed, _) = state.runtime.parsed(f_id);
            let description = parsed.module_name().map(|name| name.to_string());
            let edit = if let Some(prefix) = &filter.prefix {
                format!("{prefix}{name}")
            } else {
                format!("{name}")
            };
            Some(completion_item(
                name,
                edit,
                CompletionItemKind::VALUE,
                description,
                filter.range,
                CompletionResolveData::TermItem(f_id, t_id),
            ))
        }));
    }
    if matches!(context, CompletionContext::Type) {
        items.extend(import.iter_types().filter_map(|(name, f_id, t_id, kind)| {
            if matches!(kind, ImportKind::Hidden) {
                return None;
            }
            if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            let (parsed, _) = state.runtime.parsed(f_id);
            let description = parsed.module_name().map(|name| name.to_string());
            let edit = if let Some(prefix) = &filter.prefix {
                format!("{prefix}{name}")
            } else {
                format!("{name}")
            };
            Some(completion_item(
                name,
                edit,
                CompletionItemKind::STRUCT,
                description,
                filter.range,
                CompletionResolveData::TypeItem(f_id, t_id),
            ))
        }));
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

#[derive(Debug, Default)]
struct CompletionFilter {
    prefix: Option<SmolStr>,
    name: Option<SmolStr>,
    range: Option<Range>,
}

impl CompletionFilter {
    fn try_qualified(content: &str, token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let qualified = cst::QualifiedName::cast(node)?;

            let prefix_token = qualified.qualifier().and_then(|qualifier| qualifier.text());
            let prefix_range = prefix_token.as_ref().map(|token| token.text_range());
            let prefix = prefix_token.map(|token| token.text().into());

            let name_token = qualified.lower().or_else(|| qualified.upper());
            let name_range = name_token.as_ref().map(|token| token.text_range());
            let name = name_token.map(|token| token.text().into());

            let range = match (prefix_range, name_range) {
                (Some(p), Some(n)) => Some(p.cover(n)),
                (Some(r), None) => Some(r),
                (None, Some(r)) => Some(r),
                (None, None) => None,
            };

            let range = range.map(|range| locate::text_range_to_range(content, range));
            Some(CompletionFilter { prefix, name, range })
        })
    }

    fn try_qualifier(content: &str, token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let qualifier = cst::Qualifier::cast(node)?;
            let token = qualifier.text()?;

            let prefix = token.text();
            let prefix = SmolStr::new(prefix);

            let range = token.text_range();
            let range = locate::text_range_to_range(content, range);

            let prefix = Some(prefix);
            let name = None;
            let range = Some(range);

            Some(CompletionFilter { prefix, name, range })
        })
    }

    fn try_module_name(content: &str, token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let module_name = cst::ModuleName::cast(node)?;

            let prefix_token = module_name.qualifier().and_then(|qualifier| qualifier.text());
            let prefix_range = prefix_token.as_ref().map(|token| token.text_range());
            let prefix = prefix_token.map(|token| token.text().into());

            let name_token = module_name.name_token();
            let name_range = name_token.as_ref().map(|token| token.text_range());
            let name = name_token.map(|token| token.text().into());

            let range = match (prefix_range, name_range) {
                (Some(p), Some(n)) => Some(p.cover(n)),
                (Some(r), None) => Some(r),
                (None, Some(r)) => Some(r),
                (None, None) => None,
            };

            let range = range.map(|range| locate::text_range_to_range(content, range));
            Some(CompletionFilter { prefix, name, range })
        })
    }

    fn prefix_score(&self, other: &str) -> f64 {
        if let Some(prefix) = &self.prefix { jaro_winkler(prefix, other) } else { 1.0 }
    }

    fn name_score(&self, other: &str) -> f64 {
        if let Some(name) = &self.name { jaro_winkler(name, other) } else { 1.0 }
    }

    fn full_score(&self, other: &str) -> f64 {
        struct Full<'a> {
            prefix: &'a str,
            name: &'a str,
        }

        struct Other<'a> {
            other: &'a str,
        }

        impl<'b> IntoIterator for &Full<'b> {
            type Item = char;
            type IntoIter = Chain<Chars<'b>, Chars<'b>>;

            fn into_iter(self) -> Self::IntoIter {
                let prefix = self.prefix.chars();
                let name = self.name.chars();
                prefix.chain(name)
            }
        }

        impl<'b> IntoIterator for &Other<'b> {
            type Item = char;
            type IntoIter = Chars<'b>;

            fn into_iter(self) -> Self::IntoIter {
                self.other.chars()
            }
        }

        match (&self.prefix, &self.name) {
            (Some(prefix), Some(name)) => {
                let full = Full { prefix, name };
                let other = Other { other };
                generic_jaro_winkler(&full, &other)
            }
            (Some(prefix), None) => jaro_winkler(prefix, other),
            (None, Some(name)) => jaro_winkler(name, other),
            (None, None) => 1.0,
        }
    }
}

#[derive(Debug)]
enum CompletionContext {
    Term,
    Type,
    Module,
    General,
}

impl CompletionContext {
    fn new(content: &str, position: Position) -> CompletionContext {
        // We insert a placeholder identifier at the current position of the
        // text cursor. This is done as an effort to produce as valid of a
        // parse tree as possible before we perform further analysis.
        //
        // This is particularly helpful for incomplete qualified names. Since
        // the parser represents qualifiers as "trivia" for the current token,
        // the following source string yields a lexing error:
        //
        // component = Halogen.
        //
        // Inserting a placeholder gets rid of this error, allowing the parser
        // to produce a valid parse tree that we can use for analysis:
        //
        // component = Halogen.z'PureScript'z

        let Some(offset) = locate::position_to_offset(content, position) else {
            return CompletionContext::General;
        };

        let (left, right) = content.split_at(offset.into());
        let source = format!("{left}z'PureScript'z{right}");

        let lexed = lexing::lex(&source);
        let tokens = lexing::layout(&lexed);
        let (parsed, _) = parsing::parse(&lexed, &tokens);

        let node = parsed.syntax_node();
        let token = node.token_at_offset(offset);

        let token = match token {
            TokenAtOffset::None => {
                return CompletionContext::General;
            }
            TokenAtOffset::Single(token) => token,
            TokenAtOffset::Between(token, _) => token,
        };

        token
            .parent_ancestors()
            .find_map(|node| {
                let kind = node.kind();
                if cst::Expression::can_cast(kind) {
                    Some(CompletionContext::Term)
                } else if cst::Type::can_cast(kind) || cst::ExpressionTypeArgument::can_cast(kind) {
                    Some(CompletionContext::Type)
                } else if cst::ImportStatement::can_cast(kind) {
                    Some(CompletionContext::Module)
                } else {
                    None
                }
            })
            .unwrap_or(CompletionContext::General)
    }
}

pub(super) fn resolve_item(state: &mut State, mut item: CompletionItem) -> CompletionItem {
    let Some(value) = mem::take(&mut item.data) else { return item };
    let Ok(resolve) = serde_json::from_value::<CompletionResolveData>(value) else { return item };

    match resolve {
        CompletionResolveData::Import(f_id) => {
            if let Some(ranges) = hover::annotation_syntax_file(state, f_id) {
                resolve_documentation(ranges, &mut item);
            }
        }
        CompletionResolveData::TermItem(f_id, t_id) => {
            if let Some(ranges) = hover::annotation_syntax_file_term(state, f_id, t_id) {
                resolve_documentation(ranges, &mut item);
            }
        }
        CompletionResolveData::TypeItem(f_id, t_id) => {
            if let Some(ranges) = hover::annotation_syntax_file_type(state, f_id, t_id) {
                resolve_documentation(ranges, &mut item);
            }
        }
    }

    item
}

fn resolve_documentation(
    (root, annotation, syntax): (SyntaxNode, Option<TextRange>, Option<TextRange>),
    item: &mut CompletionItem,
) {
    let annotation = annotation.map(|range| hover::render_annotation_string(&root, range));
    let syntax = syntax.map(|range| hover::render_syntax_string(&root, range));

    item.detail = syntax;
    item.documentation = annotation.map(|annotation| {
        Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: annotation,
        })
    })
}

#[derive(Serialize, Deserialize)]
enum CompletionResolveData {
    Import(#[serde(with = "id")] FileId),
    TermItem(#[serde(with = "id")] FileId, #[serde(with = "id")] TermItemId),
    TypeItem(#[serde(with = "id")] FileId, #[serde(with = "id")] TypeItemId),
}

mod id {
    use la_arena::{Idx, RawIdx};
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub(super) fn serialize<T, S>(index: &Idx<T>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        index.into_raw().into_u32().serialize(serializer)
    }

    pub(super) fn deserialize<'d, T, D>(deserializer: D) -> Result<Idx<T>, D::Error>
    where
        D: Deserializer<'d>,
    {
        let value = u32::deserialize(deserializer)?;
        Ok(Idx::from_raw(RawIdx::from_u32(value)))
    }
}
