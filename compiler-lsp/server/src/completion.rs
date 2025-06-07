use std::{iter::Chain, mem, str::Chars};

use async_lsp::lsp_types::*;
use files::FileId;
use indexing::{ImportKind, TermItemId, TypeItemId};
use resolving::{FullResolvedModule, ResolvedImport};
use rowan::{TextRange, TokenAtOffset, ast::AstNode};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use strsim::{generic_jaro_winkler, jaro_winkler};
use syntax::{SyntaxKind, SyntaxNode, SyntaxToken, cst};

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

    let filter = CompletionFilter::try_qualified(&token)
        .or_else(|| CompletionFilter::try_qualifier(&token))
        .or_else(|| CompletionFilter::try_module_name(&token))
        .unwrap_or_default();

    let context = CompletionContext::new(&token);

    let content = state.runtime.content(id);
    let resolved = state.runtime.resolved(id);

    let _span = tracing::info_span!("completion_items").entered();
    let items = collect(state, filter, context, &content, &resolved);
    let is_incomplete = items.len() > 5;

    Some(CompletionResponse::List(CompletionList { is_incomplete, items }))
}

const ACCEPTANCE_THRESHOLD: f64 = 0.5;

fn collect(
    state: &mut State,
    filter: CompletionFilter,
    context: CompletionContext,
    content: &str,
    resolved: &FullResolvedModule,
) -> Vec<CompletionItem> {
    let mut items = vec![];

    let start = locate::offset_to_position(&content, filter.range.start());
    let end = locate::offset_to_position(&content, filter.range.end());
    let range = Range { start, end };

    if let Some(prefix) = filter.prefix.as_deref() {
        // Flag that determines if we found an exact match for this module.
        // If we have, we make sure to exclude it from the completion list.
        let mut module_match_found = false;
        let module = prefix.trim_end_matches('.');

        if let Some(import) = resolved.qualified.get(module) {
            module_match_found = true;
            collect_imports(state, &mut items, &filter, import, range);
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
                CompletionItemKind::MODULE,
                description,
                range,
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
                CompletionItemKind::MODULE,
                description,
                range,
                CompletionResolveData::Import(import.file),
            ))
        }));

        items.extend(resolved.locals.iter_terms().filter_map(|(name, f_id, t_id)| {
            if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            Some(completion_item(
                name,
                CompletionItemKind::VALUE,
                Some("Local".to_string()),
                range,
                CompletionResolveData::TermItem(f_id, t_id),
            ))
        }));

        items.extend(resolved.locals.iter_types().filter_map(|(name, f_id, t_id)| {
            if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            Some(completion_item(
                name,
                CompletionItemKind::STRUCT,
                Some("Local".to_string()),
                range,
                CompletionResolveData::TypeItem(f_id, t_id),
            ))
        }));

        for import in &resolved.unqualified {
            collect_imports(state, &mut items, &filter, import, range);
        }
    }

    if matches!(context, CompletionContext::Module) {
        collect_module(state, filter, &mut items, range);
    }

    items
}

fn collect_module(
    state: &mut State,
    filter: CompletionFilter,
    items: &mut Vec<CompletionItem>,
    range: Range,
) {
    for id in state.files.iter_id() {
        let (parsed, _) = state.runtime.parsed(id);
        let Some(name) = parsed.module_name() else {
            continue;
        };

        // Limit suggestions to exact prefix matches before fuzzy matching.
        if let Some(p) = &filter.prefix {
            if !name.starts_with(p.as_str()) {
                continue;
            }
        } else if let Some(n) = &filter.name {
            if !name.starts_with(n.as_str()) {
                continue;
            }
        }

        if filter.full_score(&name) < ACCEPTANCE_THRESHOLD {
            continue;
        }

        let description = Some(name.to_string());
        items.push(completion_item(
            &name,
            CompletionItemKind::MODULE,
            description,
            range,
            CompletionResolveData::Import(id),
        ));
    }
}

fn collect_imports(
    state: &mut State,
    items: &mut Vec<CompletionItem>,
    filter: &CompletionFilter,
    import: &ResolvedImport,
    range: Range,
) {
    items.extend(import.iter_terms().filter_map(|(name, f_id, t_id, kind)| {
        if matches!(kind, ImportKind::Hidden) {
            return None;
        }
        if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
            return None;
        }
        let (parsed, _) = state.runtime.parsed(f_id);
        let description = parsed.module_name().map(|name| name.to_string());
        Some(completion_item(
            name,
            CompletionItemKind::VALUE,
            description,
            range,
            CompletionResolveData::TermItem(f_id, t_id),
        ))
    }));
    items.extend(import.iter_types().filter_map(|(name, f_id, t_id, kind)| {
        if matches!(kind, ImportKind::Hidden) {
            return None;
        }
        if filter.name_score(name) < ACCEPTANCE_THRESHOLD {
            return None;
        }
        let (parsed, _) = state.runtime.parsed(f_id);
        let description = parsed.module_name().map(|name| name.to_string());
        Some(completion_item(
            name,
            CompletionItemKind::STRUCT,
            description,
            range,
            CompletionResolveData::TypeItem(f_id, t_id),
        ))
    }));
}

fn completion_item(
    name: &str,
    kind: CompletionItemKind,
    description: Option<String>,
    range: Range,
    data: CompletionResolveData,
) -> CompletionItem {
    let data = serde_json::to_value(data).ok();
    CompletionItem {
        label: name.to_string(),
        label_details: Some(CompletionItemLabelDetails { detail: None, description }),
        kind: Some(kind),
        text_edit: Some(CompletionTextEdit::Edit(TextEdit { range, new_text: name.to_string() })),
        data,
        ..Default::default()
    }
}

#[derive(Debug, Default)]
struct CompletionFilter {
    prefix: Option<SmolStr>,
    name: Option<SmolStr>,
    range: TextRange,
}

impl CompletionFilter {
    fn try_qualified(token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let qualified = cst::QualifiedName::cast(node)?;

            let prefix = qualified.qualifier().and_then(|qualifier| {
                let token = qualifier.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });

            let name = qualified.lower().or_else(|| qualified.upper()).map(|token| {
                let text = token.text();
                SmolStr::from(text)
            });

            let start = qualified
                .syntax()
                .first_child_or_token_by_kind(&|kind| !matches!(kind, SyntaxKind::Annotation))?
                .text_range()
                .start();

            let end = qualified.syntax().last_child_or_token()?.text_range().end();

            let range = TextRange::new(start, end);

            Some(CompletionFilter { prefix, name, range })
        })
    }

    fn try_qualifier(token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let qualifier = cst::Qualifier::cast(node)?;
            let token = qualifier.text()?;

            let prefix = token.text();
            let prefix = SmolStr::new(prefix);

            let prefix = Some(prefix);
            let name = None;

            let start = qualifier
                .syntax()
                .first_child_or_token_by_kind(&|kind| !matches!(kind, SyntaxKind::Annotation))?
                .text_range()
                .start();

            let end = qualifier.syntax().last_child_or_token()?.text_range().end();

            let range = TextRange::new(start, end);

            Some(CompletionFilter { prefix, name, range })
        })
    }

    fn try_module_name(token: &SyntaxToken) -> Option<CompletionFilter> {
        token.parent_ancestors().find_map(|node| {
            let module_name = cst::ModuleName::cast(node)?;

            let prefix = module_name.qualifier().and_then(|qualifier| {
                let token = qualifier.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });

            let name = module_name.name_token().map(|token| {
                let text = token.text();
                SmolStr::from(text)
            });

            let start = module_name
                .syntax()
                .first_child_or_token_by_kind(&|kind| !matches!(kind, SyntaxKind::Annotation))?
                .text_range()
                .start();

            let end = module_name.syntax().last_child_or_token()?.text_range().end();

            let range = TextRange::new(start, end);

            Some(CompletionFilter { prefix, name, range })
        })
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
    fn new(token: &SyntaxToken) -> CompletionContext {
        token
            .parent_ancestors()
            .find_map(|node| {
                let kind = node.kind();
                if cst::Expression::can_cast(kind) {
                    Some(CompletionContext::Term)
                } else if cst::Type::can_cast(kind) {
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
