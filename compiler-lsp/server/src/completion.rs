use async_lsp::lsp_types::*;
use indexing::ImportKind;
use resolving::{FullResolvedModule, ResolvedImport};
use rowan::{TokenAtOffset, ast::AstNode};
use smol_str::SmolStr;
use strsim::jaro_winkler;
use syntax::{SyntaxToken, cst};

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

    let filter = CompletionFilter::try_qualified(&token)
        .or_else(|| CompletionFilter::try_qualifier(&token))?;

    let resolved = state.runtime.resolved(id);

    let items = collect(state, filter, &resolved);
    let is_incomplete = items.len() > 5;

    Some(CompletionResponse::List(CompletionList { is_incomplete, items }))
}

const ACCEPTANCE_THRESHOLD: f64 = 0.5;

fn collect(
    state: &mut State,
    filter: CompletionFilter,
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
            collect_imports(state, &mut items, &filter, import);
        }

        items.extend(resolved.qualified.iter().filter_map(|(name, import)| {
            if module_match_found && name == module {
                return None;
            }
            if !name.starts_with(module) {
                return None;
            }
            let (parsed, _) = state.runtime.parsed(import.file);
            let description = parsed.cst().header().and_then(|cst| {
                let cst = cst.name()?;
                let mut string = String::default();
                if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
                    string.push_str(token.text());
                }
                if let Some(token) = cst.name_token() {
                    string.push_str(token.text());
                }
                Some(string)
            });
            Some(completion_item(&name, CompletionItemKind::MODULE, description))
        }));
    } else {
        items.extend(resolved.qualified.keys().filter_map(|import| {
            if filter.name_score(import) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            Some(completion_item(&import, CompletionItemKind::MODULE, None))
        }));

        items.extend(resolved.locals.iter_terms().filter_map(|(name, _, _)| {
            if filter.name_score(&name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            Some(completion_item(&name, CompletionItemKind::VALUE, Some("Local".to_string())))
        }));

        items.extend(resolved.locals.iter_types().filter_map(|(name, _, _)| {
            if filter.name_score(&name) < ACCEPTANCE_THRESHOLD {
                return None;
            }
            Some(completion_item(&name, CompletionItemKind::STRUCT, Some("Local".to_string())))
        }));

        for import in &resolved.unqualified {
            collect_imports(state, &mut items, &filter, import);
        }
    }

    items
}

fn collect_imports(
    state: &mut State,
    items: &mut Vec<CompletionItem>,
    filter: &CompletionFilter,
    import: &ResolvedImport,
) {
    items.extend(import.iter_terms().filter_map(|(name, id, _, kind)| {
        if matches!(kind, ImportKind::Hidden) {
            return None;
        }
        if filter.name_score(&name) < ACCEPTANCE_THRESHOLD {
            return None;
        }
        let (parsed, _) = state.runtime.parsed(id);
        let description = parsed.cst().header().and_then(|cst| {
            let cst = cst.name()?;
            let mut string = String::default();
            if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
                string.push_str(token.text());
            }
            if let Some(token) = cst.name_token() {
                string.push_str(token.text());
            }
            Some(string)
        });
        Some(completion_item(&name, CompletionItemKind::VALUE, description))
    }));
    items.extend(import.iter_types().filter_map(|(name, id, _, kind)| {
        if matches!(kind, ImportKind::Hidden) {
            return None;
        }
        if filter.name_score(&name) < ACCEPTANCE_THRESHOLD {
            return None;
        }
        let (parsed, _) = state.runtime.parsed(id);
        let description = parsed.cst().header().and_then(|cst| {
            let cst = cst.name()?;
            let mut string = String::default();
            if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
                string.push_str(token.text());
            }
            if let Some(token) = cst.name_token() {
                string.push_str(token.text());
            }
            Some(string)
        });
        Some(completion_item(&name, CompletionItemKind::STRUCT, description))
    }));
}

fn completion_item(
    name: &str,
    kind: CompletionItemKind,
    description: Option<String>,
) -> CompletionItem {
    CompletionItem {
        label: name.to_string(),
        label_details: Some(CompletionItemLabelDetails { detail: None, description }),
        kind: Some(kind),
        ..Default::default()
    }
}

#[derive(Debug)]
struct CompletionFilter {
    prefix: Option<SmolStr>,
    name: Option<SmolStr>,
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

            Some(CompletionFilter { prefix, name })
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

            Some(CompletionFilter { prefix, name })
        })
    }

    fn name_score(&self, other: &str) -> f64 {
        if let Some(name) = &self.name { jaro_winkler(name, other) } else { 1.0 }
    }
}
