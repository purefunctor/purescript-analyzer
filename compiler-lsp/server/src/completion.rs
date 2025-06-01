use async_lsp::lsp_types::*;
use indexing::ImportKind;
use resolving::FullResolvedModule;
use rowan::{TokenAtOffset, ast::AstNode};
use smol_str::SmolStr;
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
    let resolved = state.runtime.resolved(id);

    let offset = locate::position_to_offset(&content, position)?;

    let node = parsed.syntax_node();
    let token = node.token_at_offset(offset);

    let token = match token {
        TokenAtOffset::None => return None,
        TokenAtOffset::Single(token) => token,
        TokenAtOffset::Between(token, _) => token,
    };

    #[derive(Debug)]
    enum Context {
        Term,
        Type,
    }

    let _context = token.parent_ancestors().find_map(|node| {
        let kind = node.kind();
        if cst::Expression::can_cast(kind) {
            Some(Context::Term)
        } else if cst::Type::can_cast(kind) {
            Some(Context::Type)
        } else {
            None
        }
    });

    let mut items = vec![];

    let completion_prefix = qualified_prefix(&token).or_else(|| qualifier_prefix(&token));

    if let Some(CompletionPrefix { prefix, name }) = completion_prefix {
        match (prefix, name) {
            (Some(prefix), Some(name)) => {
                collect_prefixed_name(&mut items, &resolved, &prefix, &name);
            }
            (None, Some(name)) => {
                collect_name_or_prefix(&mut items, &resolved, &name);
            }
            (Some(prefix), None) => {
                collect_prefixed_name(&mut items, &resolved, &prefix, "");
            }
            (None, None) => (),
        }
    }

    let is_incomplete = items.len() > 5;
    Some(CompletionResponse::List(CompletionList { is_incomplete, items }))
}

fn collect_prefixed_name(
    items: &mut Vec<CompletionItem>,
    resolved: &FullResolvedModule,
    prefix: &str,
    name: &str,
) {
    let prefix = prefix.trim_end_matches('.');
    let mut has_exact_match = false;

    if let Some(import) = resolved.qualified.get(prefix) {
        has_exact_match = true;
        items.extend(import.iter_terms().filter_map(|(k, _, _, kind)| {
            if !matches!(kind, ImportKind::Hidden) && k.starts_with(name) {
                Some(completion_item(&k, CompletionItemKind::VALUE))
            } else {
                None
            }
        }));
        items.extend(import.iter_types().filter_map(|(k, _, _, kind)| {
            if !matches!(kind, ImportKind::Hidden) && k.starts_with(name) {
                Some(completion_item(&k, CompletionItemKind::STRUCT))
            } else {
                None
            }
        }));
    }

    items.extend(resolved.qualified.keys().filter_map(|import| {
        if has_exact_match && import == prefix {
            return None;
        }
        if import.starts_with(prefix) {
            Some(completion_item(&import, CompletionItemKind::MODULE))
        } else {
            None
        }
    }));
}

fn collect_name_or_prefix(
    items: &mut Vec<CompletionItem>,
    resolved: &FullResolvedModule,
    name: &str,
) {
    items.extend(resolved.qualified.keys().filter_map(|import| {
        if import.starts_with(name) {
            Some(completion_item(&import, CompletionItemKind::MODULE))
        } else {
            None
        }
    }));
    items.extend(
        resolved
            .locals
            .iter_terms()
            .map(|(k, _, _)| completion_item(&k, CompletionItemKind::VALUE)),
    );
    items.extend(
        resolved
            .locals
            .iter_types()
            .map(|(k, _, _)| completion_item(&k, CompletionItemKind::STRUCT)),
    );
    items.extend(resolved.unqualified.iter().flat_map(|import| {
        let terms = import.iter_terms().filter_map(|(k, _, _, kind)| {
            if !matches!(kind, ImportKind::Hidden) && k.starts_with(name) {
                Some(completion_item(&k, CompletionItemKind::VALUE))
            } else {
                None
            }
        });
        let types = import.iter_types().filter_map(|(k, _, _, kind)| {
            if !matches!(kind, ImportKind::Hidden) && k.starts_with(name) {
                Some(completion_item(&k, CompletionItemKind::STRUCT))
            } else {
                None
            }
        });
        terms.chain(types)
    }));
}

fn completion_item(name: &str, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: name.to_string(),
        label_details: Some(CompletionItemLabelDetails {
            detail: None,
            description: Some(name.to_string()),
        }),
        kind: Some(kind),
        ..Default::default()
    }
}

fn qualified_prefix(token: &SyntaxToken) -> Option<CompletionPrefix> {
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

        Some(CompletionPrefix { prefix, name })
    })
}

fn qualifier_prefix(token: &SyntaxToken) -> Option<CompletionPrefix> {
    token.parent_ancestors().find_map(|node| {
        let qualifier = cst::Qualifier::cast(node)?;
        let token = qualifier.text()?;

        let prefix = token.text();
        let prefix = SmolStr::new(prefix);

        let prefix = Some(prefix);
        let name = None;

        Some(CompletionPrefix { prefix, name })
    })
}

#[derive(Debug)]
struct CompletionPrefix {
    prefix: Option<SmolStr>,
    name: Option<SmolStr>,
}
