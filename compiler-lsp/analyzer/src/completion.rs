mod edit;
mod filter;
mod item;
mod prelude;
mod sources;

pub mod resolve;

use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::Files;
use rowan::TokenAtOffset;
use smol_str::SmolStr;

use filter::{FuzzyMatch, NoFilter, StartsWith};
use prelude::{Context, CursorSemantics, CursorText, Source};
use sources::{
    ImportedTerms, ImportedTypes, LocalTerms, LocalTypes, PrimTerms, PrimTypes, QualifiedModules,
    QualifiedTerms, QualifiedTermsSuggestions, QualifiedTypes, QualifiedTypesSuggestions,
    SuggestedTerms, SuggestedTypes, WorkspaceModules,
};
use syntax::SyntaxKind;

use crate::locate;

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Option<CompletionResponse> {
    let uri = uri.as_str();

    let id = files.id(uri)?;
    let prim_id = engine.prim_id();
    let content = engine.content(id);
    let (parsed, _) = engine.parsed(id).ok()?;

    let offset = locate::position_to_offset(&content, position)?;

    let node = parsed.syntax_node();
    let token = node.token_at_offset(offset);

    let token = match token {
        TokenAtOffset::None => return None,
        TokenAtOffset::Single(token) => token,
        TokenAtOffset::Between(left, right) => {
            let left_annotation = left.parent_ancestors().any(|node| {
                let kind = node.kind();
                matches!(kind, SyntaxKind::Annotation)
            });
            if left_annotation { right } else { left }
        }
    };

    let semantics = CursorSemantics::new(&content, position);
    let (text, range) = CursorText::new(&content, &token);

    let indexed = engine.indexed(id).ok()?;
    let resolved = engine.resolved(id).ok()?;
    let prim_resolved = engine.resolved(prim_id).ok()?;

    let context = Context {
        engine,
        files,
        id,
        content: &content,
        indexed: &indexed,
        parsed: &parsed,
        resolved: &resolved,
        prim_id,
        prim_resolved: &prim_resolved,
        semantics,
        text,
        range,
    };

    let items = collect(&context);
    let is_incomplete = items.len() > 5;

    Some(CompletionResponse::List(CompletionList { is_incomplete, items }))
}

fn collect(context: &Context) -> Vec<CompletionItem> {
    let mut items = vec![];
    match &context.text {
        CursorText::None => {
            if context.collect_modules() {
                items.extend(WorkspaceModules.candidates(context, NoFilter));
            } else {
                items.extend(QualifiedModules.candidates(context, NoFilter));
            }
            if context.collect_terms() {
                items.extend(LocalTerms.candidates(context, NoFilter));
            }
            if context.collect_types() {
                items.extend(LocalTypes.candidates(context, NoFilter));
            }
        }
        CursorText::Prefix(p) => {
            let p = p.trim_end_matches('.');
            if context.collect_modules() {
                items.extend(WorkspaceModules.candidates(context, StartsWith(p)));
            } else {
                items.extend(QualifiedModules.candidates(context, StartsWith(p)));
            }
            if context.collect_terms() {
                items.extend(QualifiedTerms(p).candidates(context, NoFilter));
                if !context.has_qualified_import(p) {
                    items.extend(QualifiedTermsSuggestions(p).candidates(context, NoFilter));
                }
            }
            if context.collect_types() {
                items.extend(QualifiedTypes(p).candidates(context, NoFilter));
                if !context.has_qualified_import(p) {
                    items.extend(QualifiedTypesSuggestions(p).candidates(context, NoFilter));
                }
            }
        }
        CursorText::Name(n) => {
            if context.collect_modules() {
                items.extend(WorkspaceModules.candidates(context, StartsWith(n)));
            } else {
                items.extend(QualifiedModules.candidates(context, StartsWith(n)));
            }
            if context.collect_terms() {
                items.extend(LocalTerms.candidates(context, FuzzyMatch(n)));
                items.extend(ImportedTerms.candidates(context, FuzzyMatch(n)));
                items.extend(SuggestedTerms.candidates(context, StartsWith(n)));
                if context.collect_implicit_prim() {
                    items.extend(PrimTerms.candidates(context, FuzzyMatch(n)));
                }
            }
            if context.collect_types() {
                items.extend(LocalTypes.candidates(context, FuzzyMatch(n)));
                items.extend(ImportedTypes.candidates(context, FuzzyMatch(n)));
                items.extend(SuggestedTypes.candidates(context, StartsWith(n)));
                if context.collect_implicit_prim() {
                    items.extend(PrimTypes.candidates(context, FuzzyMatch(n)));
                }
            }
        }
        CursorText::Both(p, n) => {
            let t: SmolStr = p.chars().chain(n.chars()).collect();

            if context.collect_modules() {
                items.extend(WorkspaceModules.candidates(context, StartsWith(&t)));
            } else {
                items.extend(QualifiedModules.candidates(context, StartsWith(&t)));
            }

            let p = p.trim_end_matches('.');
            if context.collect_terms() {
                items.extend(QualifiedTerms(p).candidates(context, FuzzyMatch(n)));
                if !context.has_qualified_import(p) {
                    items.extend(QualifiedTermsSuggestions(p).candidates(context, FuzzyMatch(n)));
                }
            }
            if context.collect_types() {
                items.extend(QualifiedTypes(p).candidates(context, FuzzyMatch(n)));
                if !context.has_qualified_import(p) {
                    items.extend(QualifiedTypesSuggestions(p).candidates(context, FuzzyMatch(n)));
                }
            }
        }
    }
    items
}
