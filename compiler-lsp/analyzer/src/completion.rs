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
use prelude::{CompletionSource, Context, CursorSemantics, CursorText};
use sources::{
    ImportedTerms, ImportedTypes, LocalTerms, LocalTypes, PrimTerms, PrimTypes, QualifiedModules,
    QualifiedTerms, QualifiedTermsSuggestions, QualifiedTypes, QualifiedTypesSuggestions,
    SuggestedTerms, SuggestedTypes, WorkspaceModules,
};
use syntax::SyntaxKind;

use crate::{AnalyzerError, locate};

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Result<Option<CompletionResponse>, AnalyzerError> {
    let current_file = {
        let uri = uri.as_str();
        files.id(uri).ok_or(AnalyzerError::NonFatal)?
    };

    let prim_id = engine.prim_id();
    let content = engine.content(current_file);
    let (parsed, _) = engine.parsed(current_file)?;

    let offset = locate::position_to_offset(&content, position).ok_or(AnalyzerError::NonFatal)?;

    let node = parsed.syntax_node();
    let token = node.token_at_offset(offset);

    let token = match token {
        TokenAtOffset::None => return Ok(None),
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

    let indexed = engine.indexed(current_file)?;
    let resolved = engine.resolved(current_file)?;
    let prim_resolved = engine.resolved(prim_id)?;

    let context = Context {
        engine,
        files,
        current_file,
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

    let items = collect(&context)?;
    let is_incomplete = items.len() > 5;

    Ok(Some(CompletionResponse::List(CompletionList { is_incomplete, items })))
}

fn collect(context: &Context) -> Result<Vec<CompletionItem>, AnalyzerError> {
    let mut items = vec![];
    let into = &mut items;

    match &context.text {
        CursorText::None => {
            if context.collect_modules() {
                WorkspaceModules.collect_into(context, NoFilter, into)?;
            } else {
                QualifiedModules.collect_into(context, NoFilter, into)?;
            }
            if context.collect_terms() {
                LocalTerms.collect_into(context, NoFilter, into)?;
            }
            if context.collect_types() {
                LocalTypes.collect_into(context, NoFilter, into)?;
            }
        }
        CursorText::Prefix(p) => {
            let p = p.trim_end_matches('.');
            if context.collect_modules() {
                WorkspaceModules.collect_into(context, StartsWith(p), into)?;
            } else {
                QualifiedModules.collect_into(context, StartsWith(p), into)?;
            }
            if context.collect_terms() {
                QualifiedTerms(p).collect_into(context, NoFilter, into)?;
                if !context.has_qualified_import(p) {
                    QualifiedTermsSuggestions(p).collect_into(context, NoFilter, into)?;
                }
            }
            if context.collect_types() {
                QualifiedTypes(p).collect_into(context, NoFilter, into)?;
                if !context.has_qualified_import(p) {
                    QualifiedTypesSuggestions(p).collect_into(context, NoFilter, into)?;
                }
            }
        }
        CursorText::Name(n) => {
            if context.collect_modules() {
                WorkspaceModules.collect_into(context, StartsWith(n), into)?;
            } else {
                QualifiedModules.collect_into(context, StartsWith(n), into)?;
            }
            if context.collect_terms() {
                LocalTerms.collect_into(context, FuzzyMatch(n), into)?;
                ImportedTerms.collect_into(context, FuzzyMatch(n), into)?;
                SuggestedTerms.collect_into(context, StartsWith(n), into)?;
                if context.collect_implicit_prim() {
                    PrimTerms.collect_into(context, FuzzyMatch(n), into)?;
                }
            }
            if context.collect_types() {
                LocalTypes.collect_into(context, FuzzyMatch(n), into)?;
                ImportedTypes.collect_into(context, FuzzyMatch(n), into)?;
                SuggestedTypes.collect_into(context, StartsWith(n), into)?;
                if context.collect_implicit_prim() {
                    PrimTypes.collect_into(context, FuzzyMatch(n), into)?;
                }
            }
        }
        CursorText::Both(p, n) => {
            let t: SmolStr = p.chars().chain(n.chars()).collect();

            if context.collect_modules() {
                WorkspaceModules.collect_into(context, StartsWith(&t), into)?;
            } else {
                QualifiedModules.collect_into(context, StartsWith(&t), into)?;
            }

            let p = p.trim_end_matches('.');
            if context.collect_terms() {
                QualifiedTerms(p).collect_into(context, FuzzyMatch(n), into)?;
                if !context.has_qualified_import(p) {
                    QualifiedTermsSuggestions(p).collect_into(context, FuzzyMatch(n), into)?;
                }
            }
            if context.collect_types() {
                QualifiedTypes(p).collect_into(context, FuzzyMatch(n), into)?;
                if !context.has_qualified_import(p) {
                    QualifiedTypesSuggestions(p).collect_into(context, FuzzyMatch(n), into)?;
                }
            }
        }
    }

    Ok(items)
}
