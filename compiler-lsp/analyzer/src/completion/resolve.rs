use std::mem;

use async_lsp::lsp_types::*;
use building::QueryEngine;
use checking::core::pretty::Pretty;
use files::FileId;
use indexing::{TermItemId, TypeItemId};
use serde::{Deserialize, Serialize};
use syntax::SyntaxNode;

use crate::AnalyzerError;
use crate::extract::{AnnotationSyntaxRange, extract_annotation, extract_syntax};

#[allow(clippy::result_large_err)]
pub fn implementation(
    engine: &QueryEngine,
    mut item: CompletionItem,
) -> Result<CompletionItem, (AnalyzerError, CompletionItem)> {
    let Some(value) = mem::take(&mut item.data) else {
        return Ok(item);
    };

    let Ok(resolve) = serde_json::from_value::<CompletionResolveData>(value) else {
        return Ok(item);
    };

    match resolve {
        CompletionResolveData::Import(file_id) => {
            match AnnotationSyntaxRange::of_file(engine, file_id) {
                Ok((root, range)) => Ok(resolve_documentation(root, range, item)),
                Err(error) => Err((error, item)),
            }
        }
        CompletionResolveData::TermItem(file_id, term_id) => {
            resolve_term_item(engine, file_id, term_id, item)
        }
        CompletionResolveData::TypeItem(file_id, type_id) => {
            resolve_type_item(engine, file_id, type_id, item)
        }
    }
}

fn resolve_documentation(
    root: SyntaxNode,
    range: AnnotationSyntaxRange,
    mut item: CompletionItem,
) -> CompletionItem {
    let annotation = range.annotation.map(|range| extract_annotation(&root, range));
    let syntax = range.syntax.map(|range| extract_syntax(&root, range));

    item.detail = syntax;
    item.documentation = annotation.map(|annotation| {
        Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: annotation,
        })
    });

    item
}

fn resolve_term_item(
    engine: &QueryEngine,
    file_id: FileId,
    term_id: TermItemId,
    mut item: CompletionItem,
) -> Result<CompletionItem, (AnalyzerError, CompletionItem)> {
    if let Ok((root, range)) = AnnotationSyntaxRange::of_file_term(engine, file_id, term_id) {
        let annotation = range.annotation.map(|range| extract_annotation(&root, range));
        item.documentation = annotation.map(|annotation| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: annotation,
            })
        });
    }

    if let Some(signature) = render_term_signature(engine, file_id, term_id) {
        item.detail = Some(signature);
    }

    Ok(item)
}

fn render_term_signature(
    engine: &QueryEngine,
    file_id: FileId,
    term_id: TermItemId,
) -> Option<String> {
    let indexed = engine.indexed(file_id).ok()?;
    let checked = engine.checked(file_id).ok()?;

    let name = &indexed.items[term_id].name;
    let name = name.as_deref()?;
    let signature = checked.lookup_term(term_id)?;

    let pretty = Pretty::new(engine, &checked).width(80).signature(name);
    Some(pretty.render(signature).to_string())
}

fn resolve_type_item(
    engine: &QueryEngine,
    file_id: FileId,
    type_id: TypeItemId,
    mut item: CompletionItem,
) -> Result<CompletionItem, (AnalyzerError, CompletionItem)> {
    if let Ok((root, range)) = AnnotationSyntaxRange::of_file_type(engine, file_id, type_id) {
        let annotation = range.annotation.map(|range| extract_annotation(&root, range));
        item.documentation = annotation.map(|annotation| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: annotation,
            })
        });
    }

    if let Some(signature) = render_type_signature(engine, file_id, type_id) {
        item.detail = Some(signature);
    }

    Ok(item)
}

fn render_type_signature(
    engine: &QueryEngine,
    file_id: FileId,
    type_id: TypeItemId,
) -> Option<String> {
    let indexed = engine.indexed(file_id).ok()?;
    let checked = engine.checked(file_id).ok()?;

    let name = &indexed.items[type_id].name;
    let name = name.as_deref()?;
    let signature = checked.lookup_type(type_id)?;

    let pretty = Pretty::new(engine, &checked).width(80).signature(name);
    Some(pretty.render(signature).to_string())
}

#[derive(Serialize, Deserialize)]
pub(crate) enum CompletionResolveData {
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
