use std::mem;

use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::FileId;
use indexing::{TermItemId, TypeItemId};
use serde::{Deserialize, Serialize};
use syntax::SyntaxNode;

use crate::{
    AnalyzerError,
    extract::{AnnotationSyntaxRange, extract_annotation, extract_syntax},
};

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

    let root_range = match resolve {
        CompletionResolveData::Import(f_id) => AnnotationSyntaxRange::of_file(engine, f_id),
        CompletionResolveData::TermItem(f_id, t_id) => {
            AnnotationSyntaxRange::of_file_term(engine, f_id, t_id)
        }
        CompletionResolveData::TypeItem(f_id, t_id) => {
            AnnotationSyntaxRange::of_file_type(engine, f_id, t_id)
        }
    };

    match root_range {
        Ok((root, range)) => Ok(resolve_documentation(root, range, item)),
        Err(error) => Err((error, item)),
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
