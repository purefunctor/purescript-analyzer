use std::mem;

use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::FileId;
use indexing::{TermItemId, TypeItemId};
use rowan::TextRange;
use serde::{Deserialize, Serialize};
use syntax::SyntaxNode;

use crate::{extract, hover};

pub fn implementation(engine: &QueryEngine, mut item: CompletionItem) -> CompletionItem {
    let Some(value) = mem::take(&mut item.data) else { return item };
    let Ok(resolve) = serde_json::from_value::<CompletionResolveData>(value) else { return item };

    match resolve {
        CompletionResolveData::Import(f_id) => {
            if let Some(ranges) = hover::annotation_syntax_file(engine, f_id) {
                resolve_documentation(ranges, &mut item);
            }
        }
        CompletionResolveData::TermItem(f_id, t_id) => {
            if let Some(ranges) = hover::annotation_syntax_file_term(engine, f_id, t_id) {
                resolve_documentation(ranges, &mut item);
            }
        }
        CompletionResolveData::TypeItem(f_id, t_id) => {
            if let Some(ranges) = hover::annotation_syntax_file_type(engine, f_id, t_id) {
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
    let annotation = annotation.map(|range| extract::extract_annotation(&root, range));
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
