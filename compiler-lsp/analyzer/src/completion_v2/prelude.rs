use async_lsp::lsp_types::*;
use building::QueryEngine;
use resolving::FullResolvedModule;

pub struct Context<'a> {
    pub resolved: &'a FullResolvedModule,
}

/// A trait for describing completion sources.
pub trait Source {
    type Filter: Filter;

    fn candidates(
        engine: &QueryEngine,
        context: &Context,
        filter: Self::Filter,
    ) -> impl Iterator<Item = CompletionItem>;
}

/// A trait for describing completion filters.
pub trait Filter {
    fn matches(&self, name: &str) -> bool;
}

pub fn completion_item(
    name: impl ToString,
    edit: impl ToString,
    kind: CompletionItemKind,
    description: Option<String>,
    range: Option<Range>,
) -> CompletionItem {
    CompletionItem {
        label: name.to_string(),
        label_details: Some(CompletionItemLabelDetails { detail: None, description }),
        kind: Some(kind),
        text_edit: range.map(|range| {
            let new_text = edit.to_string();
            CompletionTextEdit::Edit(TextEdit { range, new_text })
        }),
        ..Default::default()
    }
}
