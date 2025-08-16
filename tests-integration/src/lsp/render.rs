use async_lsp::lsp_types::{CompletionItem, TextEdit};
use itertools::Itertools;
use tabled::{Tabled, derive::display};

#[derive(Tabled)]
pub struct TabledCompletionItem {
    label: String,
    #[tabled(display("display::option", " ..."))]
    label_detail: Option<String>,
    #[tabled(display("display::option", "..."))]
    label_description: Option<String>,
    #[tabled(display("display_text_edits"))]
    additional_text_edits: Option<Vec<TextEdit>>,
}

impl From<CompletionItem> for TabledCompletionItem {
    fn from(value: CompletionItem) -> TabledCompletionItem {
        let label = value.label;
        let (label_detail, label_description) =
            value.label_details.map(|value| (value.detail, value.description)).unwrap_or_default();
        let additional_text_edits = value.additional_text_edits;
        TabledCompletionItem { label, label_detail, label_description, additional_text_edits }
    }
}

fn display_text_edits(edits: &Option<Vec<TextEdit>>) -> String {
    if let Some(edits) = edits {
        edits
            .iter()
            .map(|edit| {
                format!(
                    "{}:{}..{}:{}\n{}",
                    edit.range.start.line,
                    edit.range.start.character,
                    edit.range.end.line,
                    edit.range.end.character,
                    edit.new_text.trim()
                )
            })
            .join("\n")
    } else {
        "...".into()
    }
}
