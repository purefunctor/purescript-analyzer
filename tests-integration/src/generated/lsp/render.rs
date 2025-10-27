use async_lsp::lsp_types::{CompletionItem, CompletionTextEdit, TextEdit};
use itertools::Itertools;
use tabled::Tabled;
use tabled::derive::display;

#[derive(Tabled)]
pub struct TabledCompletionItem {
    label: String,
    #[tabled(display("display::option", " ..."))]
    label_detail: Option<String>,
    #[tabled(display("display::option", "..."))]
    label_description: Option<String>,
    #[tabled(display("display::option", "..."))]
    sort_text: Option<String>,
    #[tabled(display("display::option", "..."))]
    filter_text: Option<String>,
    #[tabled(display("display_completion_text_edit"))]
    text_edit: Option<CompletionTextEdit>,
    #[tabled(display("display_text_edits"))]
    additional_text_edits: Option<Vec<TextEdit>>,
}

impl From<CompletionItem> for TabledCompletionItem {
    fn from(value: CompletionItem) -> TabledCompletionItem {
        let label = value.label;
        let (label_detail, label_description) =
            value.label_details.map(|value| (value.detail, value.description)).unwrap_or_default();
        let sort_text = value.sort_text;
        let filter_text = value.filter_text;
        let text_edit = value.text_edit;
        let additional_text_edits = value.additional_text_edits;
        TabledCompletionItem {
            label,
            label_detail,
            label_description,
            sort_text,
            filter_text,
            text_edit,
            additional_text_edits,
        }
    }
}

fn display_completion_text_edit(edit: &Option<CompletionTextEdit>) -> String {
    if let Some(edit) = edit {
        match edit {
            CompletionTextEdit::Edit(edit) => {
                format!(
                    "{}:{}..{}:{}\n{}",
                    edit.range.start.line,
                    edit.range.start.character,
                    edit.range.end.line,
                    edit.range.end.character,
                    edit.new_text.trim()
                )
            }
            CompletionTextEdit::InsertAndReplace(_) => {
                unimplemented!("InsertAndReplace");
            }
        }
    } else {
        "...".into()
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
