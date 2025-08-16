mod render;

use std::fmt::Write;

use analyzer::QueryEngine;
use async_lsp::lsp_types::{
    CompletionList, CompletionResponse, GotoDefinitionResponse, HoverContents, LanguageString,
    Location, MarkedString, Position, Url,
};
use files::{FileId, Files};
use itertools::Itertools;
use line_index::{LineIndex, TextSize};
use render::TabledCompletionItem;
use tabled::{
    Table,
    settings::{Padding, Style},
};

#[derive(Debug, Clone, Copy)]
enum CursorKind {
    GotoDefinition,
    Hover,
    Completion,
}

impl CursorKind {
    const CHARACTERS: &[char] = &['@', '$', '^'];

    fn parse(text: &str) -> Option<CursorKind> {
        match text {
            "@" => Some(CursorKind::GotoDefinition),
            "$" => Some(CursorKind::Hover),
            "^" => Some(CursorKind::Completion),
            _ => None,
        }
    }

    fn valid(c: char) -> bool {
        CursorKind::CHARACTERS.contains(&c)
    }
}

fn extract_cursors(content: &str) -> Vec<(Position, CursorKind)> {
    let line_index = LineIndex::new(content);
    let mut cursors = vec![];

    for (index, text) in content.match_indices(CursorKind::valid) {
        let line_col = line_index.line_col(TextSize::new(index as u32));
        let line_range = line_index.line(line_col.line).unwrap();
        if !content[line_range].starts_with("--") {
            continue;
        }

        let line = line_col.line - 1;
        let character = line_col.col;
        let position = Position::new(line, character);
        let Some(kind) = CursorKind::parse(text) else { continue };

        cursors.push((position, kind));
    }

    cursors
}

pub fn report(engine: &QueryEngine, files: &Files, id: FileId) -> String {
    let uri = {
        let path = files.path(id);
        let content = files.content(id);
        let uri = Url::parse(&path).unwrap();
        prim::handle_generated(uri, &content).unwrap()
    };

    let content = engine.content(id);
    let line_index = LineIndex::new(&content);
    let cursors = extract_cursors(&content);

    let mut result = String::new();
    for (index, (position, cursor)) in cursors.iter().enumerate() {
        let uri = uri.clone();

        if index > 0 {
            writeln!(result, "\n").unwrap();
        }

        writeln!(result, "{cursor:#?} at {position:?}\n").unwrap();

        let line_0 = line_index.line(position.line);
        let line_1 = line_index.line(position.line + 1);
        if let Some((line_0, line_1)) = line_0.zip(line_1) {
            let line_0 = &content[line_0];
            let line_1 = &content[line_1];
            writeln!(result, "```").unwrap();
            write!(result, "{line_0}").unwrap();
            write!(result, "{line_1}").unwrap();
            writeln!(result, "```").unwrap();
        }
        writeln!(result).unwrap();

        dispatch_cursor(&mut result, engine, files, *position, *cursor, uri);
    }

    redact_paths(result)
}

fn dispatch_cursor(
    result: &mut String,
    engine: &QueryEngine,
    files: &Files,
    position: Position,
    cursor: CursorKind,
    uri: Url,
) {
    match cursor {
        CursorKind::GotoDefinition => {
            if let Some(response) =
                analyzer::definition::implementation(engine, files, uri, position)
            {
                let convert = |location: Location| -> String {
                    format!(
                        "{} @ {}:{}..{}:{}",
                        location.uri,
                        location.range.start.line,
                        location.range.start.character,
                        location.range.end.line,
                        location.range.end.character,
                    )
                };

                match response {
                    GotoDefinitionResponse::Scalar(location) => {
                        let location = convert(location);
                        writeln!(result, "{location}").unwrap();
                    }
                    GotoDefinitionResponse::Array(location) => {
                        let location = location.into_iter().map(convert).join("\n");
                        writeln!(result, "{location}").unwrap();
                    }
                    GotoDefinitionResponse::Link(_) => (),
                }
            }
        }
        CursorKind::Hover => {
            if let Some(response) = analyzer::hover::implementation(engine, files, uri, position) {
                let convert = |marked: MarkedString| -> String {
                    match marked {
                        MarkedString::String(string) => string,
                        MarkedString::LanguageString(LanguageString {
                            language, value, ..
                        }) => format!("```{language}\n{value}\n```"),
                    }
                };

                match response.contents {
                    HoverContents::Scalar(marked) => {
                        let marked = convert(marked);
                        writeln!(result, "{marked}").unwrap();
                    }
                    HoverContents::Array(marked) => {
                        let marked = marked.into_iter().map(convert).join("\n");
                        writeln!(result, "{marked}").unwrap();
                    }
                    HoverContents::Markup(markup) => {
                        writeln!(result, "{}", markup.value).unwrap();
                    }
                }
            }
        }
        CursorKind::Completion => {
            if let Some(response) =
                analyzer::completion::implementation(engine, files, uri, position)
            {
                match response {
                    CompletionResponse::Array(items)
                    | CompletionResponse::List(CompletionList { items, .. }) => {
                        let items: Vec<TabledCompletionItem> =
                            items.into_iter().map(TabledCompletionItem::from).collect();

                        let mut table = Table::new(items);
                        table.with(Style::modern_rounded());
                        table.with(Padding::new(2, 2, 0, 0));

                        writeln!(result, "{table}").unwrap();
                    }
                }
            }
        }
    }
}

fn redact_paths(result: String) -> String {
    let manifest_directory = env!("CARGO_MANIFEST_DIR");
    let temporary_directory = prim::TEMPORARY_DIRECTORY.path().to_str().unwrap();
    result
        .replace(manifest_directory, "/tests-integration")
        .replace(temporary_directory, "/temporary-directory")
}
