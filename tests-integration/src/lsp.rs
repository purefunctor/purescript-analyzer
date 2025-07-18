use std::fmt::Write;

use analyzer::QueryEngine;
use async_lsp::lsp_types::{CompletionList, CompletionResponse, Position, Url};
use files::{FileId, Files};
use line_index::{LineIndex, TextSize};

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
        Url::parse(&path).unwrap()
    };

    let content = engine.content(id);
    let cursors = extract_cursors(&content);

    let mut result = String::new();
    for (index, (position, cursor)) in cursors.iter().enumerate() {
        let uri = uri.clone();

        if index > 0 {
            writeln!(result, "\n").unwrap();
        }

        writeln!(result, "{cursor:#?} at {position:?}\n").unwrap();
        dispatch_cursor(&mut result, engine, files, *position, *cursor, uri);
    }

    cleanup_report(result)
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
                writeln!(result, "{response:#?}").unwrap();
            }
        }
        CursorKind::Hover => {
            if let Some(response) = analyzer::hover::implementation(engine, files, uri, position) {
                writeln!(result, "{response:#?}").unwrap();
            }
        }
        CursorKind::Completion => {
            if let Some(response) =
                analyzer::completion::implementation(engine, files, uri, position)
            {
                match response {
                    CompletionResponse::Array(items)
                    | CompletionResponse::List(CompletionList { items, .. }) => {
                        let items: Vec<_> = items
                            .into_iter()
                            .map(|item| analyzer::completion::resolve::implementation(engine, item))
                            .collect();
                        writeln!(result, "{items:#?}").unwrap();
                    }
                }
            }
        }
    }
}

fn cleanup_report(result: String) -> String {
    let manifest_directory = env!("CARGO_MANIFEST_DIR");
    result.replace(manifest_directory, "./tests-integration")
}
