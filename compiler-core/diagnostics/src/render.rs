use itertools::Itertools;
use line_index::{LineCol, LineIndex};
use lsp_types::{
    DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Position, Range,
};
use rowan::TextSize;

use crate::{Diagnostic, Severity, Span};

pub fn format_text(diagnostics: &[Diagnostic]) -> String {
    let mut output = String::new();

    for diagnostic in diagnostics {
        let severity = match diagnostic.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
        };

        output.push_str(&format!(
            "{severity}[{}] at {}..{}: {}\n",
            diagnostic.code, diagnostic.primary.start, diagnostic.primary.end, diagnostic.message
        ));

        for related in &diagnostic.related {
            output.push_str(&format!(
                "  note at {}..{}: {}\n",
                related.span.start, related.span.end, related.message
            ));
        }
    }

    output
}

fn line_text<'a>(line_index: &LineIndex, content: &'a str, line: u32) -> Option<&'a str> {
    let range = line_index.line(line)?;
    let text = &content[range];
    Some(text.trim_end_matches(['\n', '\r']))
}

fn caret_marker(line: &str, start_col: u32, end_col: Option<u32>) -> String {
    let line_len = line.chars().count() as u32;
    let start = start_col.min(line_len);
    let end = end_col.unwrap_or(line_len).min(line_len);

    if end <= start {
        format!("{}^", " ".repeat(start as usize))
    } else {
        let tilde_count = (end - start).saturating_sub(1) as usize;
        format!("{}^{}", " ".repeat(start as usize), "~".repeat(tilde_count))
    }
}

fn span_location(
    line_index: &LineIndex,
    content: &str,
    span: Span,
) -> Option<((u32, u32), (u32, u32))> {
    let start = offset_to_position(line_index, content, TextSize::from(span.start))?;
    let end = offset_to_position(line_index, content, TextSize::from(span.end))?;
    Some(((start.line, start.character), (end.line, end.character)))
}

pub fn format_rustc(diagnostics: &[Diagnostic], content: &str) -> String {
    let line_index = LineIndex::new(content);
    let mut output = String::new();

    for diagnostic in diagnostics {
        let severity = match diagnostic.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
        };

        output.push_str(&format!("{severity}[{}]: {}\n", diagnostic.code, diagnostic.message));

        if let Some(((start_line, start_col), (end_line, end_col))) =
            span_location(&line_index, content, diagnostic.primary)
        {
            let display_start_line = start_line + 1;
            let display_start_col = start_col + 1;
            let display_end_line = end_line + 1;
            let display_end_col = end_col + 1;

            output.push_str(&format!(
                "  --> {}:{}..{}:{}\n",
                display_start_line, display_start_col, display_end_line, display_end_col
            ));

            if let Some(line) = line_text(&line_index, content, start_line) {
                let line_num_width = display_start_line.to_string().len();
                output.push_str(&format!("{:>width$} |\n", "", width = line_num_width));
                output.push_str(&format!("{} | {}\n", display_start_line, line));

                let marker_end_col = if start_line == end_line { Some(end_col) } else { None };
                let marker = caret_marker(line, start_col, marker_end_col);
                output.push_str(&format!("{:>width$} | {}\n", "", marker, width = line_num_width));
            }
        }

        for related in &diagnostic.related {
            output.push_str(&format!("  note: {}\n", related.message));

            if let Some(((start_line, start_col), (end_line, end_col))) =
                span_location(&line_index, content, related.span)
            {
                let display_start_line = start_line + 1;
                let display_start_col = start_col + 1;
                let display_end_line = end_line + 1;
                let display_end_col = end_col + 1;

                output.push_str(&format!(
                    "    --> {}:{}..{}:{}\n",
                    display_start_line, display_start_col, display_end_line, display_end_col
                ));

                if let Some(line) = line_text(&line_index, content, start_line) {
                    let line_num_width = display_start_line.to_string().len();
                    output.push_str(&format!("  {:>width$} |\n", "", width = line_num_width));
                    output.push_str(&format!("  {} | {}\n", display_start_line, line));

                    let marker_end_col = if start_line == end_line { Some(end_col) } else { None };
                    let marker = caret_marker(line, start_col, marker_end_col);
                    output.push_str(&format!(
                        "  {:>width$} | {}\n",
                        "",
                        marker,
                        width = line_num_width
                    ));
                }
            }
        }
    }

    output
}

fn offset_to_position(line_index: &LineIndex, content: &str, offset: TextSize) -> Option<Position> {
    let LineCol { line, col } = line_index.line_col(offset);

    let line_text_range = line_index.line(line)?;
    let line_content = &content[line_text_range];

    let until_col = &line_content[..col as usize];
    let character = until_col.chars().count() as u32;

    Some(Position { line, character })
}

pub fn to_lsp_diagnostic(
    diagnostic: &Diagnostic,
    content: &str,
    uri: &lsp_types::Url,
) -> Option<lsp_types::Diagnostic> {
    let line_index = LineIndex::new(content);

    let to_position =
        |offset: u32| offset_to_position(&line_index, content, TextSize::from(offset));

    let start = to_position(diagnostic.primary.start)?;
    let end = to_position(diagnostic.primary.end)?;
    let range = Range { start, end };

    let severity = match diagnostic.severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
    };

    let related_information = diagnostic.related.iter().filter_map(|related| {
        let start = to_position(related.span.start)?;
        let end = to_position(related.span.end)?;
        Some(DiagnosticRelatedInformation {
            location: Location { uri: uri.clone(), range: Range { start, end } },
            message: related.message.clone(),
        })
    });

    let related_information = related_information.collect_vec();

    Some(lsp_types::Diagnostic {
        range,
        severity: Some(severity),
        code: Some(NumberOrString::String(diagnostic.code.to_string())),
        code_description: None,
        source: Some(format!("analyzer/{}", diagnostic.source)),
        message: diagnostic.message.clone(),
        related_information: if related_information.is_empty() {
            None
        } else {
            Some(related_information)
        },
        tags: None,
        data: None,
    })
}
