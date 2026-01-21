use itertools::Itertools;

use crate::{Diagnostic, Severity};

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

pub fn to_lsp_diagnostic(
    diagnostic: &Diagnostic,
    content: &str,
    uri: &lsp_types::Url,
) -> Option<lsp_types::Diagnostic> {
    use line_index::{LineCol, LineIndex};
    use lsp_types::{
        DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Position, Range,
    };

    let line_index = LineIndex::new(content);

    let to_position = |offset: u32| -> Option<Position> {
        let LineCol { line, col } = line_index.line_col(offset.into());
        let line_range = line_index.line(line)?;
        let line_content = &content[line_range];
        let until_col = &line_content[..col as usize];
        let character = until_col.chars().count() as u32;
        Some(Position { line, character })
    };

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
