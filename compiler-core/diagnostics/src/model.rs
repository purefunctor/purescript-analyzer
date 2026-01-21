#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        Span { start, end }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RelatedSpan {
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DiagnosticCode(&'static str);

impl DiagnosticCode {
    pub fn new(code: &'static str) -> DiagnosticCode {
        DiagnosticCode(code)
    }
}

impl fmt::Display for DiagnosticCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: DiagnosticCode,
    pub message: String,
    pub primary: Span,
    pub related: Vec<RelatedSpan>,
    pub source: &'static str,
}

impl Diagnostic {
    pub fn error(
        code: &'static str,
        message: impl Into<String>,
        primary: Span,
        source: &'static str,
    ) -> Diagnostic {
        let message = message.into();
        let related = vec![];
        Diagnostic {
            severity: Severity::Error,
            code: DiagnosticCode::new(code),
            message,
            primary,
            related,
            source,
        }
    }

    pub fn warning(
        code: &'static str,
        message: impl Into<String>,
        primary: Span,
        source: &'static str,
    ) -> Diagnostic {
        let message = message.into();
        let related = vec![];
        Diagnostic {
            severity: Severity::Warning,
            code: DiagnosticCode::new(code),
            message,
            primary,
            related,
            source,
        }
    }

    pub fn with_related(mut self, span: Span, message: impl Into<String>) -> Diagnostic {
        let message = message.into();
        self.related.push(RelatedSpan { span, message });
        self
    }
}
