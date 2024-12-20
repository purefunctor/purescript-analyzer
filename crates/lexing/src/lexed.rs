//! The lexer's output type.

use std::ops::Range;

use position::Position;
use syntax::SyntaxKind;

/// A sequence of tokens with metadata.
///
/// Information such as token position and error messages are stored alongside
/// the sequence of tokens. While the parser only consumes the latter, we want
/// to keep this around such that we can intersperse whitespace and comments,
/// among other things.
pub struct Lexed<'a> {
    source: &'a str,
    kinds: Vec<SyntaxKind>,
    positions: Vec<Position>,
    errors: Vec<LexError>,
}

#[derive(Debug)]
struct LexError {
    message: String,
    index: u32,
}

impl<'a> Lexed<'a> {
    pub(crate) fn new(source: &'a str) -> Lexed<'a> {
        let kinds = vec![];
        let positions = vec![];
        let errors = vec![];
        Lexed { source, kinds, positions, errors }
    }

    pub(crate) fn push(&mut self, kind: SyntaxKind, position: Position, error: Option<&str>) {
        self.kinds.push(kind);
        self.positions.push(position);

        if let Some(error) = error {
            let message = error.to_string();
            let index = self.kinds.len() as u32 - 1;
            self.errors.push(LexError { message, index });
        }
    }

    /// Returns the kind for an index.
    pub fn kind(&self, index: usize) -> SyntaxKind {
        assert!(index < self.kinds.len());
        self.kinds[index]
    }

    /// Returns the position for an index.
    pub fn position(&self, index: usize) -> Position {
        assert!(index <= self.positions.len());
        self.positions[index]
    }

    /// Returns the text for an index.
    pub fn text(&self, index: usize) -> &str {
        self.text_in_range(index..index + 1)
    }

    /// Returns the text for a range.
    pub fn text_in_range(&self, range: Range<usize>) -> &str {
        assert!(range.start < range.end && range.end < self.kinds.len());
        let low = self.positions[range.start].offset;
        let high = self.positions[range.end].offset;
        &self.source[low..high]
    }

    /// Returns the error for an index.
    pub fn error(&self, index: usize) -> Option<&str> {
        assert!(index < self.kinds.len());
        let error_index = self.errors.binary_search_by_key(&(index as u32), |v| v.index).ok()?;
        Some(&self.errors[error_index].message)
    }

    pub fn kinds(&self) -> &[SyntaxKind] {
        &self.kinds
    }
}
