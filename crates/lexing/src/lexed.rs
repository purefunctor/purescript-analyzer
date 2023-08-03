//! The lexer's output type.

use std::ops::Range;

use syntax::SyntaxKind;

use crate::position::Position;

/// A sequence of [`SyntaxKind`]s.
pub struct Lexed<'a> {
    source: &'a str,
    kinds: Vec<SyntaxKind>,
    positions: Vec<Position>,
    errors: Vec<LexError>,
}

#[derive(Debug)]
pub struct LexError {
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
            let index = self.len() as u32;
            self.errors.push(LexError { message, index });
        }
    }

    pub(crate) fn eof_position(&self) -> Position {
        self.positions[self.positions.len() - 1]
    }

    /// # Invariant
    ///
    /// [`Lexed`] always contains [`SyntaxKind::EndOfFile`] as its final element.
    /// It exclusively serves as an anchor for the final offset such that methods
    /// can compute the range for a token in a given index. With this in mind, we
    /// make sure that this token is hidden.
    ///
    /// For example, given `"hello"`
    ///
    /// ```rs
    /// // with EndOfFile, 0..5
    /// [(Lower, 0), (EndOfFile, 5)]
    ///
    /// // without EndOfFile, 0..?
    /// [(Lower, 0)]
    /// ```
    pub fn len(&self) -> usize {
        self.kinds.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the kind for an index.
    pub fn kind(&self, index: usize) -> SyntaxKind {
        assert!(index < self.len());
        self.kinds[index]
    }

    /// Returns the position for an index.
    pub fn position(&self, index: usize) -> Position {
        assert!(index < self.len());
        self.positions[index]
    }

    /// Returns the text for an index.
    pub fn text(&self, index: usize) -> &str {
        self.text_in_range(index..index + 1)
    }

    /// Returns the text for a range.
    pub fn text_in_range(&self, range: Range<usize>) -> &str {
        assert!(range.start < range.end && range.end <= self.len());
        let low = self.positions[range.start].offset;
        let high = self.positions[range.end].offset;
        &self.source[low..high]
    }

    /// Returns the error for an index.
    pub fn error(&self, index: usize) -> Option<&str> {
        assert!(index < self.len());
        let error_index = self.errors.binary_search_by_key(&(index as u32), |v| v.index).ok()?;
        Some(&self.errors[error_index].message)
    }
}
