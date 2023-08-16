//! Implements the error type.

use position::Position;

#[derive(Debug)]
pub struct ParseError {
    position: Position,
    message: String,
}

impl ParseError {
    pub fn new(position: Position, message: impl Into<String>) -> ParseError {
        let message = message.into();
        ParseError { position, message }
    }

    pub fn position(&self) -> Position {
        self.position
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}
