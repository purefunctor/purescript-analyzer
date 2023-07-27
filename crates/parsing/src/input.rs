//! The input type for the parser.

use syntax::SyntaxKind;

use crate::position::Position;

#[allow(dead_code)]
pub struct Input {
    tokens: Vec<SyntaxKind>,
    positions: Vec<Position>,
}
