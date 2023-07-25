//! The input type for the parser.

use syntax::SyntaxKind;

#[allow(dead_code)]
pub struct Input {
    tokens: Vec<SyntaxKind>,
    columns: Vec<u32>,
}
