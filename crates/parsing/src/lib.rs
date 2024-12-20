use std::sync::Arc;

use lexing::Lexed;
use position::Position;
use syntax::{SyntaxKind, SyntaxNode};

mod builder;
mod parser;

#[derive(Debug)]
pub struct ParseError {
    pub position: Position,
    pub message: Arc<str>,
}

pub fn parse(lexed: &Lexed<'_>, tokens: &[SyntaxKind]) -> (SyntaxNode, Vec<ParseError>) {
    let mut parser = parser::Parser::new(tokens);
    parser::module(&mut parser);

    let output = parser.finish();
    builder::build(lexed, output)
}
