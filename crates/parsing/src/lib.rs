mod builder;
pub mod error;
mod grammar;
mod parser;

use builder::Builder;
use error::ParseError;
use grammar::{expression, module, pattern, ty};
use lexing::{layout, lex, Lexed};
use parser::{Event, Parser};
use syntax::{SyntaxKind, SyntaxNode};

fn process_output(
    lexed: &Lexed<'_>,
    output: impl IntoIterator<Item = Event>,
) -> (SyntaxNode, Vec<ParseError>) {
    let mut builder = Builder::new(lexed);
    for event in output {
        if matches!(event, Event::Start { kind: SyntaxKind::Sentinel }) {
            continue;
        }
        if matches!(event, Event::Token { kind } if kind.is_layout()) {
            continue;
        }

        match event {
            Event::Start { kind } => builder.start(kind),
            Event::Token { kind } => builder.token(kind),
            Event::Error { message } => builder.error(message),
            Event::Finish => builder.finish(),
        }
    }

    let (node, mut errors) = builder.finalize();
    for index in 0..lexed.len() {
        if let Some(message) = lexed.error(index) {
            let position = lexed.position(index);
            errors.push(ParseError::new(position, message));
        }
    }
    (node, errors)
}

pub fn parse_expression(source: &str) -> (SyntaxNode, Vec<ParseError>) {
    let lexed = lex(source);
    let input = layout(&lexed);

    let mut parser = Parser::new(&input);
    expression(&mut parser);
    let output = parser.finalize();

    process_output(&lexed, output)
}

// FIXME: remove
pub fn parse_type(source: &str) -> (SyntaxNode, Vec<ParseError>) {
    let lexed = lex(source);
    let input = layout(&lexed);

    let mut parser = Parser::new(&input);
    ty(&mut parser);
    let output = parser.finalize();

    process_output(&lexed, output)
}

// FIXME: remove
pub fn parse_pattern(source: &str) -> (SyntaxNode, Vec<ParseError>) {
    let lexed = lex(source);
    let input = layout(&lexed);

    let mut parser = Parser::new(&input);
    pattern(&mut parser);
    let output = parser.finalize();

    process_output(&lexed, output)
}

pub fn parse_module(source: &str) -> (SyntaxNode, Vec<ParseError>) {
    let lexed = lex(source);
    let input = layout(&lexed);

    let mut parser = Parser::new(&input);
    module(&mut parser);
    let output = parser.finalize();

    process_output(&lexed, output)
}

#[test]
fn __() {
    let (node, _) = parse_module("
module Main where

import A
import B.B (c, d)
import E.E.E hiding (f, g)
import H.H.H.H (i, j) as K
import L.L.L.L.L hiding (m, n) as O
    
import Types ((-), Type, type (+), class TypeClass, value)
");
    println!("{:#?}", node);
}
