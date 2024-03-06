mod builder;
pub mod error;
mod grammar;
mod grammar_redux;
mod parser;

use builder::Builder;
use error::ParseError;
use grammar::module;
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

pub fn parse_module(source: &str) -> (SyntaxNode, Vec<ParseError>) {
    let lexed = lex(source);
    let input = layout(&lexed);

    let mut parser = Parser::new(&input);
    module(&mut parser);
    let output = parser.finalize();

    process_output(&lexed, output)
}
