use lexing::Lexed;
use rowan::GreenNodeBuilder;
use syntax::{SyntaxKind, SyntaxNode};

use crate::ParseError;

#[derive(Debug)]
pub(crate) enum Output {
    Start { kind: SyntaxKind },
    Token { kind: SyntaxKind },
    Error { message: String },
    Finish,
}

pub(crate) fn build(lexed: &Lexed<'_>, output: Vec<Output>) -> (SyntaxNode, Vec<ParseError>) {
    let mut index = 0;
    let mut builder = GreenNodeBuilder::new();
    let mut errors = vec![];

    for event in output {
        match event {
            Output::Start { kind } => {
                if kind != SyntaxKind::Node {
                    builder.start_node(kind.into());
                }
            }
            Output::Token { kind } => {
                let text = lexed.text(index);
                builder.token(kind.into(), text);
                index += 1;
            }
            Output::Error { message } => {
                let position = lexed.position(index);
                errors.push(ParseError { position, message });
            }
            Output::Finish => {
                builder.finish_node();
            }
        }
        if let Some(message) = lexed.error(index) {
            let position = lexed.position(index);
            let message = format!("lex error: {}", message);
            errors.push(ParseError { position, message });
        }
    }

    let node = SyntaxNode::new_root(builder.finish());
    (node, errors)
}
