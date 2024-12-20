use std::sync::Arc;

use lexing::Lexed;
use rowan::GreenNodeBuilder;
use syntax::{SyntaxKind, SyntaxNode};

use crate::ParseError;

#[derive(Debug)]
pub(crate) enum Output {
    Start { kind: SyntaxKind },
    Token { kind: SyntaxKind },
    Error { message: Arc<str> },
    Finish,
}

enum Annotation {
    Comment,
    Whitespace,
}

struct Builder<'l, 's> {
    lexed: &'l Lexed<'s>,
    index: usize,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,
    whitespace: Vec<usize>,
}

impl<'l, 's> Builder<'l, 's> {
    fn new(lexed: &'l Lexed<'s>) -> Builder<'l, 's> {
        let index = 0;
        let builder = GreenNodeBuilder::new();
        let errors = vec![];
        let whitespace = vec![];
        Builder { lexed, index, builder, errors, whitespace }
    }

    fn build(self) -> (SyntaxNode, Vec<ParseError>) {
        (SyntaxNode::new_root(self.builder.finish()), self.errors)
    }

    fn eat_whitespace(&mut self, annotation: Annotation) {
        let predicate = match annotation {
            Annotation::Comment => |k: SyntaxKind| k.is_whitespace_or_comment(),
            Annotation::Whitespace => |k: SyntaxKind| k.is_whitespace(),
        };
        while predicate(self.lexed.kind(self.index)) {
            self.whitespace.push(self.index);
            self.index += 1;
        }
        if self.whitespace.is_empty() {
            return;
        }
        self.builder.start_node(SyntaxKind::Comment.into());
        self.whitespace.drain(..).for_each(|index| {
            let kind = self.lexed.kind(index);
            let text = self.lexed.text(index);
            self.builder.token(kind.into(), text);
        });
        self.builder.finish_node();
    }

    fn start(&mut self, kind: SyntaxKind) {
        if kind != SyntaxKind::Node {
            self.builder.start_node(kind.into());
            self.eat_whitespace(Annotation::Comment);
        }
    }

    fn token(&mut self, kind: SyntaxKind) {
        self.eat_whitespace(Annotation::Comment);

        if let Some(message) = self.lexed.error(self.index) {
            self.error(message.to_string());
        }

        let text = self.lexed.text(self.index);
        self.builder.token(kind.into(), text);
        self.index += 1;

        self.eat_whitespace(Annotation::Whitespace);
    }

    fn error(&mut self, message: impl Into<Arc<str>>) {
        let position = self.lexed.position(self.index);
        let message = message.into();
        self.builder.token(SyntaxKind::ERROR.into(), "");
        self.errors.push(ParseError { position, message });
    }

    fn finish(&mut self) {
        self.eat_whitespace(Annotation::Whitespace);
        self.builder.finish_node();
    }
}

pub(crate) fn build(lexed: &Lexed<'_>, output: Vec<Output>) -> (SyntaxNode, Vec<ParseError>) {
    let mut builder = Builder::new(lexed);

    for event in output {
        match event {
            Output::Start { kind } => builder.start(kind),
            Output::Token { kind } => builder.token(kind),
            Output::Error { message } => builder.error(message),
            Output::Finish => builder.finish(),
        }
    }

    builder.build()
}
