use std::sync::Arc;

use lexing::Lexed;
use rowan::GreenNodeBuilder;
use syntax::SyntaxKind;

use crate::{ParseError, ParsedModule};

#[derive(Debug)]
pub(crate) enum Output {
    Start { kind: SyntaxKind },
    Token { kind: SyntaxKind },
    Error { message: Arc<str> },
    Finish,
}

struct Builder<'l, 's> {
    lexed: &'l Lexed<'s>,
    index: usize,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,
}

impl<'l, 's> Builder<'l, 's> {
    fn new(lexed: &'l Lexed<'s>) -> Builder<'l, 's> {
        let index = 0;
        let builder = GreenNodeBuilder::new();
        let errors = vec![];
        Builder { lexed, index, builder, errors }
    }

    fn build(self) -> (ParsedModule, Vec<ParseError>) {
        let node = self.builder.finish();
        (ParsedModule::new(node), self.errors)
    }

    fn start(&mut self, kind: SyntaxKind) {
        if kind != SyntaxKind::Node {
            self.builder.start_node(kind.into());
        }
    }

    fn token(&mut self, kind: SyntaxKind) {
        if kind.is_layout_token() {
            return self.builder.token(kind.into(), "");
        }

        if let Some(annotation) = self.lexed.annotation(self.index) {
            self.builder.start_node(SyntaxKind::Annotation.into());
            self.builder.token(SyntaxKind::TEXT.into(), annotation);
            self.builder.finish_node();
        }

        if let Some(message) = self.lexed.error(self.index) {
            self.error(message);
        }

        if let Some(qualifier) = self.lexed.qualifier(self.index) {
            self.builder.start_node(SyntaxKind::Qualifier.into());
            self.builder.token(SyntaxKind::TEXT.into(), qualifier);
            self.builder.finish_node();
        }

        let text = self.lexed.text(self.index);
        self.builder.token(kind.into(), text);

        self.index += 1;
    }

    fn error(&mut self, message: impl Into<Arc<str>>) {
        let info = self.lexed.info(self.index);
        let offset = info.qualifier as usize;
        let position = self.lexed.position(self.index);
        let message = message.into();
        self.builder.token(SyntaxKind::ERROR.into(), "");
        self.errors.push(ParseError { offset, position, message });
    }

    fn finish(&mut self) {
        self.builder.finish_node();
    }
}

pub(crate) fn build(lexed: &Lexed<'_>, output: Vec<Output>) -> (ParsedModule, Vec<ParseError>) {
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
