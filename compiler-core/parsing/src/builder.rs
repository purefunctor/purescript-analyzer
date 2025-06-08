use std::sync::Arc;

use lexing::Lexed;
use rowan::GreenNodeBuilder;
use syntax::SyntaxKind;

use crate::{ParseError, ParsedModule};

#[derive(Debug)]
pub(crate) enum Output {
    Start { kind: SyntaxKind },
    Prefix,
    Token { kind: SyntaxKind },
    Error { message: Arc<str> },
    Finish,
}

struct Builder<'l, 's> {
    lexed: &'l Lexed<'s>,
    index: usize,
    prefixed: bool,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,
}

impl<'l, 's> Builder<'l, 's> {
    fn new(lexed: &'l Lexed<'s>) -> Builder<'l, 's> {
        let index = 0;
        let prefixed = false;
        let builder = GreenNodeBuilder::new();
        let errors = vec![];
        Builder { lexed, index, prefixed, builder, errors }
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

    fn prefix(&mut self) {
        if let Some(annotation) = self.lexed.annotation(self.index) {
            self.builder.start_node(SyntaxKind::Annotation.into());
            self.builder.token(SyntaxKind::TEXT.into(), annotation);
            self.builder.finish_node();
        }

        if let Some(qualifier) = self.lexed.qualifier(self.index) {
            self.builder.start_node(SyntaxKind::Qualifier.into());
            self.builder.token(SyntaxKind::TEXT.into(), qualifier);
            self.builder.finish_node();
        }

        self.prefixed = true;
    }

    fn token(&mut self, kind: SyntaxKind) {
        if kind.is_layout_token() {
            return self.builder.token(kind.into(), "");
        }

        match self.lexed.annotation(self.index) {
            Some(annotation) if !self.prefixed => {
                self.builder.start_node(SyntaxKind::Annotation.into());
                self.builder.token(SyntaxKind::TEXT.into(), annotation);
                self.builder.finish_node();
            }
            _ => (),
        }

        if let Some(message) = self.lexed.error(self.index) {
            self.error(message);
        }

        match self.lexed.qualifier(self.index) {
            Some(qualifier) if !self.prefixed => {
                self.builder.start_node(SyntaxKind::Qualifier.into());
                self.builder.token(SyntaxKind::TEXT.into(), qualifier);
                self.builder.finish_node();
            }
            _ => (),
        }

        if !matches!(kind, SyntaxKind::ERROR) {
            let text = self.lexed.text(self.index);
            self.builder.token(kind.into(), text);
        }

        self.index += 1;
        self.prefixed = false;
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
            Output::Prefix => builder.prefix(),
            Output::Token { kind } => builder.token(kind),
            Output::Error { message } => builder.error(message),
            Output::Finish => builder.finish(),
        }
    }

    builder.build()
}
