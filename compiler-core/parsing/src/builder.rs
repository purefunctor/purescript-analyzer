use std::sync::Arc;

use lexing::Lexed;
use rowan::GreenNodeBuilder;
use syntax::SyntaxKind;

use crate::{ParseError, ParsedModule};

#[derive(Debug)]
pub(crate) enum Output {
    Start { kind: SyntaxKind },
    Annotate,
    Qualify,
    Token { kind: SyntaxKind },
    Error { message: Arc<str> },
    Finish,
}

struct Builder<'l, 's> {
    lexed: &'l Lexed<'s>,
    index: usize,
    annotated: bool,
    qualified: bool,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,
}

impl<'l, 's> Builder<'l, 's> {
    fn new(lexed: &'l Lexed<'s>) -> Builder<'l, 's> {
        let index = 0;
        let annotated = false;
        let qualified = false;
        let builder = GreenNodeBuilder::new();
        let errors = vec![];
        Builder { lexed, index, annotated, qualified, builder, errors }
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

    fn annotate(&mut self) {
        if let Some(annotation) = self.lexed.annotation(self.index)
            && !self.annotated
        {
            self.builder.start_node(SyntaxKind::Annotation.into());
            self.builder.token(SyntaxKind::TEXT.into(), annotation);
            self.builder.finish_node();
        }

        self.annotated = true;
    }

    fn qualify(&mut self) {
        if let Some(qualifier) = self.lexed.qualifier(self.index)
            && !self.qualified
        {
            self.builder.start_node(SyntaxKind::Qualifier.into());
            self.builder.token(SyntaxKind::TEXT.into(), qualifier);
            self.builder.finish_node();
        }

        self.qualified = true;
    }

    fn token(&mut self, kind: SyntaxKind) {
        if kind.is_layout_token() {
            return self.builder.token(kind.into(), "");
        }

        self.annotate();

        if let Some(message) = self.lexed.error(self.index) {
            self.error(message);
        }

        self.qualify();

        if !matches!(kind, SyntaxKind::ERROR) {
            let text = self.lexed.text(self.index);
            self.builder.token(kind.into(), text);
        }

        self.index += 1;
        self.annotated = false;
        self.qualified = false;
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
            Output::Annotate => builder.annotate(),
            Output::Qualify => builder.qualify(),
            Output::Token { kind } => builder.token(kind),
            Output::Error { message } => builder.error(message),
            Output::Finish => builder.finish(),
        }
    }

    builder.build()
}
