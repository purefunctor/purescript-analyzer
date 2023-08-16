//! Implements the [`SyntaxNode`] builder.
//!
//! [`SyntaxNode`]: syntax::SyntaxNode

use lexing::Lexed;
use rowan::GreenNodeBuilder;
use syntax::{SyntaxKind, SyntaxNode};

use crate::error::ParseError;

pub(crate) struct Builder<'a> {
    lexed: &'a Lexed<'a>,
    index: usize,
    inner: GreenNodeBuilder<'static>,
    errors: Vec<ParseError>,
}

impl<'a> Builder<'a> {
    pub(crate) fn new(lexed: &'a Lexed<'a>) -> Builder<'a> {
        let index = 0;
        let mut inner = GreenNodeBuilder::default();
        inner.start_node(SyntaxKind::Source.into());
        let errors = vec![];
        Builder { lexed, index, inner, errors }
    }

    pub(crate) fn finalize(mut self) -> (SyntaxNode, Vec<ParseError>) {
        self.inner.finish_node();
        (SyntaxNode::new_root(self.inner.finish()), self.errors)
    }

    fn is_eof(&self) -> bool {
        self.index >= self.lexed.len()
    }

    fn eat_token(&mut self, kind: SyntaxKind) {
        if kind.is_layout() {
            return;
        }
        let text = self.lexed.text(self.index);
        self.inner.token(kind.into(), text);
        self.index += 1;
    }

    fn eat_whitespace_or_comments(&mut self) {
        while !self.is_eof() {
            let kind = self.lexed.kind(self.index);
            if !kind.is_whitespace_or_comment() {
                break;
            }
            self.eat_token(kind);
        }
    }

    pub(crate) fn start(&mut self, kind: SyntaxKind) {
        self.eat_whitespace_or_comments();
        self.inner.start_node(kind.into());
    }

    pub(crate) fn token(&mut self, kind: SyntaxKind) {
        self.eat_whitespace_or_comments();
        self.eat_token(kind);
    }

    pub(crate) fn error(&mut self, message: String) {
        let position = self.lexed.position(self.index);
        self.errors.push(ParseError::new(position, message));
    }

    pub(crate) fn finish(&mut self) {
        self.inner.finish_node();
    }
}
