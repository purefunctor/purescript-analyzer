use rowan::ast::{support, AstChildren, AstNode};

use crate::PureScript;

_create_ast_t!(OneOrMore, Separated, Wrapped, ZeroOrMore);

impl<T: AstNode<Language = PureScript>> OneOrMore<T> {
    pub fn children(&self) -> AstChildren<T> {
        support::children(&self.node)
    }
}

impl<T: AstNode<Language = PureScript>> Separated<T> {
    pub fn children(&self) -> AstChildren<T> {
        support::children(&self.node)
    }
}

impl<T: AstNode<Language = PureScript>> Wrapped<T> {
    pub fn child(&self) -> Option<T> {
        support::child(&self.node)
    }
}

impl<T: AstNode<Language = PureScript>> ZeroOrMore<T> {
    pub fn children(&self) -> AstChildren<T> {
        support::children(&self.node)
    }
}
