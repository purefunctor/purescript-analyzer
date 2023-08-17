use rowan::ast::{support, AstChildren, AstNode};

use crate::PureScript;

_create_ast_t!(ZeroOrMore, OneOrMore);

impl<T: AstNode<Language = PureScript>> ZeroOrMore<T> {
    pub fn children(&self) -> AstChildren<T> {
        support::children(&self.node)
    }
}

impl<T: AstNode<Language = PureScript>> OneOrMore<T> {
    pub fn children(&self) -> AstChildren<T> {
        support::children(&self.node)
    }
}
