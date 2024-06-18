use rowan::ast::{support, AstChildren, AstNode};

use crate::PureScript;

_create_ast_t!(OneOrMore, Source, Wrapped);

impl<T: AstNode<Language = PureScript>> OneOrMore<T> {
    pub fn children(&self) -> AstChildren<T> {
        support::children(&self.node)
    }
}

impl<T: AstNode<Language = PureScript>> Source<T> {
    pub fn child(&self) -> Option<T> {
        support::child(&self.node)
    }
}

impl<T: AstNode<Language = PureScript>> Wrapped<T> {
    pub fn child(&self) -> Option<T> {
        support::child(&self.node)
    }
}

_create_ast_t!(ArgumentList, LayoutList);

impl<T: AstNode<Language = PureScript>> ArgumentList<T> {
    pub fn children(&self) -> AstChildren<T> {
        support::children(&self.node)
    }
}

impl<T: AstNode<Language = PureScript>> LayoutList<T> {
    pub fn children(&self) -> AstChildren<T> {
        support::children(&self.node)
    }
}
