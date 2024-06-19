use rowan::ast::{support, AstChildren, AstNode};

use crate::PureScript;

use super::QualifiedName;

_create_ast_t!(Source, Wrapped);

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

_create_ast!(SymbolOperator);
_create_ast_t!(TickOperator);

impl SymbolOperator {
    pub fn name(&self) -> Option<QualifiedName> {
        support::child(&self.node)
    }
}

impl <T: AstNode<Language = PureScript>> TickOperator<T> {
    pub fn term(&self) -> Option<T> {
        support::child(&self.node)
    }
}

_create_ast_t2!(OperatorPair);

impl<T: AstNode<Language = PureScript>, U: AstNode<Language = PureScript>> OperatorPair<T, U> {
    pub fn operator(&self) -> Option<T> {
        T::cast(self.node.first_child()?)
    }

    pub fn term(&self) -> Option<U> {
        U::cast(self.node.last_child()?)
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
