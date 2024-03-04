use rowan::ast::{support, AstChildren, AstNode};
use smol_str::SmolStr;

use crate::SyntaxToken;

_create_ast!(ModuleName, Name, NameRef, QualifiedName, QualifiedPrefix);

impl ModuleName {
    pub fn children(&self) -> AstChildren<Name> {
        support::children(&self.node)
    }
}

impl Name {
    pub fn token(&self) -> Option<SyntaxToken> {
        self.node.first_token()
    }

    pub fn as_str(&self) -> Option<SmolStr> {
        Some(self.node.first_token()?.text().into())
    }
}

impl NameRef {
    pub fn token(&self) -> Option<SyntaxToken> {
        self.node.first_token()
    }

    pub fn as_str(&self) -> Option<SmolStr> {
        Some(self.node.first_token()?.text().into())
    }
}

impl QualifiedName {
    pub fn prefix(&self) -> Option<QualifiedPrefix> {
        QualifiedPrefix::cast(self.node.first_child()?)
    }

    pub fn name(&self) -> Option<Name> {
        self.node.children().find_map(Name::cast)
    }

    pub fn name_ref(&self) -> Option<NameRef> {
        self.node.children().find_map(NameRef::cast)
    }
}

impl QualifiedPrefix {
    pub fn children(&self) -> AstChildren<NameRef> {
        support::children(&self.node)
    }
}
