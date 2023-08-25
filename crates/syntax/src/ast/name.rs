use rowan::ast::{support, AstChildren, AstNode};
use smol_str::SmolStr;

_create_ast!(ModuleName, Name, NameRef, QualifiedName, QualifiedPrefix);

impl ModuleName {
    pub fn children(&self) -> AstChildren<Name> {
        support::children(&self.node)
    }
}

impl Name {
    pub fn as_str(&self) -> Option<SmolStr> {
        Some(self.node.first_token()?.text().into())
    }
}

impl NameRef {
    pub fn as_str(&self) -> Option<SmolStr> {
        Some(self.node.first_token()?.text().into())
    }
}

impl QualifiedName {
    pub fn prefix(&self) -> Option<QualifiedPrefix> {
        QualifiedPrefix::cast(self.node.first_child()?)
    }

    pub fn name_ref(&self) -> Option<NameRef> {
        // prefix is optional, so we try to parse name_ref
        // at the first position, and then the second...
        if let Some(name_ref) = NameRef::cast(self.node.first_child()?) {
            Some(name_ref)
        } else {
            NameRef::cast(self.node.children().nth(1)?)
        }
    }
}
