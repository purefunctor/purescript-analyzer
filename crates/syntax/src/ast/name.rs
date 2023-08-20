use rowan::ast::{support, AstChildren};
use smol_str::SmolStr;

_create_ast!(ModuleName, Name, NameRef);

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
