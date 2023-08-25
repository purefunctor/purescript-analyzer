use rowan::ast::AstNode;

use super::{Declaration, ModuleName, ZeroOrMore};

_create_ast!(ImportDeclaration, Module, ModuleHeader, ModuleImports, ModuleBody);

impl ImportDeclaration {
    pub fn module_name(&self) -> Option<ModuleName> {
        ModuleName::cast(self.node.first_child()?)
    }
}

impl Module {
    pub fn header(&self) -> Option<ModuleHeader> {
        ModuleHeader::cast(self.node.first_child()?)
    }

    pub fn imports(&self) -> Option<ModuleImports> {
        ModuleImports::cast(self.node.children().nth(1)?)
    }

    pub fn body(&self) -> Option<ModuleBody> {
        ModuleBody::cast(self.node.last_child()?)
    }
}

impl ModuleHeader {
    pub fn name(&self) -> Option<ModuleName> {
        ModuleName::cast(self.node.first_child()?)
    }
}

impl ModuleImports {
    pub fn imports(&self) -> Option<ZeroOrMore<ImportDeclaration>> {
        ZeroOrMore::cast(self.node.first_child()?)
    }
}

impl ModuleBody {
    pub fn declarations(&self) -> Option<ZeroOrMore<Declaration>> {
        ZeroOrMore::cast(self.node.first_child()?)
    }
}
