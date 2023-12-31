use rowan::ast::AstNode;

use super::{Declaration, ModuleName, NameRef, Separated, Wrapped, ZeroOrMore};

_create_ast!(
    ImportDeclaration,
    ImportList,
    ImportQualified,
    Module,
    ModuleHeader,
    ExportList,
    ModuleImports,
    ModuleBody
);
_create_ast_v!(ExportItem, ExportValue(ExportValue));

impl ImportDeclaration {
    pub fn module_name(&self) -> Option<ModuleName> {
        ModuleName::cast(self.node.first_child()?)
    }

    pub fn import_list(&self) -> Option<ImportList> {
        ImportList::cast(self.node.children().nth(1)?)
    }

    pub fn import_qualified(&self) -> Option<ImportQualified> {
        ImportQualified::cast(self.node.last_child()?)
    }
}

impl ImportQualified {
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

    pub fn export_list(&self) -> Option<ExportList> {
        ExportList::cast(self.node.last_child()?)
    }
}

impl ExportList {
    pub fn child(&self) -> Option<Wrapped<Separated<ExportItem>>> {
        Wrapped::cast(self.node.first_child()?)
    }
}

impl ExportValue {
    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
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
