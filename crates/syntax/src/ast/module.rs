use rowan::ast::AstNode;

use crate::{SyntaxKind, SyntaxToken};

use super::{Declaration, ModuleName, NameRef};

_create_ast!(
    Module,
    ModuleHeader,
    ExportList,
    ModuleImports,
    ImportDeclaration,
    ImportList,
    ImportQualified,
    ModuleBody
);

_create_ast_v!(
    ExportItem,
    ExportClass(ExportClass),
    ExportType(ExportType),
    ExportValue(ExportValue)
);

_create_ast_v!(
    ImportItem,
    ImportClass(ImportClass),
    ImportOp(ImportOp),
    ImportType(ImportType),
    ImportTypeOp(ImportTypeOp),
    ImportValue(ImportValue)
);

_create_ast_v!(DataMembers, DataAll(DataAll), DataEnumerated(DataEnumerated));

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

_has_children!(ModuleImports<ImportDeclaration>, ModuleBody<Declaration>);

impl ModuleHeader {
    pub fn name(&self) -> Option<ModuleName> {
        ModuleName::cast(self.node.first_child()?)
    }

    pub fn export_list(&self) -> Option<ExportList> {
        ExportList::cast(self.node.last_child()?)
    }
}

_has_children!(ExportList<ExportItem>);

impl ExportClass {
    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
    }
}

impl ExportType {
    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
    }

    pub fn data_members(&self) -> Option<DataMembers> {
        DataMembers::cast(self.node.last_child()?)
    }
}

impl ExportValue {
    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
    }
}

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

impl ImportList {
    pub fn hiding_token(&self) -> Option<SyntaxToken> {
        let token = self.node.first_token()?;
        if let SyntaxKind::HidingKw = token.kind() {
            Some(token)
        } else {
            None
        }
    }
}

_has_children!(ImportList<ImportItem>);

impl ImportClass {
    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
    }
}

impl ImportValue {
    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
    }
}

impl ImportQualified {
    pub fn module_name(&self) -> Option<ModuleName> {
        ModuleName::cast(self.node.first_child()?)
    }
}

impl ImportType {
    pub fn name_ref(&self) -> Option<NameRef> {
        NameRef::cast(self.node.first_child()?)
    }

    pub fn data_members(&self) -> Option<DataMembers> {
        DataMembers::cast(self.node.last_child()?)
    }
}

_has_children!(DataEnumerated<NameRef>);
