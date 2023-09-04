use std::sync::Arc;

use files::FileId;
use la_arena::{Arena, Idx};
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{id::InFile, names::ModuleName, ResolverDatabase};

#[derive(Debug, PartialEq, Eq)]
pub struct ImportDeclaration {
    module_name: ModuleName,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ImportId {
    inner: Idx<ImportDeclaration>,
}

impl ImportId {
    pub(crate) fn new(inner: Idx<ImportDeclaration>) -> ImportId {
        ImportId { inner }
    }

    pub fn in_file(self, file_id: FileId) -> InFile<ImportId> {
        InFile { file_id, value: self }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct QualifiedImports {
    inner: Arena<ImportDeclaration>,
    name_to_id: FxHashMap<ModuleName, ImportId>,
}

impl QualifiedImports {
    pub(crate) fn qualified_imports_query(
        db: &dyn ResolverDatabase,
        file_id: FileId,
    ) -> Arc<QualifiedImports> {
        let mut qualified_imports = QualifiedImports::default();

        let node = db.parse_file(file_id);
        let imports = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.imports()?.imports()?.children()));
        if let Some(imports) = imports {
            for import in imports {
                if let Some(import_qualified) = import.import_qualified() {
                    let module_name = ModuleName::from(import_qualified.module_name().unwrap());
                    let import_declaration = ImportDeclaration { module_name: module_name.clone() };
                    let import_id = qualified_imports.inner.alloc(import_declaration);
                    qualified_imports.name_to_id.insert(module_name, ImportId::new(import_id));
                }
            }
        }

        Arc::new(qualified_imports)
    }

    pub fn import_declaration(&self, import_id: ImportId) -> &ImportDeclaration {
        &self.inner[import_id.inner]
    }

    pub fn import_id(&self, module_name: &ModuleName) -> Option<ImportId> {
        self.name_to_id.get(module_name).copied()
    }
}
