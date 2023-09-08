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
        let import_declarations = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.imports()?.imports()?.children()));
        if let Some(import_declarations) = import_declarations {
            for import_declaration in import_declarations {
                qualified_imports.collect_import(import_declaration);
            }
        }

        Arc::new(qualified_imports)
    }

    fn collect_import(&mut self, import_declaration: ast::ImportDeclaration) -> Option<()> {
        let import_qualified = import_declaration.import_qualified()?;
        let module_name = ModuleName::try_from(import_qualified.module_name()?).ok()?;
        let import_declaration = ImportDeclaration { module_name: module_name.clone() };
        let import_id = ImportId::new(self.inner.alloc(import_declaration));
        self.name_to_id.insert(module_name, import_id);
        Some(())
    }

    pub fn import_declaration(&self, import_id: ImportId) -> &ImportDeclaration {
        &self.inner[import_id.inner]
    }

    pub fn import_id(&self, module_name: &ModuleName) -> Option<ImportId> {
        self.name_to_id.get(module_name).copied()
    }
}
