//! See documentation for [`QualifiedImports`].
use std::sync::Arc;

use files::FileId;
use la_arena::{Arena, Idx};
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{id::InFile, names::ModuleName, ResolverDatabase};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ImportDeclaration {
    // FIXME: migrate to ModuleId
    pub(crate) file_id: FileId,
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

/// A file's qualified imports.
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
                qualified_imports.collect_import(db, import_declaration);
            }
        }

        Arc::new(qualified_imports)
    }

    fn collect_import(
        &mut self,
        db: &dyn ResolverDatabase,
        import_declaration: ast::ImportDeclaration,
    ) -> Option<()> {
        let imported_module_name = ModuleName::try_from(import_declaration.module_name()?).ok()?;
        let qualified_as_module_name =
            ModuleName::try_from(import_declaration.import_qualified()?.module_name()?).ok()?;

        let module_id = db.module_map().module_id(&imported_module_name)?;
        let file_id = db.module_map().file_id(module_id)?;

        let import_declaration = ImportDeclaration { file_id };
        let import_id = ImportId::new(self.inner.alloc(import_declaration));

        self.name_to_id.insert(qualified_as_module_name, import_id);

        Some(())
    }

    pub fn import_declaration(&self, import_id: ImportId) -> ImportDeclaration {
        self.inner[import_id.inner]
    }

    pub fn import_id(&self, module_name: &ModuleName) -> Option<ImportId> {
        self.name_to_id.get(module_name).copied()
    }
}
