use std::sync::Arc;

use files::FileId;
use la_arena::Arena;
use rowan::ast::AstNode;
use syntax::ast;

use crate::{
    names::{InDb, ModuleName, NameRef},
    ResolverDatabase,
};

use super::ModuleMap;

#[derive(Debug, PartialEq, Eq)]
pub enum ImportItem {
    ImportValue(NameRef),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportList {
    items: Vec<ImportItem>,
    hiding: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct QualifiedImport {
    module_name: ModuleName,
    pub file_id: FileId,
    qualified_as: ModuleName,
    import_list: Option<ImportList>,
}

impl QualifiedImport {
    pub fn is_value_imported(&self, v: impl AsRef<str>) -> bool {
        match &self.import_list {
            Some(import_list) => {
                let is_member = import_list.items.iter().any(|import_item| match import_item {
                    ImportItem::ImportValue(i) => v.as_ref() == i.as_ref(),
                });
                if import_list.hiding {
                    !is_member
                } else {
                    is_member
                }
            }
            None => true,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnqualifiedImport {
    module_name: ModuleName,
    file_id: FileId,
}

#[derive(Debug, PartialEq, Eq)]
enum ImportDeclaration {
    Qualified(QualifiedImport),
    Unqualified(UnqualifiedImport),
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleImports {
    inner: Arena<ImportDeclaration>,
}

impl ModuleImports {
    pub(crate) fn module_imports_query(
        db: &dyn ResolverDatabase,
        file_id: FileId,
    ) -> Arc<ModuleImports> {
        let module_map = db.module_map();
        let mut module_imports = ModuleImports::default();

        let node = db.parse_file(file_id);
        let import_declarations = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.imports()?.imports()?.children()));
        if let Some(import_declarations) = import_declarations {
            for import_declaration in import_declarations {
                module_imports.collect_import(db, &module_map, import_declaration);
            }
        }

        Arc::new(module_imports)
    }

    fn collect_import(
        &mut self,
        db: &dyn ResolverDatabase,
        module_map: &ModuleMap,
        import_declaration: ast::ImportDeclaration,
    ) -> Option<()> {
        let module_name = import_declaration.module_name()?.in_db(db)?;
        let file_id = module_map.file_id(&module_name);
        let qualified_as = import_declaration
            .import_qualified()
            .and_then(|qualified| qualified.module_name()?.in_db(db));

        let import_declaration = if let Some(qualified_as) = qualified_as {
            let import_list = self.collect_import_list(db, import_declaration);
            ImportDeclaration::Qualified(QualifiedImport {
                module_name,
                file_id,
                qualified_as,
                import_list,
            })
        } else {
            ImportDeclaration::Unqualified(UnqualifiedImport { module_name, file_id })
        };

        self.inner.alloc(import_declaration);

        Some(())
    }

    fn collect_import_list(
        &self,
        db: &dyn ResolverDatabase,
        import_declaration: ast::ImportDeclaration,
    ) -> Option<ImportList> {
        let import_list = import_declaration.import_list()?;

        let hiding = import_list.hiding_token().is_some();
        let items = import_list
            .import_items()?
            .children()
            .map(|import_item| {
                Some(match import_item {
                    ast::ImportItem::ImportClass(_) => todo!(),
                    ast::ImportItem::ImportOp(_) => todo!(),
                    ast::ImportItem::ImportType(_) => todo!(),
                    ast::ImportItem::ImportTypeOp(_) => todo!(),
                    ast::ImportItem::ImportValue(i) => {
                        ImportItem::ImportValue(i.name_ref()?.in_db(db)?)
                    }
                })
            })
            .collect::<Option<_>>()?;

        Some(ImportList { items, hiding })
    }

    pub fn unqualified_imports(&self) -> impl Iterator<Item = &UnqualifiedImport> {
        self.inner.iter().filter_map(|(_, import)| match import {
            ImportDeclaration::Qualified(_) => None,
            ImportDeclaration::Unqualified(i) => Some(i),
        })
    }

    pub fn find_qualified(&self, prefix: &ModuleName) -> Option<&QualifiedImport> {
        self.inner.iter().find_map(|(_, import)| match import {
            ImportDeclaration::Qualified(q) => {
                if &q.qualified_as == prefix {
                    Some(q)
                } else {
                    None
                }
            }
            ImportDeclaration::Unqualified(_) => None,
        })
    }
}
