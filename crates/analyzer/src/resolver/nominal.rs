//! See documentation for [`NominalMap`].

use std::sync::Arc;

use files::FileId;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    ResolverDatabase,
};

/// Maps names to [`AstId`]s.
///
/// The nominal map is built by traversing the source file and mapping names
/// of items such as constructors, values, etc. to their [`AstId`]s. This is
/// particularly useful for module-local name resolution, with [`Exports`]
/// being one of its primary dependents.
///
/// [`Exports`]: crate::resolver::Exports
#[derive(Debug, Default, PartialEq, Eq)]
pub struct NominalMap {
    annotation: FxHashMap<SmolStr, InFile<AstId<ast::AnnotationDeclaration>>>,
    foreign_data: FxHashMap<SmolStr, InFile<AstId<ast::ForeignDataDeclaration>>>,
    value: FxHashMap<SmolStr, Vec<InFile<AstId<ast::ValueDeclaration>>>>,
}

impl NominalMap {
    pub(crate) fn nominal_map_query(db: &dyn ResolverDatabase, file_id: FileId) -> Arc<NominalMap> {
        let mut nominal_map = NominalMap::default();

        let node = db.parse_file(file_id);
        let declarations = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.body()?.declarations()?.children()));
        if let Some(declarations) = declarations {
            for declaration in declarations {
                nominal_map.collect_declaration(db, file_id, &declaration);
            }
        }

        Arc::new(nominal_map)
    }

    fn collect_declaration(
        &mut self,
        db: &dyn ResolverDatabase,
        file_id: FileId,
        declaration: &ast::Declaration,
    ) -> Option<()> {
        match declaration {
            ast::Declaration::AnnotationDeclaration(annotation) => {
                let name = annotation.name()?.as_str()?;
                let id = db.positional_map(file_id).ast_id(annotation).in_file(file_id);
                self.annotation.insert(name, id);
            }
            ast::Declaration::ForeignDataDeclaration(data) => {
                let name = data.name()?.as_str()?;
                let id = db.positional_map(file_id).ast_id(data).in_file(file_id);
                self.foreign_data.insert(name, id);
            }
            ast::Declaration::ValueDeclaration(value) => {
                let name = value.name()?.as_str()?;
                let id = db.positional_map(file_id).ast_id(value).in_file(file_id);
                self.value.entry(name).or_default().push(id);
            }
        }

        Some(())
    }

    pub fn get_annotation(
        &self,
        name: impl AsRef<str>,
    ) -> Option<InFile<AstId<ast::AnnotationDeclaration>>> {
        self.annotation.get(name.as_ref()).copied()
    }

    pub fn get_foreign_data(
        &self,
        name: impl AsRef<str>,
    ) -> Option<InFile<AstId<ast::ForeignDataDeclaration>>> {
        self.foreign_data.get(name.as_ref()).copied()
    }

    pub fn get_value(
        &self,
        name: impl AsRef<str>,
    ) -> Option<Arc<[InFile<AstId<ast::ValueDeclaration>>]>> {
        self.value.get(name.as_ref()).map(|values| values.as_slice().into())
    }
}
