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
    data: FxHashMap<SmolStr, InFile<AstId<ast::DataDeclaration>>>,
    constructor: FxHashMap<SmolStr, InFile<AstId<ast::DataConstructor>>>,
    foreign_data: FxHashMap<SmolStr, InFile<AstId<ast::ForeignDataDeclaration>>>,
    value: FxHashMap<SmolStr, Vec<InFile<AstId<ast::ValueDeclaration>>>>,
    constructor_to_data:
        FxHashMap<InFile<AstId<ast::DataConstructor>>, InFile<AstId<ast::DataDeclaration>>>,
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
            ast::Declaration::DataDeclaration(data) => {
                let name = data.name()?.as_str()?;
                let data_id = db.positional_map(file_id).ast_id(data).in_file(file_id);
                for constructor in data.constructors()?.children() {
                    self.collect_constructor(db, file_id, data_id, &constructor);
                }
                self.data.insert(name, data_id);
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

    fn collect_constructor(
        &mut self,
        db: &dyn ResolverDatabase,
        file_id: FileId,
        data_id: InFile<AstId<ast::DataDeclaration>>,
        constructor: &ast::DataConstructor,
    ) -> Option<()> {
        let name = constructor.name()?.as_str()?;
        let constructor_id = db.positional_map(file_id).ast_id(constructor).in_file(file_id);
        self.constructor.insert(name, constructor_id);
        self.constructor_to_data.insert(constructor_id, data_id);

        Some(())
    }

    pub fn get_annotation(
        &self,
        name: impl AsRef<str>,
    ) -> Option<InFile<AstId<ast::AnnotationDeclaration>>> {
        self.annotation.get(name.as_ref()).copied()
    }

    pub fn get_data(&self, name: impl AsRef<str>) -> Option<InFile<AstId<ast::DataDeclaration>>> {
        self.data.get(name.as_ref()).copied()
    }

    pub fn get_constructor(
        &self,
        name: impl AsRef<str>,
    ) -> Option<InFile<AstId<ast::DataConstructor>>> {
        self.constructor.get(name.as_ref()).copied()
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

    pub fn constructor_of(
        &self,
        id: InFile<AstId<ast::DataConstructor>>,
    ) -> InFile<AstId<ast::DataDeclaration>> {
        if let Some(id) = self.constructor_to_data.get(&id) {
            *id
        } else {
            panic!("Invariant violated: a constructor was not assigned to a declarations");
        }
    }
}
