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

#[derive(Debug, Default, PartialEq, Eq)]
pub struct NominalMap {
    annotations: FxHashMap<SmolStr, InFile<AstId<ast::AnnotationDeclaration>>>,
    values: FxHashMap<SmolStr, Vec<InFile<AstId<ast::ValueDeclaration>>>>,
}

impl NominalMap {
    pub(crate) fn nominal_map_query(db: &dyn ResolverDatabase, file_id: FileId) -> Arc<NominalMap> {
        let mut nominal_map = NominalMap::default();

        let node = db.parse_file(file_id);
        let declarations = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.body()?.declarations()?.children()));
        if let Some(declarations) = declarations {
            for declaration in declarations {
                match declaration {
                    ast::Declaration::AnnotationDeclaration(annotation) => {
                        let name = annotation.name().unwrap().as_str().unwrap();
                        let id = db.positional_map(file_id).ast_id(&annotation).in_file(file_id);
                        nominal_map.annotations.insert(name, id);
                    }
                    ast::Declaration::ValueDeclaration(value) => {
                        let name = value.name().unwrap().as_str().unwrap();
                        let id = db.positional_map(file_id).ast_id(&value).in_file(file_id);
                        nominal_map.values.entry(name).or_default().push(id);
                    }
                }
            }
        }

        Arc::new(nominal_map)
    }
}
