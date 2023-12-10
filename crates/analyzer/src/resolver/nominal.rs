//! See documentation for [`NominalMap`].

use std::sync::Arc;

use files::FileId;
use itertools::Itertools;
use la_arena::{Arena, Idx};
use rowan::ast::{AstChildren, AstNode};
use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    ResolverDatabase,
};

use super::PositionalMap;

#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroup {
    pub name: SmolStr,
    pub annotation: Option<AstId<ast::ValueAnnotationDeclaration>>,
    pub equations: FxHashSet<AstId<ast::ValueEquationDeclaration>>,
}

pub type ValueGroupId = Idx<ValueGroup>;

/// Maps names to stable IDs.
///
/// The nominal map is built by traversing the source file and mapping names
/// of items such as constructors, values, etc. to their [`AstId`]s. This is
/// particularly useful for module-local name resolution, with [`Exports`]
/// being one of its primary dependents.
///
/// Likewise, it also takes care of items that exist in "groups" such as
/// value declarations, which are usually associated with their siblings
/// (for "equational-style" declarations) or type annotations.
///
/// [`Exports`]: crate::resolver::Exports
#[derive(Debug, PartialEq, Eq)]
pub struct NominalMap {
    file_id: FileId,
    value_groups: Arena<ValueGroup>,
    name_to_value: FxHashMap<SmolStr, ValueGroupId>,
}

impl NominalMap {
    fn new(file_id: FileId) -> NominalMap {
        let value_groups = Arena::default();
        let name_to_value = FxHashMap::default();
        NominalMap { file_id, value_groups, name_to_value }
    }

    pub(crate) fn nominal_map_query(db: &dyn ResolverDatabase, file_id: FileId) -> Arc<NominalMap> {
        let mut nominal_map = NominalMap::new(file_id);

        let node = db.parse_file(file_id);
        let declarations = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.body()?.declarations()?.children()));

        if let Some(declarations) = declarations {
            let positional_map = db.positional_map(file_id);
            collect(&mut nominal_map, &positional_map, declarations);
        }

        Arc::new(nominal_map)
    }

    pub fn value_group_id(&self, name: impl AsRef<str>) -> Option<InFile<ValueGroupId>> {
        self.name_to_value
            .get(name.as_ref())
            .copied()
            .map(|id| InFile { file_id: self.file_id, value: id })
    }

    pub fn value_group_data(&self, id: InFile<ValueGroupId>) -> &ValueGroup {
        &self.value_groups[id.value]
    }

    pub fn value_groups(&self) -> impl Iterator<Item = (InFile<ValueGroupId>, &ValueGroup)> {
        self.value_groups
            .iter()
            .map(|(id, data)| (InFile { file_id: self.file_id, value: id }, data))
    }
}

#[derive(Debug, PartialEq, Eq)]
enum DeclarationKey {
    Data(Option<SmolStr>),
    Value(Option<SmolStr>),
}

fn collect(
    nominal_map: &mut NominalMap,
    positional_map: &PositionalMap,
    declarations: AstChildren<ast::Declaration>,
) -> Option<()> {
    let mut declarations = declarations.peekable();

    if declarations.peek().is_none() {
        return Some(());
    }

    let groups = declarations.group_by(|declaration| match declaration {
        ast::Declaration::DataDeclaration(data) => {
            DeclarationKey::Data(data.name().and_then(|name| name.as_str()))
        }
        ast::Declaration::ForeignDataDeclaration(_) => todo!("Unimplemented!"),
        ast::Declaration::ValueAnnotationDeclaration(value) => {
            DeclarationKey::Value(value.name().and_then(|name| name.as_str()))
        }
        ast::Declaration::ValueEquationDeclaration(value) => {
            DeclarationKey::Value(value.name().and_then(|name| name.as_str()))
        }
    });

    for (key, mut group) in groups.into_iter() {
        match key {
            DeclarationKey::Data(_) => (),
            DeclarationKey::Value(v) => {
                let name = v?;
                let mut annotation = None;
                let mut equations = FxHashSet::default();

                match group.next()? {
                    ast::Declaration::ValueAnnotationDeclaration(a) => {
                        annotation = Some(positional_map.ast_id(&a));
                    }
                    ast::Declaration::ValueEquationDeclaration(e) => {
                        equations.insert(positional_map.ast_id(&e));
                    }
                    _ => unreachable!("Impossible."),
                }

                equations.extend(group.filter_map(|declaration| {
                    if let ast::Declaration::ValueEquationDeclaration(e) = declaration {
                        Some(positional_map.ast_id(&e))
                    } else {
                        None
                    }
                }));

                let value_name = name.clone();
                let value_index =
                    nominal_map.value_groups.alloc(ValueGroup { name, annotation, equations });

                nominal_map.name_to_value.insert(value_name, value_index);
            }
        }
    }

    Some(())
}
