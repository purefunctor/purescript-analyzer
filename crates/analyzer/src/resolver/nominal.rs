//! See documentation for [`NominalMap`].

use std::sync::Arc;

use files::FileId;
use la_arena::{Arena, Idx};
use rowan::ast::AstNode;
use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    ResolverDatabase,
};

#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroupData {
    pub name: SmolStr,
    pub annotation: Option<AstId<ast::ValueAnnotationDeclaration>>,
    pub values: FxHashSet<AstId<ast::ValueDeclaration>>,
}

pub type ValueGroupId = Idx<ValueGroupData>;

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
#[derive(Debug, Default, PartialEq, Eq)]
pub struct NominalMap {
    value_groups: Arena<ValueGroupData>,
    values: FxHashMap<SmolStr, InFile<ValueGroupId>>,
}

impl NominalMap {
    pub(crate) fn nominal_map_query(db: &dyn ResolverDatabase, file_id: FileId) -> Arc<NominalMap> {
        let mut collector = Collector::new(db, file_id);

        let node = db.parse_file(file_id);
        let declarations = ast::Source::<ast::Module>::cast(node)
            .and_then(|source| Some(source.child()?.body()?.declarations()?.children()));
        if let Some(declarations) = declarations {
            for declaration in declarations {
                collector.collect_declaration(&declaration);
            }
        }

        Arc::new(collector.into_inner())
    }

    pub fn value_group_id(&self, name: impl AsRef<str>) -> Option<InFile<ValueGroupId>> {
        self.values.get(name.as_ref()).copied()
    }

    pub fn value_group_data(&self, id: InFile<ValueGroupId>) -> &ValueGroupData {
        &self.value_groups[id.value]
    }
}

/// A state machine for collecting [`NominalMap`] entries.
struct Collector<'a> {
    db: &'a dyn ResolverDatabase,
    file_id: FileId,
    inner: NominalMap,
    state: CollectorState,
}

/// The current state for the [`Collector`].
#[derive(Default, Debug)]
enum CollectorState {
    #[default]
    Initial,
    ValueGroup(ValueGroupData),
}

impl<'a> Collector<'a> {
    fn new(db: &'a dyn ResolverDatabase, file_id: FileId) -> Collector<'a> {
        let inner = NominalMap::default();
        let state = CollectorState::default();
        Collector { db, file_id, inner, state }
    }

    fn into_inner(mut self) -> NominalMap {
        match self.state {
            CollectorState::ValueGroup(value_group) => {
                let name = value_group.name.clone();
                let index = self.inner.value_groups.alloc(value_group);
                self.inner.values.insert(name, InFile { file_id: self.file_id, value: index });
            }
            CollectorState::Initial => (),
        }
        self.inner
    }

    fn collect_declaration(&mut self, declaration: &ast::Declaration) -> Option<()> {
        match declaration {
            ast::Declaration::DataDeclaration(_) => todo!(),
            ast::Declaration::ForeignDataDeclaration(_) => todo!(),
            ast::Declaration::ValueAnnotationDeclaration(annotation) => {
                let name = annotation.name()?.as_str()?;
                match &self.state {
                    CollectorState::ValueGroup(current_group) if name == current_group.name => {
                        panic!("Invalid position for a value annotation!");
                    }
                    _ => {
                        let annotation =
                            Some(self.db.positional_map(self.file_id).ast_id(annotation));
                        let initial_value = None;
                        self.allocate_value_group(name, annotation, initial_value)
                    }
                }
            }
            ast::Declaration::ValueDeclaration(value) => {
                let name = value.name()?.as_str()?;
                match &mut self.state {
                    CollectorState::ValueGroup(current_group) if name == current_group.name => {
                        current_group
                            .values
                            .insert(self.db.positional_map(self.file_id).ast_id(value));
                    }
                    _ => {
                        let annotation = None;
                        let initial_value =
                            Some(self.db.positional_map(self.file_id).ast_id(value));
                        self.allocate_value_group(name, annotation, initial_value);
                    }
                }
            }
        }
        Some(())
    }

    fn allocate_value_group(
        &mut self,
        name: SmolStr,
        annotation: Option<AstId<ast::ValueAnnotationDeclaration>>,
        initial_value: Option<AstId<ast::ValueDeclaration>>,
    ) {
        let mut values = FxHashSet::default();
        if let Some(initial_value) = initial_value {
            values.insert(initial_value);
        }
        let present_value_group = ValueGroupData { name, annotation, values };
        let past_state =
            std::mem::replace(&mut self.state, CollectorState::ValueGroup(present_value_group));
        if let CollectorState::ValueGroup(past_value_group) = past_state {
            let past_name = past_value_group.name.clone();
            let index = self.inner.value_groups.alloc(past_value_group);
            self.inner.values.insert(past_name, InFile { file_id: self.file_id, value: index });
        }
    }
}
