//! An index from names to stable IDs.

use std::{borrow::Borrow, sync::Arc};

use files::FileId;
use itertools::Itertools;
use la_arena::{Arena, Idx};
use rowan::ast::{AstChildren, AstNode};
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    interner::InDb,
    IndexDatabase,
};

use super::PositionalMap;

/// Information about data declarations.
#[derive(Debug, PartialEq, Eq)]
pub struct DataGroup {
    /// The name of the data declaration.
    pub name: Arc<str>,
    /// The annotation declaration.
    pub annotation: Option<AstId<ast::DataAnnotation>>,
    /// The equation declaration.
    pub equation: AstId<ast::DataDeclaration>,
    /// The constructors inside the equation.
    pub constructors: FxHashMap<Arc<str>, AstId<ast::DataConstructor>>,
}

pub type DataGroupId = Idx<DataGroup>;

/// Information about value declarations.
#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroup {
    /// The name of the value declaration.
    pub name: Arc<str>,
    /// The annotation declaration.
    pub annotation: Option<AstId<ast::ValueAnnotationDeclaration>>,
    /// The equation declarations.
    pub equations: Vec<AstId<ast::ValueEquationDeclaration>>,
}

pub type ValueGroupId = Idx<ValueGroup>;

/// An index from names to stable IDs.
///
/// This is particularly useful for module-local name resolution. This also
/// takes care of items that may have several associated declarations, such
/// is the case for annotations and equations.
#[derive(Debug, PartialEq, Eq)]
pub struct NominalMap {
    file_id: FileId,
    data_groups: Arena<DataGroup>,
    value_groups: Arena<ValueGroup>,
    name_to_data: FxHashMap<Arc<str>, DataGroupId>,
    name_to_constructor: FxHashMap<Arc<str>, (DataGroupId, AstId<ast::DataConstructor>)>,
    name_to_value: FxHashMap<Arc<str>, ValueGroupId>,
}

impl NominalMap {
    fn new(file_id: FileId) -> NominalMap {
        let data_groups = Default::default();
        let value_groups = Default::default();
        let name_to_data = Default::default();
        let name_to_constructor = Default::default();
        let name_to_value = Default::default();
        NominalMap {
            file_id,
            data_groups,
            value_groups,
            name_to_data,
            name_to_constructor,
            name_to_value,
        }
    }

    pub fn data_id(&self, name: impl Borrow<str>) -> Option<InFile<DataGroupId>> {
        self.name_to_data.get(name.borrow()).map(|id| InFile { file_id: self.file_id, value: *id })
    }

    pub fn constructor_id(
        &self,
        name: impl Borrow<str>,
    ) -> Option<(InFile<DataGroupId>, InFile<AstId<ast::DataConstructor>>)> {
        self.name_to_constructor.get(name.borrow()).copied().map(|(group_id, constructor_id)| {
            let group_id = InFile { file_id: self.file_id, value: group_id };
            let constructor_id = InFile { file_id: self.file_id, value: constructor_id };
            (group_id, constructor_id)
        })
    }

    pub fn data_group(&self, id: InFile<DataGroupId>) -> &DataGroup {
        &self.data_groups[id.value]
    }

    pub fn iter_data_group(&self) -> impl Iterator<Item = (InFile<DataGroupId>, &DataGroup)> {
        self.data_groups
            .iter()
            .map(|(id, data)| (InFile { file_id: self.file_id, value: id }, data))
    }

    pub fn value_id(&self, name: impl Borrow<str>) -> Option<InFile<ValueGroupId>> {
        self.name_to_value.get(name.borrow()).map(|id| InFile { file_id: self.file_id, value: *id })
    }

    pub fn value_group(&self, id: InFile<ValueGroupId>) -> &ValueGroup {
        &self.value_groups[id.value]
    }

    pub fn iter_value_group(&self) -> impl Iterator<Item = (InFile<ValueGroupId>, &ValueGroup)> {
        self.value_groups
            .iter()
            .map(|(id, data)| (InFile { file_id: self.file_id, value: id }, data))
    }
}

pub(super) fn nominal_map_query(db: &dyn IndexDatabase, file_id: FileId) -> Arc<NominalMap> {
    let mut nominal_map = NominalMap::new(file_id);

    let node = db.parse_file(file_id);
    let declarations = ast::Source::<ast::Module>::cast(node)
        .and_then(|source| Some(source.child()?.body()?.declarations()?.children()));

    if let Some(declarations) = declarations {
        let positional_map = db.positional_map(file_id);
        collect(db, &mut nominal_map, &positional_map, declarations);
    }

    Arc::new(nominal_map)
}

#[derive(Debug, PartialEq, Eq)]
enum DeclarationKey {
    Data(Option<Arc<str>>),
    Value(Option<Arc<str>>),
}

fn collect(
    db: &dyn IndexDatabase,
    nominal_map: &mut NominalMap,
    positional_map: &PositionalMap,
    declarations: AstChildren<ast::Declaration>,
) -> Option<()> {
    let mut declarations = declarations.peekable();

    if declarations.peek().is_none() {
        return Some(());
    }

    let groups = declarations.group_by(|declaration| match declaration {
        ast::Declaration::DataAnnotation(data) => {
            DeclarationKey::Data(data.name().and_then(|name| name.in_db(db)))
        }
        ast::Declaration::DataDeclaration(data) => {
            DeclarationKey::Data(data.name().and_then(|name| name.in_db(db)))
        }
        ast::Declaration::ForeignDataDeclaration(_) => todo!("Unimplemented!"),
        ast::Declaration::ValueAnnotationDeclaration(value) => {
            DeclarationKey::Value(value.name().and_then(|name| name.in_db(db)))
        }
        ast::Declaration::ValueEquationDeclaration(value) => {
            DeclarationKey::Value(value.name().and_then(|name| name.in_db(db)))
        }
    });

    for (key, group) in groups.into_iter() {
        match key {
            DeclarationKey::Data(name) => {
                collect_data(db, nominal_map, positional_map, name, group)?;
            }
            DeclarationKey::Value(name) => {
                collect_value(nominal_map, positional_map, name, group)?;
            }
        }
    }

    Some(())
}

fn collect_data(
    db: &dyn IndexDatabase,
    nominal_map: &mut NominalMap,
    positional_map: &PositionalMap,
    name: Option<Arc<str>>,
    group: impl Iterator<Item = ast::Declaration>,
) -> Option<()> {
    let name = name?;
    let mut group = group;

    let mut annotation = None;

    let mut declaration = group.next()?;
    if let ast::Declaration::DataAnnotation(a) = &declaration {
        annotation = Some(positional_map.ast_id(a));
        declaration = group.next()?;
    }

    if let ast::Declaration::DataDeclaration(d) = &declaration {
        let equation = positional_map.ast_id(d);
        let mut constructors = FxHashMap::default();

        for constructor_ast in d.constructors()?.children() {
            let constructor_name = constructor_ast.name()?.in_db(db)?;
            constructors.insert(constructor_name, positional_map.ast_id(&constructor_ast));
        }

        let data_name = Arc::clone(&name);
        let data_group_id =
            nominal_map.data_groups.alloc(DataGroup { name, annotation, equation, constructors });

        let data_group = &nominal_map.data_groups[data_group_id];

        nominal_map.name_to_data.insert(data_name, data_group_id);
        for (constructor_name, constructor_id) in data_group.constructors.iter() {
            nominal_map
                .name_to_constructor
                .insert(Arc::clone(constructor_name), (data_group_id, *constructor_id));
        }
    } else {
        unreachable!("Impossible.");
    }

    Some(())
}

fn collect_value(
    nominal_map: &mut NominalMap,
    positional_map: &PositionalMap,
    name: Option<Arc<str>>,
    group: impl Iterator<Item = ast::Declaration>,
) -> Option<()> {
    let name = name?;
    let mut group = group;

    let mut annotation = None;
    let mut equations = vec![];

    match group.next()? {
        ast::Declaration::ValueAnnotationDeclaration(a) => {
            annotation = Some(positional_map.ast_id(&a));
        }
        ast::Declaration::ValueEquationDeclaration(e) => {
            equations.push(positional_map.ast_id(&e));
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

    let value_name = Arc::clone(&name);
    let value_index = nominal_map.value_groups.alloc(ValueGroup { name, annotation, equations });

    nominal_map.name_to_value.insert(value_name, value_index);

    Some(())
}
