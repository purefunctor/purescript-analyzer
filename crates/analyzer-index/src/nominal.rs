//! An index from names to stable IDs.

use std::{borrow::Borrow, sync::Arc};

use analyzer_interner::InDb;
use files::FileId;
use itertools::Itertools;
use la_arena::{Arena, Idx};
use rowan::ast::{AstChildren, AstNode};
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    IndexDatabase,
};

use super::PositionalMap;

/// Information about class declarations.
#[derive(Debug, PartialEq, Eq)]
pub struct ClassGroup {
    /// The name of the class declaration.
    pub name: Arc<str>,
    /// The signature of the declaration.
    pub signature: Option<AstId<ast::ClassSignature>>,
    /// The class declaration itself.
    pub declaration: AstId<ast::ClassDeclaration>,
    /// Members inside the declaration.
    pub members: FxHashMap<Arc<str>, AstId<ast::ClassMember>>,
}

pub type ClassGroupId = Idx<ClassGroup>;

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

/// Information about instance chains.
#[derive(Debug, PartialEq, Eq)]
pub struct InstanceChain {
    /// The name of the class.
    pub class_name: Arc<str>,
    /// The instances in a chain.
    pub instances: Vec<InstanceDeclaration>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceDeclaration {
    /// The ID of the instance.
    pub id: AstId<ast::InstanceDeclaration>,
    /// The members of the instance.
    pub members: FxHashMap<Arc<str>, Vec<InstanceMemberGroup>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceMemberGroup {
    /// The signature declaration.
    pub signature: Option<AstId<ast::InstanceMemberSignature>>,
    /// The equation declarations.
    pub equations: Vec<AstId<ast::InstanceMemberEquation>>,
}

pub type InstanceChainId = Idx<InstanceChain>;

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
    groups: Groups,
    name_to: NameTo,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct Groups {
    class: Arena<ClassGroup>,
    data: Arena<DataGroup>,
    instance: Arena<InstanceChain>,
    value: Arena<ValueGroup>,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct NameTo {
    class: FxHashMap<Arc<str>, ClassGroupId>,
    class_member: FxHashMap<Arc<str>, (ClassGroupId, AstId<ast::ClassMember>)>,
    data: FxHashMap<Arc<str>, DataGroupId>,
    data_constructor: FxHashMap<Arc<str>, (DataGroupId, AstId<ast::DataConstructor>)>,
    value: FxHashMap<Arc<str>, ValueGroupId>,
}

impl NominalMap {
    fn new(file_id: FileId) -> NominalMap {
        let groups = Groups::default();
        let name_to = NameTo::default();
        NominalMap { file_id, groups, name_to }
    }

    pub fn class_id(&self, name: impl Borrow<str>) -> Option<InFile<ClassGroupId>> {
        self.name_to.class.get(name.borrow()).map(|id| InFile { file_id: self.file_id, value: *id })
    }

    pub fn iter_class_group(&self) -> impl Iterator<Item = (InFile<ClassGroupId>, &ClassGroup)> {
        self.groups
            .class
            .iter()
            .map(|(id, class)| (InFile { file_id: self.file_id, value: id }, class))
    }

    pub fn data_id(&self, name: impl Borrow<str>) -> Option<InFile<DataGroupId>> {
        self.name_to.data.get(name.borrow()).map(|id| InFile { file_id: self.file_id, value: *id })
    }

    pub fn constructor_id(
        &self,
        name: impl Borrow<str>,
    ) -> Option<(InFile<DataGroupId>, InFile<AstId<ast::DataConstructor>>)> {
        self.name_to.data_constructor.get(name.borrow()).copied().map(
            |(group_id, constructor_id)| {
                let group_id = InFile { file_id: self.file_id, value: group_id };
                let constructor_id = InFile { file_id: self.file_id, value: constructor_id };
                (group_id, constructor_id)
            },
        )
    }

    pub fn data_group(&self, id: InFile<DataGroupId>) -> &DataGroup {
        &self.groups.data[id.value]
    }

    pub fn iter_data_group(&self) -> impl Iterator<Item = (InFile<DataGroupId>, &DataGroup)> {
        self.groups
            .data
            .iter()
            .map(|(id, data)| (InFile { file_id: self.file_id, value: id }, data))
    }

    pub fn value_id(&self, name: impl Borrow<str>) -> Option<InFile<ValueGroupId>> {
        self.name_to.value.get(name.borrow()).map(|id| InFile { file_id: self.file_id, value: *id })
    }

    pub fn value_group(&self, id: InFile<ValueGroupId>) -> &ValueGroup {
        &self.groups.value[id.value]
    }

    pub fn iter_value_group(&self) -> impl Iterator<Item = (InFile<ValueGroupId>, &ValueGroup)> {
        self.groups
            .value
            .iter()
            .map(|(id, data)| (InFile { file_id: self.file_id, value: id }, data))
    }
}

pub(super) fn nominal_map_query(db: &dyn IndexDatabase, file_id: FileId) -> Arc<NominalMap> {
    let mut nominal_map = NominalMap::new(file_id);

    let node = db.parse_file(file_id);
    let declarations = ast::Source::<ast::Module>::cast(node)
        .and_then(|source| Some(source.child()?.body()?.children()));

    if let Some(declarations) = declarations {
        let positional_map = db.positional_map(file_id);
        collect(db, &mut nominal_map, &positional_map, declarations);
    }

    Arc::new(nominal_map)
}

#[derive(Debug, PartialEq, Eq)]
enum DeclarationKey {
    Class(Option<Arc<str>>),
    Data(Option<Arc<str>>),
    Instance,
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

    let groups = declarations.chunk_by(|declaration| match declaration {
        ast::Declaration::ClassDeclaration(class) => {
            DeclarationKey::Class(class.name().and_then(|name| name.in_db(db)))
        }
        ast::Declaration::ClassSignature(class) => {
            DeclarationKey::Class(class.name().and_then(|name| name.in_db(db)))
        }
        ast::Declaration::DataAnnotation(data) => {
            DeclarationKey::Data(data.name().and_then(|name| name.in_db(db)))
        }
        ast::Declaration::DataDeclaration(data) => {
            DeclarationKey::Data(data.name().and_then(|name| name.in_db(db)))
        }
        ast::Declaration::InstanceChain(_) => DeclarationKey::Instance,
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
            DeclarationKey::Class(name) => {
                collect_class(db, nominal_map, positional_map, name, group)?;
            }
            DeclarationKey::Data(name) => {
                collect_data(db, nominal_map, positional_map, name, group)?;
            }
            DeclarationKey::Instance => {
                collect_instances(db, nominal_map, positional_map, group)?;
            }
            DeclarationKey::Value(name) => {
                collect_value(nominal_map, positional_map, name, group)?;
            }
        }
    }

    Some(())
}

fn collect_class(
    db: &dyn IndexDatabase,
    nominal_map: &mut NominalMap,
    positional_map: &PositionalMap,
    name: Option<Arc<str>>,
    group: impl Iterator<Item = ast::Declaration>,
) -> Option<()> {
    let name = name?;
    let mut group = group;

    let mut signature = None;

    let mut declaration = group.next()?;
    if let ast::Declaration::ClassSignature(s) = &declaration {
        signature = Some(positional_map.ast_id(s));
        declaration = group.next()?;
    }

    if let ast::Declaration::ClassDeclaration(d) = &declaration {
        let declaration = positional_map.ast_id(d);
        let mut members = FxHashMap::default();

        if let Some(class_members) = d.members() {
            for member_ast in class_members.children() {
                let member_name = member_ast.name()?.in_db(db)?;
                members.insert(member_name, positional_map.ast_id(&member_ast));
            }
        }

        let class_name = Arc::clone(&name);
        let class_group_id =
            nominal_map.groups.class.alloc(ClassGroup { name, signature, declaration, members });

        let class_group = &nominal_map.groups.class[class_group_id];

        nominal_map.name_to.class.insert(class_name, class_group_id);
        for (member_name, member_id) in class_group.members.iter() {
            nominal_map
                .name_to
                .class_member
                .insert(Arc::clone(member_name), (class_group_id, *member_id));
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

        if let Some(data_constructors) = d.constructors() {
            for constructor_ast in data_constructors.children() {
                let constructor_name = constructor_ast.name()?.in_db(db)?;
                constructors.insert(constructor_name, positional_map.ast_id(&constructor_ast));
            }
        }

        let data_name = Arc::clone(&name);
        let data_group_id =
            nominal_map.groups.data.alloc(DataGroup { name, annotation, equation, constructors });

        let data_group = &nominal_map.groups.data[data_group_id];

        nominal_map.name_to.data.insert(data_name, data_group_id);
        for (constructor_name, constructor_id) in data_group.constructors.iter() {
            nominal_map
                .name_to
                .data_constructor
                .insert(Arc::clone(constructor_name), (data_group_id, *constructor_id));
        }
    } else {
        unreachable!("Impossible.");
    }

    Some(())
}

fn collect_instances(
    db: &dyn IndexDatabase,
    nominal_map: &mut NominalMap,
    positional_map: &PositionalMap,
    group: impl Iterator<Item = ast::Declaration>,
) -> Option<()> {
    for declaration in group {
        let ast::Declaration::InstanceChain(chain) = declaration else {
            unreachable!("impossible:");
        };
        collect_instance_chain(db, nominal_map, positional_map, chain);
    }
    Some(())
}

fn collect_instance_chain(
    db: &dyn IndexDatabase,
    nominal_map: &mut NominalMap,
    positional_map: &PositionalMap,
    chain: ast::InstanceChain,
) -> Option<()> {
    let mut instances = chain.declarations();
    let mut collected = vec![];

    let Some(initial_instance) = instances.next() else {
        unreachable!("impossible: should have at least one instance.");
    };
    let initial_name = initial_instance.class_name()?.name()?.in_db(db)?;
    collected.push(collect_single_instance(db, positional_map, initial_instance)?);

    for current_instance in instances {
        let current_name = current_instance.class_name()?.name()?.in_db(db)?;
        if current_name != initial_name {
            return None;
        }
        collected.push(collect_single_instance(db, positional_map, current_instance)?);
    }

    nominal_map
        .groups
        .instance
        .alloc(InstanceChain { class_name: initial_name, instances: collected });

    Some(())
}

fn collect_single_instance(
    db: &dyn IndexDatabase,
    positional_map: &PositionalMap,
    instance: ast::InstanceDeclaration,
) -> Option<InstanceDeclaration> {
    let id = positional_map.ast_id(&instance);

    let grouped = instance.members()?.children().chunk_by(|member| match member {
        ast::InstanceMember::InstanceMemberEquation(e) => e.name().and_then(|name| name.in_db(db)),
        ast::InstanceMember::InstanceMemberSignature(s) => s.name().and_then(|name| name.in_db(db)),
    });

    let mut collected: FxHashMap<_, Vec<_>> = FxHashMap::default();
    for (name, mut grouped_members) in grouped.into_iter() {
        let name = name?;
        let mut signature = None;
        let mut equations = vec![];

        match grouped_members.next()? {
            ast::InstanceMember::InstanceMemberEquation(e) => {
                equations.push(positional_map.ast_id(&e));
            }
            ast::InstanceMember::InstanceMemberSignature(s) => {
                signature = Some(positional_map.ast_id(&s));
            }
        }

        equations.extend(grouped_members.filter_map(|member| {
            if let ast::InstanceMember::InstanceMemberEquation(e) = member {
                Some(positional_map.ast_id(&e))
            } else {
                None
            }
        }));

        collected.entry(name).or_default().push(InstanceMemberGroup { signature, equations });
    }

    Some(InstanceDeclaration { id, members: collected })
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
    let value_index = nominal_map.groups.value.alloc(ValueGroup { name, annotation, equations });

    nominal_map.name_to.value.insert(value_name, value_index);

    Some(())
}
