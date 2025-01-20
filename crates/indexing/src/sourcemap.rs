use fxhash::FxBuildHasher;
use indexmap::IndexSet;
use rowan::ast::{AstNode, AstPtr};
use syntax::cst;

use crate::id::Id;

pub type ImportId = Id<cst::ImportStatement>;
pub type ImportPtr = AstPtr<cst::ImportStatement>;

pub type DeclarationId = Id<cst::Declaration>;
pub type DeclarationPtr = AstPtr<cst::Declaration>;

pub type ConstructorId = Id<cst::DataConstructor>;
pub type ConstructorPtr = AstPtr<cst::DataConstructor>;

pub type ClassMemberId = Id<cst::ClassMemberStatement>;
pub type ClassMemberPtr = AstPtr<cst::ClassMemberStatement>;

pub type InstanceId = Id<cst::InstanceDeclaration>;
pub type InstancePtr = AstPtr<cst::InstanceDeclaration>;

pub type InstanceMemberId = Id<cst::InstanceMemberStatement>;
pub type InstanceMemberPtr = AstPtr<cst::InstanceMemberStatement>;

/// Mapping from module items to stable IDs.
///
/// The `SourceMap` derives stable [`Id`] values from [`AstPtr`] in the CST.
///
/// Text ranges are highly volatile and may change even when the code retains
/// the same semantic structure. For example, adding a newline before a value
/// binding will change the text range of the value binding, but no semantic
/// information was changed.
///
/// As such, text ranges are highly unsuitable for associating information
/// to CST nodes. Instead, we derive IDs by traversing the CST and assiging
/// a unique ID to each node.
///
/// ```purescript
/// life :: Int   -- Declaration(0)
/// life = 42     -- Declaration(1)
///
/// data Maybe a  -- Declaration(3)
///   = Nothing   -- Constructor(0)
///   | Just a    -- Constructor(1)
/// ```
///
/// Stable IDs guarantee stability across non-semantic edits, as well as
/// semantic edits that append information onto the module.
///
/// ```purescript
/// -- This comment is non-semantic
/// life :: Int   -- Declaration(0)
/// life = 42     -- Declaration(1)
///
/// data Maybe a  -- Declaration(3)
///   = Nothing   -- Constructor(0)
///   | Just a    -- Constructor(1)
///
/// newtype Id a  -- Declaration(4)
///   = Id a      -- Constructor(2)
/// ```
///
/// Internally, the `SourceMap` is implemented using [`IndexSet`] with
/// [`AstPtr`] as its key. It could also be implemented as a [`Vec`]
/// backed by a SIMD-based lookup algorithm. For now, the current
/// implementation works well enough for the general case.
#[derive(Debug, Default)]
pub struct SourceMap {
    imports: FxIndexSet<ImportPtr>,
    declaration: FxIndexSet<DeclarationPtr>,
    constructor: FxIndexSet<ConstructorPtr>,
    class_member: FxIndexSet<ClassMemberPtr>,
    instance: FxIndexSet<InstancePtr>,
    instance_member: FxIndexSet<InstanceMemberPtr>,
}

type FxIndexSet<T> = IndexSet<T, FxBuildHasher>;

fn insert<T: AstNode>(m: &mut FxIndexSet<AstPtr<T>>, k: &T) -> Id<T> {
    let pointer = AstPtr::new(k);
    let index = m.insert_full(pointer).0;
    Id::from_raw(index)
}

fn ptr<T: AstNode>(m: &FxIndexSet<AstPtr<T>>, id: Id<T>) -> Option<AstPtr<T>> {
    m.get_index(id.index).cloned()
}

fn id<T: AstNode>(m: &FxIndexSet<AstPtr<T>>, ptr: AstPtr<T>) -> Option<Id<T>> {
    let index = m.get_full(&ptr)?.0;
    Some(Id::from_raw(index))
}

impl SourceMap {
    pub(crate) fn insert_import(&mut self, import: &cst::ImportStatement) -> ImportId {
        insert(&mut self.imports, import)
    }

    pub(crate) fn insert_declaration(&mut self, declaration: &cst::Declaration) -> DeclarationId {
        insert(&mut self.declaration, declaration)
    }

    pub(crate) fn insert_constructor(
        &mut self,
        constructor: &cst::DataConstructor,
    ) -> ConstructorId {
        insert(&mut self.constructor, constructor)
    }

    pub(crate) fn insert_class_member(
        &mut self,
        class_member: &cst::ClassMemberStatement,
    ) -> ClassMemberId {
        insert(&mut self.class_member, class_member)
    }

    pub(crate) fn insert_instance(&mut self, instance: &cst::InstanceDeclaration) -> InstanceId {
        insert(&mut self.instance, instance)
    }

    pub(crate) fn insert_instance_member(
        &mut self,
        instance_member: &cst::InstanceMemberStatement,
    ) -> InstanceMemberId {
        insert(&mut self.instance_member, instance_member)
    }
}

impl SourceMap {
    pub fn import_ptr(&self, id: ImportId) -> Option<ImportPtr> {
        ptr(&self.imports, id)
    }

    pub fn import_id(&self, ptr: ImportPtr) -> Option<ImportId> {
        id(&self.imports, ptr)
    }

    pub fn declaration_ptr(&self, id: DeclarationId) -> Option<DeclarationPtr> {
        ptr(&self.declaration, id)
    }

    pub fn declaration_id(&self, ptr: DeclarationPtr) -> Option<DeclarationId> {
        id(&self.declaration, ptr)
    }

    pub fn constructor_ptr(&self, id: ConstructorId) -> Option<ConstructorPtr> {
        ptr(&self.constructor, id)
    }

    pub fn constructor_id(&self, ptr: ConstructorPtr) -> Option<ConstructorId> {
        id(&self.constructor, ptr)
    }

    pub fn class_member_ptr(&self, id: ClassMemberId) -> Option<ClassMemberPtr> {
        ptr(&self.class_member, id)
    }

    pub fn class_member_id(&self, ptr: ClassMemberPtr) -> Option<ClassMemberId> {
        id(&self.class_member, ptr)
    }

    pub fn instance_ptr(&self, id: InstanceId) -> Option<InstancePtr> {
        ptr(&self.instance, id)
    }

    pub fn instance_id(&self, ptr: InstancePtr) -> Option<InstanceId> {
        id(&self.instance, ptr)
    }

    pub fn instance_member_ptr(&self, id: InstanceMemberId) -> Option<InstanceMemberPtr> {
        ptr(&self.instance_member, id)
    }

    pub fn instance_member_id(&self, ptr: InstanceMemberPtr) -> Option<InstanceMemberId> {
        id(&self.instance_member, ptr)
    }
}
