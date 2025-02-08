use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::source::*;

#[derive(Debug, PartialEq, Eq)]
pub enum TermItem {
    ClassMember { id: ClassMemberId },
    Constructor { id: DataConstructorId },
    Derive { id: DeriveId },
    Foreign { id: ForeignValueId },
    Instance { id: InstanceId },
    Operator { id: InfixId },
    Value { signature: Option<ValueSignatureId>, equations: Vec<ValueEquationId> },
}

pub type TermItemId = Idx<TermItem>;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeItem {
    Data {
        signature: Option<DataSignatureId>,
        equation: Option<DataEquationId>,
        role: Option<TypeRoleId>,
    },
    Newtype {
        signature: Option<NewtypeSignatureId>,
        equation: Option<NewtypeEquationId>,
        role: Option<TypeRoleId>,
    },
    Synonym {
        signature: Option<TypeSignatureId>,
        equation: Option<TypeEquationId>,
    },
    Class {
        signature: Option<ClassSignatureId>,
        declaration: Option<ClassDeclarationId>,
    },
    Foreign {
        id: ForeignDataId,
    },
    Operator {
        id: InfixId,
    },
}

pub type TypeItemId = Idx<TypeItem>;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Index {
    term_item: Arena<TermItem>,
    type_item: Arena<TypeItem>,
    term_nominal: FxHashMap<SmolStr, TermItemId>,
    type_nominal: FxHashMap<SmolStr, TypeItemId>,
    data_relational: Vec<(TypeItemId, DataConstructorId)>,
    class_relational: Vec<(TypeItemId, ClassMemberId)>,
    chain_relational: Vec<(InstanceChainId, InstanceId)>,
    instance_relational: Vec<(InstanceId, InstanceMemberId)>,
}

impl Index {
    pub(crate) fn insert_term_item(&mut self, k: Option<SmolStr>, v: TermItem) -> TermItemId {
        let id = self.term_item.alloc(v);
        let Some(k) = k else { return id };
        self.term_nominal.insert(k, id);
        id
    }

    pub(crate) fn term_item_mut(&mut self, k: &str) -> Option<(&mut TermItem, TermItemId)> {
        let &id = self.term_nominal.get(k)?;
        Some((&mut self.term_item[id], id))
    }

    pub(crate) fn insert_type_item(&mut self, k: Option<SmolStr>, v: TypeItem) -> TypeItemId {
        let id = self.type_item.alloc(v);
        let Some(k) = k else { return id };
        self.type_nominal.insert(k, id);
        id
    }

    pub(crate) fn type_item_mut(&mut self, k: &str) -> Option<(&mut TypeItem, TypeItemId)> {
        let &id = self.type_nominal.get(k)?;
        Some((&mut self.type_item[id], id))
    }

    pub(crate) fn insert_data_relation(&mut self, item_id: TypeItemId, id: DataConstructorId) {
        self.data_relational.push((item_id, id));
    }

    pub(crate) fn insert_class_relation(&mut self, item_id: TypeItemId, id: ClassMemberId) {
        self.class_relational.push((item_id, id));
    }

    pub(crate) fn insert_chain_relation(&mut self, c_id: InstanceChainId, i_id: InstanceId) {
        self.chain_relational.push((c_id, i_id));
    }

    pub(crate) fn insert_instance_relation(&mut self, i_id: InstanceId, m_id: InstanceMemberId) {
        self.instance_relational.push((i_id, m_id));
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemKind {
    DataSignature(DataSignatureId),
    DataEquation(DataEquationId),

    NewtypeSignature(NewtypeSignatureId),
    NewtypeEquation(NewtypeEquationId),

    Constructor(DataConstructorId),

    SynonymSignature(TypeSignatureId),
    SynonymEquation(TypeEquationId),

    ClassSignature(ClassSignatureId),
    ClassDeclaration(ClassDeclarationId),
    ClassMember(ClassMemberId),

    ValueSignature(ValueSignatureId),
    ValueEquation(ValueEquationId),

    ForeignData(ForeignDataId),
    ForeignValue(ForeignValueId),
    Operator(InfixId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExistingKind {
    Term(TermItemId),
    Type(TypeItemId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum IndexError {
    DuplicateItem { kind: ItemKind, existing: ExistingKind },
    MismatchedItem { kind: ItemKind, existing: ExistingKind },
    InvalidRole { id: TypeRoleId, existing: Option<TypeItemId> },
}
