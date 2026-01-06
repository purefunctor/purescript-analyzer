//! Implements the core structures used in the type checker.

pub mod debruijn;
pub mod pretty;

use std::sync::Arc;

use files::FileId;
use indexing::{InstanceChainId, TypeItemId};
use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ForallBinder {
    pub visible: bool,
    pub name: SmolStr,
    pub level: debruijn::Level,
    pub kind: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variable {
    Implicit(debruijn::Level),
    Skolem(debruijn::Level, TypeId),
    Bound(debruijn::Level),
    Free(SmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RowField {
    pub label: SmolStr,
    pub id: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RowType {
    /// A stable-sorted list representing `Map<Label, NonEmptyList<Type>>`.
    pub fields: Arc<[RowField]>,
    /// Closed row if [`None`]; Open row if [`Some`].
    ///
    /// The [`TypeId`] is typically a [`Type::Variable`].
    pub tail: Option<TypeId>,
}

impl RowType {
    pub fn from_unsorted(mut fields: Vec<RowField>, tail: Option<TypeId>) -> RowType {
        fields.sort_by(|a, b| a.label.cmp(&b.label));
        RowType { fields: Arc::from(fields), tail }
    }

    pub fn closed(fields: Vec<RowField>) -> RowType {
        RowType::from_unsorted(fields, None)
    }

    pub fn empty() -> RowType {
        RowType { fields: Arc::from([]), tail: None }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Saturation {
    Full,
    Partial,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Application(TypeId, TypeId),
    Constrained(TypeId, TypeId),
    Constructor(FileId, TypeItemId),
    Forall(ForallBinder, TypeId),
    Function(TypeId, TypeId),
    Integer(i32),
    KindApplication(TypeId, TypeId),
    Kinded(TypeId, TypeId),
    Operator(FileId, TypeItemId),
    OperatorApplication(FileId, TypeItemId, TypeId, TypeId),
    Row(RowType),
    String(lowering::StringKind, SmolStr),
    SynonymApplication(Saturation, FileId, TypeItemId, Arc<[TypeId]>),
    Unification(u32),
    Variable(Variable),
    Unknown,
}

pub type TypeId = interner::Id<Type>;

pub type TypeInterner = interner::Interner<Type>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Operator {
    pub associativity: lowering::Associativity,
    pub precedence: u8,
    pub file_id: FileId,
    pub type_id: TypeItemId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Synonym {
    pub quantified_variables: debruijn::Size,
    pub kind_variables: debruijn::Size,
    pub type_variables: debruijn::Size,
    pub synonym_type: TypeId,
}

impl Synonym {
    pub fn has_arguments(&self) -> bool {
        self.type_variables != debruijn::Size(0)
    }

    pub fn with_synonym_type(mut self, synonym_type: TypeId) -> Synonym {
        self.synonym_type = synonym_type;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instance {
    pub arguments: Vec<(TypeId, TypeId)>,
    pub constraints: Vec<(TypeId, TypeId)>,
    pub resolution: (FileId, TypeItemId),
    pub chain_id: InstanceChainId,
    pub chain_position: u32,
    pub kind_variables: debruijn::Size,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub superclasses: Arc<[(TypeId, TypeId)]>,
    pub type_variable_kinds: Vec<TypeId>,
    pub quantified_variables: debruijn::Size,
    pub kind_variables: debruijn::Size,
}
