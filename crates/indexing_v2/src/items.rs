use la_arena::Idx;
use smol_str::SmolStr;

use crate::source::*;

#[derive(Debug, PartialEq, Eq)]
pub struct TermItem {
    pub name: Option<SmolStr>,
    pub kind: TermItemKind,
    pub exported: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TermItemKind {
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
pub struct TypeItem {
    pub name: Option<SmolStr>,
    pub kind: TypeItemKind,
    pub exported: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeItemKind {
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
