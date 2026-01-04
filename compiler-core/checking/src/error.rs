//! Implements the errors emitted by the type checker.

use std::sync::Arc;

use crate::TypeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorStep {
    TermDeclaration(indexing::TermItemId),
    TypeDeclaration(indexing::TypeItemId),
    ConstructorArgument(lowering::TypeId),

    InferringKind(lowering::TypeId),
    CheckingKind(lowering::TypeId),

    InferringBinder(lowering::BinderId),
    CheckingBinder(lowering::BinderId),

    InferringExpression(lowering::ExpressionId),
    CheckingExpression(lowering::ExpressionId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    AmbiguousConstraint {
        constraint: TypeId,
    },
    CannotUnify {
        t1: TypeId,
        t2: TypeId,
    },
    EmptyAdoBlock,
    EmptyDoBlock,
    InstanceHeadMismatch {
        class_file: files::FileId,
        class_item: indexing::TypeItemId,
        expected: usize,
        actual: usize,
    },
    InstanceMemberTypeMismatch {
        expected: TypeId,
        actual: TypeId,
    },
    InvalidTypeOperator {
        id: TypeId,
    },
    NoInstanceFound {
        constraint: TypeId,
    },
    PartialSynonymApplication {
        id: lowering::TypeId,
    },
    RecursiveSynonymExpansion {
        file_id: files::FileId,
        item_id: indexing::TypeItemId,
    },
    TooManyBinders {
        signature: lowering::TypeId,
        expected: u32,
        actual: u32,
    },
    TypeSignatureVariableMismatch {
        id: lowering::TypeId,
        expected: u32,
        actual: u32,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CheckError {
    pub kind: ErrorKind,
    pub step: Arc<[ErrorStep]>,
}
