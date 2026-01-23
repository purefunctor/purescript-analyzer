//! Implements the errors emitted by the type checker.

use std::sync::Arc;

use interner::{Id, Interner};
use smol_str::SmolStr;

pub type TypeErrorMessageId = Id<SmolStr>;
pub type TypeErrorMessageInterner = Interner<SmolStr>;

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

    InferringDoBind(lowering::DoStatementId),
    InferringDoDiscard(lowering::DoStatementId),
    CheckingDoLet(lowering::DoStatementId),

    InferringAdoMap(lowering::DoStatementId),
    InferringAdoApply(lowering::DoStatementId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    AmbiguousConstraint {
        constraint: TypeErrorMessageId,
    },
    CannotDeriveClass {
        class_file: files::FileId,
        class_id: indexing::TypeItemId,
    },
    CannotDeriveForType {
        type_message: TypeErrorMessageId,
    },
    ContravariantOccurrence {
        type_message: TypeErrorMessageId,
    },
    CovariantOccurrence {
        type_message: TypeErrorMessageId,
    },
    CannotUnify {
        t1: TypeErrorMessageId,
        t2: TypeErrorMessageId,
    },
    DeriveInvalidArity {
        class_file: files::FileId,
        class_id: indexing::TypeItemId,
        expected: usize,
        actual: usize,
    },
    DeriveMissingFunctor,
    EmptyAdoBlock,
    EmptyDoBlock,
    InstanceHeadMismatch {
        class_file: files::FileId,
        class_item: indexing::TypeItemId,
        expected: usize,
        actual: usize,
    },
    InstanceMemberTypeMismatch {
        expected: TypeErrorMessageId,
        actual: TypeErrorMessageId,
    },
    InvalidTypeOperator {
        kind_message: TypeErrorMessageId,
    },
    ExpectedNewtype {
        type_message: TypeErrorMessageId,
    },
    NoInstanceFound {
        constraint: TypeErrorMessageId,
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
    InvalidRoleDeclaration {
        index: usize,
        declared: crate::core::Role,
        inferred: crate::core::Role,
    },
    CoercibleConstructorNotInScope {
        file_id: files::FileId,
        item_id: indexing::TypeItemId,
    },
    CustomWarning {
        message_id: TypeErrorMessageId,
    },
    CustomFailure {
        message_id: TypeErrorMessageId,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CheckError {
    pub kind: ErrorKind,
    pub step: Arc<[ErrorStep]>,
}
