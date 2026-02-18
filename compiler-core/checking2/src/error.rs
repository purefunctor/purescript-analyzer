//! Implements the errors emitted by the type checker.

use std::sync::Arc;

use smol_str::SmolStr;

use crate::core::SmolStrId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCrumb {
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
    CheckingAdoLet(lowering::DoStatementId),

    CheckingLetName(lowering::LetBindingNameGroupId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    AmbiguousConstraint {
        constraint: SmolStrId,
    },
    CannotDeriveClass {
        class_file: files::FileId,
        class_id: indexing::TypeItemId,
    },
    CannotDeriveForType {
        type_message: SmolStrId,
    },
    ContravariantOccurrence {
        type_message: SmolStrId,
    },
    CovariantOccurrence {
        type_message: SmolStrId,
    },
    CannotUnify {
        t1: SmolStrId,
        t2: SmolStrId,
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
    InvalidFinalBind,
    InvalidFinalLet,
    InstanceHeadMismatch {
        class_file: files::FileId,
        class_item: indexing::TypeItemId,
        expected: usize,
        actual: usize,
    },
    InstanceHeadLabeledRow {
        class_file: files::FileId,
        class_item: indexing::TypeItemId,
        position: usize,
        type_message: SmolStrId,
    },
    InstanceMemberTypeMismatch {
        expected: SmolStrId,
        actual: SmolStrId,
    },
    InvalidTypeApplication {
        function_type: SmolStrId,
        function_kind: SmolStrId,
        argument_type: SmolStrId,
    },
    InvalidTypeOperator {
        kind_message: SmolStrId,
    },
    ExpectedNewtype {
        type_message: SmolStrId,
    },
    InvalidNewtypeDeriveSkolemArguments,
    NoInstanceFound {
        constraint: SmolStrId,
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
        message_id: SmolStrId,
    },
    RedundantPatterns {
        patterns: Arc<[SmolStr]>,
    },
    MissingPatterns {
        patterns: Arc<[SmolStrId]>,
    },
    CustomFailure {
        message_id: SmolStrId,
    },
    PropertyIsMissing {
        labels: Arc<[SmolStr]>,
    },
    AdditionalProperty {
        labels: Arc<[SmolStr]>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CheckError {
    pub kind: ErrorKind,
    pub crumbs: Arc<[ErrorCrumb]>,
}
