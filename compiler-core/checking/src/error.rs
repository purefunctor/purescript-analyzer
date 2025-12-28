//! Implements the errors emitted by the type checker.

use std::sync::Arc;

use crate::TypeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorStep {
    TermDeclaration(indexing::TermItemId),
    TypeDeclaration(indexing::TypeItemId),
    ConstructorArgument(lowering::TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    TypeSignatureVariableMismatch { id: lowering::TypeId, expected: u32, actual: u32 },
    CannotUnify { t1: TypeId, t2: TypeId },
    PartialSynonymApplication { id: lowering::TypeId },
    InvalidTypeOperator { id: TypeId },
    RecursiveSynonymExpansion { file_id: files::FileId, item_id: indexing::TypeItemId },
    TooManyBinders { signature: lowering::TypeId, expected: u32, actual: u32 },
    NoInstanceFound { constraint: TypeId },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CheckError {
    pub kind: ErrorKind,
    pub step: Arc<[ErrorStep]>,
}
