use std::sync::Arc;

use crate::TypeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorStep {
    TypeDeclaration(indexing::TypeItemId),
    ConstructorArgument(lowering::TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    TypeSignatureVariableMismatch { id: lowering::TypeId, expected: u32, actual: u32 },
    CannotUnify { t1: TypeId, t2: TypeId },
    PartialSynonymApplication { id: lowering::TypeId },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CheckError {
    pub kind: ErrorKind,
    pub step: Arc<[ErrorStep]>,
}
