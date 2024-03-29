//! ASTs for inference errors.

use std::sync::Arc;

use crate::{id::InFile, index::nominal::ValueGroupId, surface::ExprId};

use super::CoreTypeId;

/// Hints for the error's location.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hint {
    ValueGroup(ValueGroupId),
    Expression(ExprId),
}

/// The kind of the inference error.
#[derive(Debug, PartialEq, Eq)]
pub enum InferErrorKind {
    CannotUnify(CoreTypeId, CoreTypeId),
    OccursCheck(InFile<u32>, CoreTypeId),
}

/// An error encountered during inference.
#[derive(Debug, PartialEq, Eq)]
pub struct InferError {
    pub hints: Arc<[Hint]>,
    pub kind: InferErrorKind,
}
