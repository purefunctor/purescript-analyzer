//! ASTs for inference errors.

use std::sync::Arc;

use crate::{index::nominal::ValueGroupId, surface::ExprId};

use super::CoreTypeId;

/// Hints for the error's location.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Hint {
    ValueGroup(ValueGroupId),
    Expression(ExprId),
}

/// The kind of the inference error.
#[derive(Debug)]
pub enum InferErrorKind {
    CannotUnify(CoreTypeId, CoreTypeId),
}

/// An error encountered during inference.
#[derive(Debug)]
pub struct InferError {
    pub hints: Arc<[Hint]>,
    pub kind: InferErrorKind,
}
