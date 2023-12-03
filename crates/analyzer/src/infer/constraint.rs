//! Type definitions for the constraint system.

use crate::{id::InFile, resolver::ValueGroupId};

use super::{TypeId, Unification};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    CompleteFirst(InFile<ValueGroupId>),
    UnifyDeep(TypeId, TypeId),
    UnifySolve(Unification, TypeId),
}
