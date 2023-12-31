//! Type definitions for the constraint system.

use super::{TypeId, Unification};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    UnifyDeep(Unification, Unification),
    UnifySolve(Unification, TypeId),
}
