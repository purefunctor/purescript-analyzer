use super::{TypeId, Unification};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    UnifyDeep(TypeId, TypeId),
    UnifySolve(Unification, TypeId),
}
