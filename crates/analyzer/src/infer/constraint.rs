//! Type definitions for the constraint system.

use syntax::ast;

use crate::id::{AstId, InFile};

use super::TypeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    UnifyDeep(TypeId, TypeId),
    UnifySolve(u32, InFile<AstId<ast::ValueDeclaration>>, TypeId),
}
