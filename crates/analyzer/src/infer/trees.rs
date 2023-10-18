//! ASTs for type inference.
//!
//! Unlike the definition from [`surface`], [`Type`] makes use of [`salsa`]'s
//! interning mechanism rather than an [`Arena`] that gets passed around. The
//! main reason for this difference is the fact that type inference operates
//! at a global context. Using [`Arena`]s would be impractical, since it would
//! mean that types cannot be shared across two things that need inference.
//!
//! [`Arena`]: la_arena::Arena
//! [`surface`]: crate::surface
use syntax::ast;

use crate::id::{AstId, InFile};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(salsa::InternId);

impl salsa::InternKey for TypeId {
    fn from_intern_id(v: salsa::InternId) -> TypeId {
        TypeId(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Function(Box<[TypeId]>, TypeId),
    Literal(Literal),
    NotImplemented,
    Unification(Unification),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int,
    Number,
    String,
    Char,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Unification {
    Local(u32, InFile<AstId<ast::ValueDeclaration>>),
    Global(InFile<AstId<ast::ValueDeclaration>>),
}
