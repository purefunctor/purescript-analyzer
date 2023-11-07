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
mod printer;

use crate::{id::InFile, resolver::ValueGroupId};

pub use printer::PrettyPrinter;

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
    Application(TypeId, TypeId),
    Function(TypeId, TypeId),
    Primitive(Primitive),
    Reference(InFile<ValueGroupId>),
    Unification(Unification),
    NotImplemented,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Int,
    Number,
    Char,
    String,
    Boolean,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unification {
    pub index: u32,
    pub provenance: Provenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Provenance {
    ValueGroup(InFile<ValueGroupId>),
}
