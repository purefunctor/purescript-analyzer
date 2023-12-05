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

use crate::{id::InFile, resolver::ValueGroupId, surface};

pub use printer::PrettyPrinter;
use rustc_hash::FxHashMap;

use super::constraint::Constraint;

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

#[derive(Debug, PartialEq, Eq)]
pub struct InferBindingGroup {
    // FIXME: this is temporary for the current implementation...
    pub of_value_group: FxHashMap<ValueGroupId, InferValueGroup>,
    constraints: Vec<Constraint>,
}

impl InferBindingGroup {
    pub fn new(
        of_value_group: FxHashMap<ValueGroupId, InferValueGroup>,
        constraints: Vec<Constraint>,
    ) -> InferBindingGroup {
        InferBindingGroup { of_value_group, constraints }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct InferValueGroup {
    // FIXME: again, this is temporary
    pub ty: TypeId,
    of_expr: FxHashMap<surface::ExprId, TypeId>,
    of_let_name: FxHashMap<surface::LetNameId, TypeId>,
    of_binder: FxHashMap<surface::BinderId, TypeId>,
}

impl InferValueGroup {
    pub fn new(
        ty: TypeId,
        of_expr: FxHashMap<surface::ExprId, TypeId>,
        of_let_name: FxHashMap<surface::LetNameId, TypeId>,
        of_binder: FxHashMap<surface::BinderId, TypeId>,
    ) -> InferValueGroup {
        InferValueGroup { ty, of_expr, of_let_name, of_binder }
    }
}
