use super::{Type, TypeId};

/// Storage backend for [`Type`].
pub trait TypeStorage {
    /// Interns a [`Type`] into a [`TypeId`].
    fn intern(&mut self, t: Type) -> TypeId;

    /// Returns the [`Type`] of a [`TypeId`].
    fn index(&self, id: TypeId) -> &Type;
}
