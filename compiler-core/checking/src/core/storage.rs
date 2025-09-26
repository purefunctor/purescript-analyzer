use super::{Type, TypeId};

/// Storage backend for [`Type`].
pub trait TypeStorage {
    /// The [`TypeId`] of [`Type::Unknown`].
    fn unknown(&self) -> TypeId;

    /// Interns a [`Type`] into a [`TypeId`].
    fn intern(&mut self, t: Type) -> TypeId;

    /// Returns the [`Type`] of a [`TypeId`].
    fn index(&self, id: TypeId) -> &Type;
}
