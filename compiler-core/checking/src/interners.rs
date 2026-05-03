use smol_str::SmolStr;

use crate::core::{ForallBinder, ForallBinderId, RowType, RowTypeId, Type, TypeId};

#[derive(Default)]
pub struct CoreInterners {
    types: interner::parallel::Interner<Type>,
    forall_binders: interner::parallel::Interner<ForallBinder>,
    row_types: interner::parallel::Interner<RowType>,
    smol_strs: interner::parallel::Interner<SmolStr>,
}

impl CoreInterners {
    pub fn intern_type(&self, t: Type) -> TypeId {
        self.types.intern(t)
    }

    pub fn lookup_type(&self, id: TypeId) -> Type {
        self.types[id].clone()
    }

    pub fn intern_forall_binder(&self, b: ForallBinder) -> ForallBinderId {
        self.forall_binders.intern(b)
    }

    pub fn lookup_forall_binder(&self, id: ForallBinderId) -> ForallBinder {
        self.forall_binders[id]
    }

    pub fn intern_row_type(&self, r: RowType) -> RowTypeId {
        self.row_types.intern(r)
    }

    pub fn lookup_row_type(&self, id: RowTypeId) -> RowType {
        self.row_types[id].clone()
    }

    pub fn intern_smol_str(&self, s: SmolStr) -> crate::core::SmolStrId {
        self.smol_strs.intern(s)
    }

    pub fn lookup_smol_str(&self, id: crate::core::SmolStrId) -> SmolStr {
        self.smol_strs[id].clone()
    }
}
