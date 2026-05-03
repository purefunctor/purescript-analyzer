use parking_lot::RwLock;
use smol_str::SmolStr;

use crate::core::{ForallBinder, ForallBinderId, RowType, RowTypeId, Type, TypeId};

#[derive(Default)]
pub struct CoreInterners {
    types: RwLock<interner::Interner<Type>>,
    forall_binders: RwLock<interner::Interner<ForallBinder>>,
    row_types: RwLock<interner::Interner<RowType>>,
    smol_strs: RwLock<interner::Interner<SmolStr>>,
}

impl CoreInterners {
    pub fn intern_type(&self, t: Type) -> TypeId {
        self.types.write().intern(t)
    }

    pub fn lookup_type(&self, id: TypeId) -> Type {
        self.types.read()[id].clone()
    }

    pub fn intern_forall_binder(&self, b: ForallBinder) -> ForallBinderId {
        self.forall_binders.write().intern(b)
    }

    pub fn lookup_forall_binder(&self, id: ForallBinderId) -> ForallBinder {
        self.forall_binders.read()[id]
    }

    pub fn intern_row_type(&self, r: RowType) -> RowTypeId {
        self.row_types.write().intern(r)
    }

    pub fn lookup_row_type(&self, id: RowTypeId) -> RowType {
        self.row_types.read()[id].clone()
    }

    pub fn intern_smol_str(&self, s: SmolStr) -> crate::core::SmolStrId {
        self.smol_strs.write().intern(s)
    }

    pub fn lookup_smol_str(&self, id: crate::core::SmolStrId) -> SmolStr {
        self.smol_strs.read()[id].clone()
    }
}
