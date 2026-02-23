use std::ops;

use smol_str::SmolStr;

use crate::core::{
    ForallBinder, ForallBinderId, RowType, RowTypeId, Synonym, SynonymId, Type, TypeId,
};

#[derive(Default)]
pub struct CoreInterners {
    types: interner::Interner<Type>,
    forall_binders: interner::Interner<ForallBinder>,
    row_types: interner::Interner<RowType>,
    synonyms: interner::Interner<Synonym>,
    smol_strs: interner::Interner<SmolStr>,
}

impl CoreInterners {
    pub fn intern_type(&mut self, t: Type) -> TypeId {
        self.types.intern(t)
    }

    pub fn lookup_type(&self, id: TypeId) -> Type {
        self.types[id].clone()
    }

    pub fn intern_forall_binder(&mut self, b: ForallBinder) -> ForallBinderId {
        self.forall_binders.intern(b)
    }

    pub fn lookup_forall_binder(&self, id: ForallBinderId) -> ForallBinder {
        self.forall_binders[id].clone()
    }

    pub fn intern_row_type(&mut self, r: RowType) -> RowTypeId {
        self.row_types.intern(r)
    }

    pub fn lookup_row_type(&self, id: RowTypeId) -> RowType {
        self.row_types[id].clone()
    }

    pub fn intern_synonym(&mut self, s: Synonym) -> SynonymId {
        self.synonyms.intern(s)
    }

    pub fn lookup_synonym(&self, id: SynonymId) -> Synonym {
        self.synonyms[id].clone()
    }

    pub fn intern_smol_str(&mut self, s: SmolStr) -> crate::core::SmolStrId {
        self.smol_strs.intern(s)
    }

    pub fn lookup_smol_str(&self, id: crate::core::SmolStrId) -> SmolStr {
        self.smol_strs[id].clone()
    }
}

impl ops::Index<TypeId> for CoreInterners {
    type Output = Type;

    fn index(&self, id: TypeId) -> &Type {
        &self.types[id]
    }
}

impl ops::Index<ForallBinderId> for CoreInterners {
    type Output = ForallBinder;

    fn index(&self, id: ForallBinderId) -> &ForallBinder {
        &self.forall_binders[id]
    }
}

impl ops::Index<RowTypeId> for CoreInterners {
    type Output = RowType;

    fn index(&self, id: RowTypeId) -> &RowType {
        &self.row_types[id]
    }
}

impl ops::Index<SynonymId> for CoreInterners {
    type Output = Synonym;

    fn index(&self, id: SynonymId) -> &Synonym {
        &self.synonyms[id]
    }
}
