//! Implements instantiation for polymorphic types.
use rustc_hash::FxHashMap;

use crate::{
    infer::{Provenance, Type, TypeId},
    InferDatabase,
};

use super::{replace::replace_type, InferState};

struct InstantiateContext<'env, 'state> {
    db: &'env dyn InferDatabase,
    infer_state: &'state mut InferState,
    provenance: Provenance,
}

impl<'env, 'state> InstantiateContext<'env, 'state> {
    fn fresh_unification(&mut self) -> TypeId {
        self.infer_state.fresh_unification(self.db, self.provenance)
    }

    fn instantiate(&mut self, type_id: TypeId) -> TypeId {
        if let Type::Forall(initial_variable, initial_body) = self.db.lookup_intern_type(type_id) {
            let mut replacements = FxHashMap::default();

            let initial_unification = self.fresh_unification();
            replacements.insert(initial_variable, initial_unification);
            let mut current_body = initial_body;

            while let Type::Forall(variable, body) = self.db.lookup_intern_type(current_body) {
                let unification = self.fresh_unification();
                replacements.insert(variable, unification);
                current_body = body;
            }

            replace_type(self.db, replacements, current_body)
        } else {
            type_id
        }
    }
}

pub(super) fn instantiate_type(
    db: &dyn InferDatabase,
    infer_state: &mut InferState,
    provenance: Provenance,
    type_id: TypeId,
) -> TypeId {
    InstantiateContext { db, infer_state, provenance }.instantiate(type_id)
}
