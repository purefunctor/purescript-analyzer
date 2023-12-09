//! Implements the substitution routine for unification variables.
use rustc_hash::FxHashMap;

use crate::{
    infer::{Type, TypeId, Unification},
    InferDatabase,
};

use super::{BindingGroupTypes, ValueGroupTypes};

/// The core recursive logic that actually performs substitution.
fn substitute(
    db: &dyn InferDatabase,
    substitutions: &FxHashMap<Unification, TypeId>,
    t_id: TypeId,
) -> TypeId {
    match db.lookup_intern_type(t_id) {
        Type::Application(f_id, x_id) => {
            let f_id = substitute(db, substitutions, f_id);
            let x_id = substitute(db, substitutions, x_id);
            db.intern_type(Type::Application(f_id, x_id))
        }
        Type::Function(a_id, r_id) => {
            let a_id = substitute(db, substitutions, a_id);
            let r_id = substitute(db, substitutions, r_id);
            db.intern_type(Type::Function(a_id, r_id))
        }
        Type::Primitive(_) => t_id,
        Type::Unification(u) => *substitutions.get(&u).unwrap_or(&t_id),
        Type::NotImplemented => t_id,
    }
}

/// A trait for applying substitutions on containers of types.
pub(super) trait ApplySubstitution {
    fn apply_substitution(
        &mut self,
        db: &dyn InferDatabase,
        substitutions: &FxHashMap<Unification, TypeId>,
    );
}

impl ApplySubstitution for BindingGroupTypes {
    fn apply_substitution(
        &mut self,
        db: &dyn InferDatabase,
        substitutions: &FxHashMap<Unification, TypeId>,
    ) {
        self.of_value_group
            .values_mut()
            .for_each(|value_group_types| value_group_types.apply_substitution(db, substitutions));
    }
}

impl ApplySubstitution for ValueGroupTypes {
    fn apply_substitution(
        &mut self,
        db: &dyn InferDatabase,
        substitutions: &FxHashMap<Unification, TypeId>,
    ) {
        self.of_value_group = substitute(db, substitutions, self.of_value_group);
        self.of_expr.values_mut().for_each(|t_id| *t_id = substitute(db, substitutions, *t_id));
        self.of_let_name.values_mut().for_each(|t_id| *t_id = substitute(db, substitutions, *t_id));
        self.of_binder.values_mut().for_each(|t_id| *t_id = substitute(db, substitutions, *t_id));
    }
}
