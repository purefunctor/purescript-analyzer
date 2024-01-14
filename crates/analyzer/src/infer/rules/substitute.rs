//! Implements the substitution routine for unification variables.
use rustc_hash::FxHashMap;

use crate::{
    infer::{Type, TypeId, Unification},
    InferDatabase,
};

use super::{BindingGroupTypes, ValueGroupTypes};

/// The core recursive logic that actually performs substitution.
fn substitute_core(
    db: &dyn InferDatabase,
    substitutions: &FxHashMap<Unification, TypeId>,
    type_id: TypeId,
) -> TypeId {
    match db.lookup_intern_type(type_id) {
        Type::Application(f, x) => {
            let f = substitute_core(db, substitutions, f);
            let x = substitute_core(db, substitutions, x);
            db.intern_type(Type::Application(f, x))
        }
        Type::Constructor(_) => type_id,
        Type::Forall(v, b) => {
            let b = substitute_core(db, substitutions, b);
            db.intern_type(Type::Forall(v, b))
        }
        Type::Function(a, r) => {
            let a = substitute_core(db, substitutions, a);
            let r = substitute_core(db, substitutions, r);
            db.intern_type(Type::Function(a, r))
        }
        Type::Primitive(_) => type_id,
        Type::Unification(u) => *substitutions.get(&u).unwrap_or(&type_id),
        Type::Variable(_) => type_id,
        Type::NotImplemented => type_id,
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

impl ApplySubstitution for FxHashMap<Unification, TypeId> {
    fn apply_substitution(
        &mut self,
        db: &dyn InferDatabase,
        substitutions: &FxHashMap<Unification, TypeId>,
    ) {
        self.values_mut()
            .for_each(|type_id| *type_id = substitute_core(db, substitutions, *type_id));
    }
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
        self.of_value_group = substitute_core(db, substitutions, self.of_value_group);
        self.of_expr
            .values_mut()
            .for_each(|t_id| *t_id = substitute_core(db, substitutions, *t_id));
        self.of_let_name
            .values_mut()
            .for_each(|t_id| *t_id = substitute_core(db, substitutions, *t_id));
        self.of_binder
            .values_mut()
            .for_each(|t_id| *t_id = substitute_core(db, substitutions, *t_id));
    }
}
