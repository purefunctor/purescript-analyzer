//! Implements the type variable substitution routine.
use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    infer::{Type, TypeId},
    InferDatabase,
};

struct ReplaceContext<'env> {
    db: &'env dyn InferDatabase,
    replacements: FxHashMap<Arc<str>, TypeId>,
    in_scope: FxHashSet<Arc<str>>,
}

impl<'env> ReplaceContext<'env> {
    fn replace_core(&mut self, type_id: TypeId) -> TypeId {
        match self.db.lookup_intern_type(type_id) {
            Type::Application(f, x) => {
                let f = self.replace_core(f);
                let x = self.replace_core(x);
                self.db.intern_type(Type::Application(f, x))
            }
            Type::Constructor(_) => type_id,
            Type::Forall(v, b) => {
                self.in_scope.insert(Arc::clone(&v));
                let b = self.replace_core(b);
                self.in_scope.remove(&v);
                self.db.intern_type(Type::Forall(v, b))
            }
            Type::Function(a, r) => {
                let a = self.replace_core(a);
                let r = self.replace_core(r);
                self.db.intern_type(Type::Function(a, r))
            }
            Type::Primitive(_) => type_id,
            Type::Unification(_) => type_id,
            Type::Variable(v) => {
                if self.in_scope.contains(&v) {
                    type_id
                } else {
                    self.replacements
                        .get(&v)
                        .copied()
                        .unwrap_or_else(|| self.db.intern_type(Type::NotImplemented))
                }
            }
            Type::NotImplemented => type_id,
        }
    }
}

pub(super) fn replace_type(
    db: &dyn InferDatabase,
    replacements: FxHashMap<Arc<str>, TypeId>,
    type_id: TypeId,
) -> TypeId {
    let in_scope = FxHashSet::default();
    ReplaceContext { db, replacements, in_scope }.replace_core(type_id)
}
