//! Implements the unification and subsumption algorithm.

use crate::{
    infer::{constraint::Constraint, PrettyPrinter, Type, TypeId},
    InferDatabase,
};

use super::InferState;

pub(super) struct UnifyContext<'env, 'state> {
    db: &'env dyn InferDatabase,
    infer_state: &'state mut InferState,
}

impl<'env, 'state> UnifyContext<'env, 'state> {
    pub(super) fn new(
        db: &'env dyn InferDatabase,
        infer_state: &'state mut InferState,
    ) -> UnifyContext<'env, 'state> {
        UnifyContext { db, infer_state }
    }
}

impl<'env, 'state> UnifyContext<'env, 'state> {
    pub(super) fn unify(&mut self, x_id: TypeId, y_id: TypeId) {
        let pp = PrettyPrinter::new(self.db);
        eprintln!("unify({}, {})", pp.ty(x_id).pretty(80), pp.ty(y_id).pretty(80));

        let x_ty = self.db.lookup_intern_type(x_id);
        let y_ty = self.db.lookup_intern_type(y_id);

        match (x_ty, y_ty) {
            (Type::Function(x_function, x_result), Type::Function(y_function, y_result)) => {
                self.unify(x_function, y_function);
                self.unify(x_result, y_result);
            }
            (Type::Unification(x_u), Type::Unification(y_u)) => {
                if x_u != y_u {
                    self.infer_state.constraints.push(Constraint::UnifyDeep(x_u, y_u))
                }
            }
            (Type::Unification(x_u), _) => {
                self.infer_state.constraints.push(Constraint::UnifySolve(x_u, y_id));
            }
            (_, Type::Unification(y_u)) => {
                self.infer_state.constraints.push(Constraint::UnifySolve(y_u, x_id));
            }
            (_, _) => {
                unimplemented!("Oh No!");
            }
        }
    }
}
