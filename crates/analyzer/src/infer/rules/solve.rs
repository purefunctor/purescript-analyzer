//! Implements the constraint solving algorithm.

use rustc_hash::FxHashMap;

use crate::{
    infer::{constraint::Constraint, TypeId, Unification},
    InferDatabase,
};

use super::{unify::UnifyContext, InferState};

pub(super) struct SolveContext<'env, 'state> {
    db: &'env dyn InferDatabase,
    infer_state: &'state mut InferState,

    unification_solved: FxHashMap<Unification, TypeId>,
    unification_deferred: Vec<(Unification, Unification)>,
}

impl<'env, 'state> SolveContext<'env, 'state> {
    pub(super) fn new(
        db: &'env dyn InferDatabase,
        infer_state: &'state mut InferState,
    ) -> SolveContext<'env, 'state> {
        let unification_solved = FxHashMap::default();
        let unification_deferred = Vec::default();
        SolveContext { db, infer_state, unification_solved, unification_deferred }
    }
}

impl<'env, 'state> SolveContext<'env, 'state> {
    pub(super) fn step(&mut self) {
        while let Some(constraint) = self.infer_state.constraints.pop() {
            match constraint {
                Constraint::UnifyDeep(x_u, y_u) => {
                    let x_s = self.unification_solved.get(&x_u).copied();
                    let y_s = self.unification_solved.get(&y_u).copied();
                    match (x_s, y_s) {
                        (Some(x_t), Some(y_t)) => {
                            UnifyContext::new(self.db, self.infer_state).unify(x_t, y_t)
                        }
                        (Some(x_t), None) => {
                            self.unification_solved.insert(y_u, x_t);
                        }
                        (None, Some(y_t)) => {
                            self.unification_solved.insert(x_u, y_t);
                        }
                        (None, None) => {
                            self.unification_deferred.push((x_u, y_u));
                        }
                    }
                }
                Constraint::UnifySolve(x_u, y_t) => {
                    self.unification_solved.insert(x_u, y_t);
                }
            }
        }

        self.unification_deferred.retain(|(x_u, y_u)| {
            let x_s = self.unification_solved.get(x_u).copied();
            let y_s = self.unification_solved.get(y_u).copied();

            if x_s.is_none() && y_s.is_none() {
                return true;
            }

            self.infer_state.constraints.push(Constraint::UnifyDeep(*x_u, *y_u));
            false
        });
    }

    pub(super) fn solve(&mut self) {
        while !self.infer_state.constraints.is_empty() {
            self.step();
        }
    }
}