use std::{mem, sync::Arc};

use crate::{
    id::InFile,
    infer::{Constraint, CoreType, Hint, InferError, InferErrorKind},
    InferenceDatabase,
};

use super::{CoreTypeId, SolveContext};

fn occurs_check(db: &dyn InferenceDatabase, u: InFile<u32>, t: CoreTypeId) -> bool {
    match db.lookup_intern_type(t) {
        CoreType::Application(function, argument) => {
            occurs_check(db, u, function) || occurs_check(db, u, argument)
        }
        CoreType::Constructor(_) => false,
        CoreType::Forall(_, inner) => occurs_check(db, u, inner),
        CoreType::Function(argument, result) => {
            occurs_check(db, u, argument) || occurs_check(db, u, result)
        }
        CoreType::Primitive(_) => false,
        CoreType::Unification(v) => u == v,
        CoreType::Variable(_) => false,
        CoreType::NotImplemented => false,
    }
}

impl<'i, 'a> SolveContext<'i, 'a> {
    fn solve_unification(
        &mut self,
        db: &dyn InferenceDatabase,
        hints: Arc<[Hint]>,
        x_u: InFile<u32>,
        y_t: CoreTypeId,
    ) {
        if occurs_check(db, x_u, y_t) {
            self.infer.add_error(InferError { hints, kind: InferErrorKind::OccursCheck(x_u, y_t) });
            return;
        }
        if let Some(x_t) = self.state.unification_solved.get(&x_u) {
            self.infer.unify_types_with_hints(db, hints, *x_t, y_t);
        } else {
            self.state.unification_solved.insert(x_u, y_t);
        }
    }

    fn step(&mut self, db: &dyn InferenceDatabase, constraint: Constraint) {
        match constraint {
            Constraint::UnifyDeep(hints, x_u, y_u) => {
                let x_s = self.state.unification_solved.get(&x_u).copied();
                let y_s = self.state.unification_solved.get(&y_u).copied();
                match (x_s, y_s) {
                    (Some(x_t), Some(y_t)) => {
                        self.infer.unify_types_with_hints(db, hints, x_t, y_t);
                    }
                    (Some(x_t), None) => {
                        self.solve_unification(db, hints, y_u, x_t);
                    }
                    (None, Some(y_t)) => {
                        self.solve_unification(db, hints, x_u, y_t);
                    }
                    (None, None) => {
                        self.state.unification_deferred.push((hints, x_u, y_u));
                    }
                }
            }
            Constraint::UnifySolve(hints, x_u, y_t) => {
                self.solve_unification(db, hints, x_u, y_t);
            }
        }
    }

    fn pass(&mut self, db: &dyn InferenceDatabase) {
        while let Some(constraint) = self.infer.state.constraints.pop() {
            self.step(db, constraint);
        }

        let unification_deferred = mem::take(&mut self.state.unification_deferred);
        for (hints, x_u, y_u) in unification_deferred {
            let x_s = self.state.unification_solved.get(&x_u).copied();
            let y_s = self.state.unification_solved.get(&y_u).copied();
            if x_s.is_none() && y_s.is_none() {
                self.state.unification_deferred.push((hints, x_u, y_u));
            } else {
                self.infer.state.constraints.push(Constraint::UnifyDeep(hints, x_u, y_u));
            }
        }
    }

    pub(super) fn solve(&mut self, db: &dyn InferenceDatabase) {
        eprintln!("Solving:");
        let mut i = 0;
        while i < 1000 && !self.infer.state.constraints.is_empty() {
            self.pass(db);
            i += 1;
        }
    }
}
