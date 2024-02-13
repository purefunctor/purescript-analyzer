use crate::{
    id::InFile,
    infer::{pretty_print, Constraint, CoreType},
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
    fn solve_unification(&mut self, db: &dyn InferenceDatabase, x_u: InFile<u32>, y_t: CoreTypeId) {
        if occurs_check(db, x_u, y_t) {
            eprintln!("Infinite type error: {} ~ {}", x_u.value, pretty_print(db, y_t));
            return;
        }
        if let Some(x_t) = self.state.unification_solved.get(&x_u) {
            eprintln!("{} : {} ~ {}", x_u.value, pretty_print(db, *x_t), pretty_print(db, y_t));
            self.infer.unify_types(db, *x_t, y_t);
        } else {
            self.state.unification_solved.insert(x_u, y_t);
        }
    }

    fn step(&mut self, db: &dyn InferenceDatabase, constraint: Constraint) {
        match constraint {
            Constraint::UnifyDeep(x_u, y_u) => {
                let x_s = self.state.unification_solved.get(&x_u).copied();
                let y_s = self.state.unification_solved.get(&y_u).copied();
                match (x_s, y_s) {
                    (Some(x_t), Some(y_t)) => {
                        self.infer.unify_types(db, x_t, y_t);
                    }
                    (Some(x_t), None) => {
                        self.solve_unification(db, y_u, x_t);
                    }
                    (None, Some(y_t)) => {
                        self.solve_unification(db, x_u, y_t);
                    }
                    (None, None) => {
                        self.state.unification_deferred.push((x_u, y_u));
                    }
                }
            }
            Constraint::UnifySolve(x_u, y_t) => {
                self.solve_unification(db, x_u, y_t);
            }
        }
    }

    fn pass(&mut self, db: &dyn InferenceDatabase) {
        while let Some(constraint) = self.infer.result.constraints.pop() {
            self.step(db, constraint);
        }
        self.state.unification_deferred.retain(|(x_u, y_u)| {
            let x_s = self.state.unification_solved.get(x_u).copied();
            let y_s = self.state.unification_solved.get(y_u).copied();
            if x_s.is_none() && y_s.is_none() {
                return true;
            }
            self.infer.result.constraints.push(Constraint::UnifyDeep(*x_u, *y_u));
            false
        });
    }

    pub(super) fn solve(&mut self, db: &dyn InferenceDatabase) {
        eprintln!("Solving:");
        let mut i = 0;
        while i < 1000 && !self.infer.result.constraints.is_empty() {
            self.pass(db);
            i += 1;
        }
    }
}
