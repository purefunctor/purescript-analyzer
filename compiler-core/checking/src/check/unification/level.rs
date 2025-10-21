//! Level-tracking unification.

use crate::{
    ExternalQueries,
    check::{CheckContext, CheckState, unification::UnificationState},
    core::{Type, TypeId, Variable, debruijn},
};

/// Functions for creating unification variables.
impl CheckState {
    /// Creates a fresh unification variable with the provided kind.
    pub fn fresh_unification_kinded(&mut self, kind: TypeId) -> TypeId {
        let domain = self.bound.size();
        let unification_id = self.unification.fresh(domain, kind);
        self.storage.intern(Type::Unification(unification_id))
    }

    /// Creates a fresh polykinded unification variable.
    pub fn fresh_unification<Q>(&mut self, context: &CheckContext<Q>) -> TypeId
    where
        Q: ExternalQueries,
    {
        let kind_ty = self.fresh_unification_type(context);
        self.fresh_unification_kinded(kind_ty)
    }

    /// Creates a fresh [`Type`]-kinded unification variable.
    pub fn fresh_unification_type<Q>(&mut self, context: &CheckContext<Q>) -> TypeId
    where
        Q: ExternalQueries,
    {
        self.fresh_unification_kinded(context.prim.t)
    }
}

/// Functions for solving unification variables.
impl CheckState {
    /// Solves a unification variable to a [`TypeId`].
    pub fn solve(
        &mut self,
        codomain: debruijn::Size,
        unification_id: u32,
        solution: TypeId,
    ) -> Option<u32> {
        let occurs = Some(unification_id);

        if !self.promote_type(occurs, codomain, unification_id, solution) {
            return None;
        }

        self.unification.solve(unification_id, solution);

        Some(unification_id)
    }

    pub fn promote_type(
        &mut self,
        occurs: Option<u32>,
        codomain: debruijn::Size,
        unification_id: u32,
        solution: TypeId,
    ) -> bool {
        let solution = self.normalize_type(solution);
        match self.storage[solution] {
            Type::Application(function, argument) => {
                self.promote_type(occurs, codomain, unification_id, function)
                    && self.promote_type(occurs, codomain, unification_id, argument)
            }

            Type::Constructor(_, _) => true,

            Type::Forall(ref binder, inner) => {
                let inner_codomain = codomain.increment();
                self.promote_type(occurs, codomain, unification_id, binder.kind)
                    && self.promote_type(occurs, inner_codomain, unification_id, inner)
            }

            Type::Function(argument, result) => {
                self.promote_type(occurs, codomain, unification_id, argument)
                    && self.promote_type(occurs, codomain, unification_id, result)
            }

            Type::KindApplication(function, argument) => {
                self.promote_type(occurs, codomain, unification_id, function)
                    && self.promote_type(occurs, codomain, unification_id, argument)
            }

            Type::Unification(solution_id) => {
                let unification = self.unification.get(unification_id);
                let solution = self.unification.get(solution_id);

                if occurs == Some(solution_id) {
                    return false;
                }

                if unification.domain < solution.domain {
                    let promoted_id = self.unification.fresh(unification.domain, unification.kind);
                    let promoted_ty = self.storage.intern(Type::Unification(promoted_id));

                    // promoted_ty is simple enough to not warrant `solve` recursion
                    self.unification.solve(solution_id, promoted_ty);
                }

                true
            }

            Type::Variable(ref variable) => {
                let unification = self.unification.get(unification_id);
                if let Variable::Bound(index) = variable
                    && !index.in_scope(unification.domain)
                {
                    return false;
                }

                true
            }

            Type::Unknown => true,
        }
    }
}

impl CheckState {
    pub fn normalize_type(&self, mut id: TypeId) -> TypeId {
        loop {
            match self.storage[id] {
                Type::Unification(unification_id) => {
                    if let UnificationState::Solved(solution) =
                        self.unification.get(unification_id).state
                    {
                        id = solution;
                    } else {
                        break id;
                    }
                }
                Type::Variable(Variable::Bound(index)) => {
                    let size = self.bound.size();
                    if let Some(level) = index.to_level(size)
                        && let Some(resolved) = self.types.get(level)
                    {
                        id = *resolved;
                    } else {
                        break id;
                    }
                }
                _ => break id,
            }
        }
    }
}
