//! Level-tracking unification.

use crate::ExternalQueries;
use crate::algorithm::unification::UnificationState;
use crate::algorithm::{CheckContext, CheckState};
use crate::core::{Type, TypeId, Variable, debruijn};

/// Functions for creating unification variables.
impl CheckState {
    /// Creates a fresh unification variable with the provided domain and kind.
    pub fn fresh_unification_kinded_at(&mut self, domain: debruijn::Size, kind: TypeId) -> TypeId {
        let unification_id = self.unification.fresh(domain, kind);
        self.storage.intern(Type::Unification(unification_id))
    }

    /// Creates a fresh unification variable with the provided kind.
    pub fn fresh_unification_kinded(&mut self, kind: TypeId) -> TypeId {
        let domain = self.bound.size();
        self.fresh_unification_kinded_at(domain, kind)
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

impl CheckState {
    /// Normalizes unification and bound type variables.
    ///
    /// This function also applies path compression to unification variables,
    /// where if a unification variable `?0` solves to `?1`, which solves to
    /// `Type`, `?0` is solved to `Type` to prune the lookup.
    pub fn normalize_type(&mut self, mut id: TypeId) -> TypeId {
        let mut to_compress = vec![];

        let id = loop {
            match self.storage[id] {
                Type::Unification(unification_id) => {
                    if let UnificationState::Solved(solution) =
                        self.unification.get(unification_id).state
                    {
                        id = solution;
                        to_compress.push(unification_id);
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
        };

        for unification_id in to_compress {
            self.unification.solve(unification_id, id);
        }

        id
    }
}
