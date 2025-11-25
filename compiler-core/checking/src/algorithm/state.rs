//! Implements the algorithm's state.

pub mod unification;
pub use unification::*;

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use itertools::Itertools;
use lowering::{GraphNodeId, ImplicitBindingId, LoweredModule, TypeVariableBindingId};
use rustc_hash::FxHashMap;

use crate::algorithm::{quantify, transfer};
use crate::core::{Type, TypeId, TypeInterner, Variable, debruijn};
use crate::{CheckedModule, ExternalQueries};

#[derive(Default)]
pub struct CheckState {
    pub storage: TypeInterner,
    pub checked: CheckedModule,

    pub bound: debruijn::Bound,
    pub kinds: debruijn::BoundMap<TypeId>,
    pub types: debruijn::BoundMap<TypeId>,

    pub unification: UnificationContext,
    pub binding_group: BindingGroupContext,
}

#[derive(Default)]
pub struct BindingGroupContext {
    pub terms: FxHashMap<TermItemId, TypeId>,
    pub types: FxHashMap<TypeItemId, TypeId>,
}

pub struct CheckContext<'a, Q>
where
    Q: ExternalQueries,
{
    pub queries: &'a Q,
    pub prim: PrimCore,

    pub id: FileId,
    pub indexed: Arc<IndexedModule>,
    pub lowered: Arc<LoweredModule>,

    pub prim_indexed: Arc<IndexedModule>,
}

impl<'a, Q> CheckContext<'a, Q>
where
    Q: ExternalQueries,
{
    pub fn new(
        queries: &'a Q,
        state: &mut CheckState,
        id: FileId,
    ) -> QueryResult<CheckContext<'a, Q>> {
        let indexed = queries.indexed(id)?;
        let lowered = queries.lowered(id)?;
        let prim = PrimCore::collect(queries, state)?;
        let prim_id = queries.prim_id();
        let prim_indexed = queries.indexed(prim_id)?;
        Ok(CheckContext { queries, prim, id, indexed, lowered, prim_indexed })
    }
}

pub struct PrimCore {
    pub t: TypeId,
    pub function: TypeId,
    pub array: TypeId,
    pub record: TypeId,
    pub number: TypeId,
    pub int: TypeId,
    pub string: TypeId,
    pub char: TypeId,
    pub boolean: TypeId,
    pub partial: TypeId,
    pub constraint: TypeId,
    pub symbol: TypeId,
    pub row: TypeId,
    pub unknown: TypeId,
}

impl PrimCore {
    fn collect(queries: &impl ExternalQueries, state: &mut CheckState) -> QueryResult<PrimCore> {
        let file_id = queries.prim_id();
        let resolved = queries.resolved(file_id)?;

        let mut lookup_type = |name: &str| {
            let prim_type = resolved.lookup_type(&resolved, None, name);

            let (file_id, type_id) =
                prim_type.unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim"));

            state.storage.intern(Type::Constructor(file_id, type_id))
        };

        Ok(PrimCore {
            t: lookup_type("Type"),
            function: lookup_type("Function"),
            array: lookup_type("Array"),
            record: lookup_type("Record"),
            number: lookup_type("Number"),
            int: lookup_type("Int"),
            string: lookup_type("String"),
            char: lookup_type("Char"),
            boolean: lookup_type("Boolean"),
            partial: lookup_type("Partial"),
            constraint: lookup_type("Constraint"),
            symbol: lookup_type("Symbol"),
            row: lookup_type("Row"),
            unknown: state.storage.intern(Type::Unknown),
        })
    }
}

impl CheckState {
    pub fn bind_forall(&mut self, id: TypeVariableBindingId, kind: TypeId) -> debruijn::Level {
        let variable = debruijn::Variable::Forall(id);
        let level = self.bound.bind(variable);
        self.kinds.insert(level, kind);
        level
    }

    pub fn lookup_forall(&self, id: TypeVariableBindingId) -> Option<debruijn::Index> {
        let variable = debruijn::Variable::Forall(id);
        self.bound.index_of(variable)
    }

    pub fn forall_binding_kind(&self, id: TypeVariableBindingId) -> Option<TypeId> {
        let variable = debruijn::Variable::Forall(id);
        let level = self.bound.level_of(variable)?;
        self.kinds.get(level).copied()
    }

    pub fn bind_implicit(
        &mut self,
        node: GraphNodeId,
        id: ImplicitBindingId,
        kind: TypeId,
    ) -> debruijn::Level {
        let variable = debruijn::Variable::Implicit { node, id };
        let level = self.bound.level_of(variable).unwrap_or_else(|| self.bound.bind(variable));
        self.kinds.insert(level, kind);
        level
    }

    pub fn lookup_implicit(
        &self,
        node: GraphNodeId,
        id: ImplicitBindingId,
    ) -> Option<debruijn::Index> {
        let variable = debruijn::Variable::Implicit { node, id };
        self.bound.index_of(variable)
    }

    pub fn implicit_binding_kind(
        &self,
        node: GraphNodeId,
        id: ImplicitBindingId,
    ) -> Option<TypeId> {
        let variable = debruijn::Variable::Implicit { node, id };
        let level = self.bound.level_of(variable)?;
        self.kinds.get(level).copied()
    }

    pub fn bind_with_type(&mut self, level: debruijn::Level, t: TypeId, k: TypeId) {
        debug_assert!(!self.kinds.contains(level), "invariant violated: {level} already bound");
        self.bound.bind(debruijn::Variable::Core);
        self.types.insert(level, t);
        self.kinds.insert(level, k);
    }

    pub fn core_kind(&self, index: debruijn::Index) -> Option<TypeId> {
        let size = self.bound.size();
        let level = index.to_level(size)?;
        self.kinds.get(level).copied()
    }

    pub fn unbind(&mut self, level: debruijn::Level) {
        self.bound.unbind(level);
        self.types.unbind(level);
        self.kinds.unbind(level);
    }

    pub fn type_binding_group<Q>(
        &mut self,
        context: &CheckContext<Q>,
        group: impl AsRef<[TypeItemId]>,
    ) where
        Q: ExternalQueries,
    {
        for &item in group.as_ref() {
            let t = self.fresh_unification_type(context);
            self.binding_group.types.insert(item, t);
        }
    }

    pub fn commit_binding_group<Q>(&mut self, context: &CheckContext<Q>)
    where
        Q: ExternalQueries,
    {
        for (item_id, type_id) in self.binding_group.terms.drain().collect_vec() {
            if let Some(type_id) = quantify::quantify(self, type_id) {
                let type_id = transfer::globalize(self, context, type_id);
                self.checked.terms.insert(item_id, type_id);
            }
        }
        for (item_id, type_id) in self.binding_group.types.drain().collect_vec() {
            if let Some(type_id) = quantify::quantify(self, type_id) {
                let type_id = transfer::globalize(self, context, type_id);
                self.checked.types.insert(item_id, type_id);
            }
        }
    }
}

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
