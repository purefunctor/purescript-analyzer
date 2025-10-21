use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::IndexedModule;
use lowering::{GraphNodeId, ImplicitBindingId, LoweredModule, TypeVariableBindingId};

use crate::{
    ExternalQueries,
    check::unification::UnificationContext,
    core::{Type, TypeId, TypeInterner, debruijn},
};

#[derive(Default)]
pub struct CheckState {
    pub storage: TypeInterner,

    pub bound: debruijn::Bound,
    pub kinds: debruijn::BoundMap<TypeId>,
    pub types: debruijn::BoundMap<TypeId>,

    pub unification: UnificationContext,
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
}

pub struct CheckContext<'a, Q>
where
    Q: ExternalQueries,
{
    pub queries: &'a Q,
    pub prim: PrimCore,
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
        Ok(CheckContext { queries, prim, indexed, lowered, prim_indexed })
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
        let prim_id = queries.prim_id();
        let prim_resolved = queries.resolved(prim_id)?;

        let mut lookup_prim_type = |name| {
            let prim_type = prim_resolved.lookup_type(&prim_resolved, None, name);
            let (_, type_id) = prim_type.expect("invariant violated: missing Type");
            state.storage.intern(Type::Constructor(prim_id, type_id))
        };

        Ok(PrimCore {
            t: lookup_prim_type("Type"),
            function: lookup_prim_type("Function"),
            array: lookup_prim_type("Array"),
            record: lookup_prim_type("Record"),
            number: lookup_prim_type("Number"),
            int: lookup_prim_type("Int"),
            string: lookup_prim_type("String"),
            char: lookup_prim_type("Char"),
            boolean: lookup_prim_type("Boolean"),
            partial: lookup_prim_type("Partial"),
            constraint: lookup_prim_type("Constraint"),
            symbol: lookup_prim_type("Symbol"),
            row: lookup_prim_type("Row"),
            unknown: state.storage.intern(Type::Unknown),
        })
    }
}
