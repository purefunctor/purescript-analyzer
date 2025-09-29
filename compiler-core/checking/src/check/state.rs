use building_types::QueryResult;
use indexing::IndexedModule;
use lowering::{GraphNodeId, ImplicitBindingId, LoweredModule, TypeVariableBindingId};

use crate::{
    External,
    core::{Type, TypeId, debruijn, storage::TypeStorage},
};

pub struct CheckState<'s, S>
where
    S: TypeStorage,
{
    pub storage: &'s mut S,
    unique: u32,
    bound: debruijn::Bound,
    kinds: debruijn::BoundMap<TypeId>,
}

impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    pub fn new(storage: &'s mut S) -> CheckState<'s, S> {
        let unique = 0;
        let bound = debruijn::Bound::default();
        let kinds = debruijn::BoundMap::default();
        CheckState { storage, unique, bound, kinds }
    }
}

impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    pub fn fresh_unification(&mut self) -> TypeId {
        let unique = self.unique;
        self.unique += 1;
        self.storage.intern(Type::Unification(unique))
    }

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

    pub fn bind_implicit(&mut self, node: GraphNodeId, id: ImplicitBindingId) -> debruijn::Level {
        let variable = debruijn::Variable::Implicit { node, id };
        if let Some(level) = self.bound.level_of(variable) {
            level
        } else {
            self.bound.bind(variable)
        }
    }

    pub fn lookup_implicit(
        &self,
        node: GraphNodeId,
        id: ImplicitBindingId,
    ) -> Option<debruijn::Index> {
        let variable = debruijn::Variable::Implicit { node, id };
        self.bound.index_of(variable)
    }

    pub fn unbind(&mut self, level: debruijn::Level) {
        self.bound.unbind(level);
        self.kinds.unbind(level);
    }
}

pub struct CheckContext<'e> {
    pub prim: PrimCore,
    pub indexed: &'e IndexedModule,
    pub lowered: &'e LoweredModule,
    pub prim_indexed: &'e IndexedModule,
}

impl<'e> CheckContext<'e> {
    pub fn new(
        prim: PrimCore,
        indexed: &'e IndexedModule,
        lowered: &'e LoweredModule,
        prim_indexed: &'e IndexedModule,
    ) -> CheckContext<'e> {
        CheckContext { prim, indexed, lowered, prim_indexed }
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
    pub fn collect<S>(external: &impl External, state: &mut CheckState<S>) -> QueryResult<PrimCore>
    where
        S: TypeStorage,
    {
        let prim_id = external.prim_id();
        let prim_resolved = external.resolved(prim_id)?;

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
