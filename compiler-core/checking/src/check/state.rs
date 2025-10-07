use std::sync::Arc;

use building_types::QueryResult;
use indexing::IndexedModule;
use lowering::{GraphNodeId, ImplicitBindingId, LoweredModule, TypeVariableBindingId};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    External,
    check::{
        substitute,
        unification::{UnificationContext, UnificationState},
    },
    core::{Pruning, Spine, Type, TypeId, Variable, debruijn, storage::TypeStorage},
};

pub struct CheckState<'s, S>
where
    S: TypeStorage,
{
    pub storage: &'s mut S,
    bound: debruijn::Bound,
    kinds: debruijn::BoundMap<TypeId>,
    unification: UnificationContext,
}

impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    pub fn new(storage: &'s mut S) -> CheckState<'s, S> {
        let bound = debruijn::Bound::default();
        let kinds = debruijn::BoundMap::default();
        let unification = UnificationContext::default();
        CheckState { storage, bound, kinds, unification }
    }
}

impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
    pub fn fresh_unification(&mut self, context: &CheckContext) -> TypeId {
        // Create a new unification variable for the kind `?k :: <bound> -> Type`
        let kind_pi = self.unification_function_kind(context, context.prim.t);
        let kind_id = self.unification.fresh(kind_pi);

        let kind_pr = self.bound.iter().map(|_| true).collect();
        let kind_ty = self.storage.intern(Type::Pruning(kind_id, kind_pr));

        // Create a new unification variable for the type: `?t :: <bound> -> ?k`
        let type_pi = self.unification_function_kind(context, kind_ty);
        let type_id = self.unification.fresh(type_pi);

        let type_pr = self.bound.iter().map(|_| true).collect();
        self.storage.intern(Type::Pruning(type_id, type_pr))
    }

    /// Create the [`Type::Function`]-based kind representation
    /// using the current environment and a given kind:
    ///
    /// ```text
    /// [ a :: Type, b :: Type ], Type
    ///
    /// (a :: Type) -> (b :: Type) -> Type
    /// ```
    fn unification_function_kind(&mut self, context: &CheckContext, kind: TypeId) -> TypeId {
        self.bound.iter().rev().fold(kind, |kind, (level, _)| {
            let variable_kind = self.kinds.get(level).copied();
            let variable_kind = variable_kind.unwrap_or(context.prim.unknown);
            self.storage.intern(Type::Function(variable_kind, kind))
        })
    }

    /// Finds the terminal solution of a unification variable.
    pub fn force_unification(&self, mut t: TypeId) -> TypeId {
        while let Type::Unification(unification, _) = self.storage.index(t)
            && let UnificationState::Solved(solution) = self.unification.get(*unification).state
        {
            t = solution
        }
        t
    }

    pub fn normalize(&mut self, t: TypeId) -> TypeId {
        let t = self.force_unification(t);
        match *self.storage.index(t) {
            Type::Application(function, argument) => {
                let function = self.normalize(function);
                if let Type::Lambda(lambda) = *self.storage.index(function) {
                    let result = substitute::substitute_bound(self, argument, lambda);
                    self.normalize(result)
                } else {
                    t
                }
            }

            Type::Pruning(unification, ref pruning) => {
                let pruning = Arc::clone(pruning);
                self.apply_pruning(unification, pruning)
            }

            _ => t,
        }
    }

    fn apply_pruning(&mut self, unification: u32, pruning: Pruning) -> TypeId {
        let debruijn::Level(level) = self.bound.level();

        let spine = pruning.iter().enumerate().filter(|(_, keep)| **keep).map(|(index, _)| {
            let index = level - index as u32 - 1;
            let index = debruijn::Index(index);

            let variable = Variable::Bound(index);
            self.storage.intern(Type::Variable(variable))
        });

        let spine = spine.collect();
        let core = self.storage.intern(Type::Unification(unification, spine));

        self.normalize(core)
    }

    pub fn invert_spine(
        &self,
        codomain: debruijn::Level,
        spine: Spine,
    ) -> Option<(PartialRenaming, Option<Pruning>)> {
        let mut domain = debruijn::Level(0);
        let mut renaming = FxHashMap::default();

        let mut nonlinear = FxHashSet::default();
        let mut positions = vec![];

        for argument in spine.iter() {
            let argument = self.force_unification(*argument);
            if let Type::Variable(Variable::Bound(debruijn::Index(index))) =
                self.storage.index(argument)
            {
                let level = codomain.0 - index - 1;
                if renaming.contains_key(&level) || nonlinear.contains(&level) {
                    renaming.remove(&level);
                    nonlinear.insert(level);
                    positions.push(false);
                } else {
                    renaming.insert(level, domain);
                    positions.push(true);
                }
                domain = domain.increment();
            } else {
                return None;
            }
        }

        let partial_renaming = PartialRenaming { occurs: None, domain, codomain, renaming };
        let pruning = if nonlinear.is_empty() { None } else { Some(Arc::from(positions)) };

        Some((partial_renaming, pruning))
    }
}

impl<'s, S> CheckState<'s, S>
where
    S: TypeStorage,
{
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

pub struct PartialRenaming {
    /// Unification variable for the occurs check.
    pub occurs: Option<u32>,
    /// The size of the source context.
    pub domain: debruijn::Level,
    /// The size of the target context.
    pub codomain: debruijn::Level,
    /// The actual variable mapping.
    pub renaming: FxHashMap<u32, debruijn::Level>,
}
