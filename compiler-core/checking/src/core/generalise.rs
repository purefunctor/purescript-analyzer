//! Implements generalisation algorithms for the core representation.
//!
//! Simply put, generalisation is an operation that takes some inferred
//! type full of unsolved [unification variables] and replaces them with
//! [universally quantified] [rigid type variables]. For example:
//!
//! ```purescript
//! id :: ?0 -> ?0
//! ```
//!
//! this will generalise into the following:
//!
//! ```purescript
//! id :: forall (t0 :: Type). t0 -> t0
//! ```
//!
//! [unification variables]: crate::core::Type::Unification
//! [universally quantified]: crate::core::Type::Forall
//! [rigid type variables]: crate::core::Type::Rigid

use building_types::QueryResult;
use itertools::Itertools;
use petgraph::algo;
use petgraph::prelude::DiGraphMap;
use rustc_hash::FxHashSet;

use crate::context::CheckContext;
use crate::core::constraint::{CanonicalConstraintId, canonical, elaborate};
use crate::core::walk::{TypeWalker, WalkAction, walk_type};
use crate::core::{ForallBinder, Name, Type, TypeId, normalise, zonk};
use crate::state::{CheckState, UnificationEntry, UnificationState};
use crate::{ExternalQueries, safe_loop};

type UniGraph = DiGraphMap<u32, ()>;

fn collect_unification_into<Q>(
    graph: &mut UniGraph,
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    fn aux<Q>(
        graph: &mut UniGraph,
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: TypeId,
        dependent: Option<u32>,
        visited_kinds: &mut FxHashSet<u32>,
    ) -> QueryResult<()>
    where
        Q: ExternalQueries,
    {
        let id = normalise::normalise(state, context, id)?;
        let t = context.lookup_type(id);

        match t {
            Type::Application(function, argument) | Type::KindApplication(function, argument) => {
                aux(graph, state, context, function, dependent, visited_kinds)?;
                aux(graph, state, context, argument, dependent, visited_kinds)?;
            }
            Type::Forall(binder_id, inner) => {
                let binder = context.lookup_forall_binder(binder_id);
                aux(graph, state, context, binder.kind, dependent, visited_kinds)?;
                aux(graph, state, context, inner, dependent, visited_kinds)?;
            }
            Type::Constrained(constraint, inner) => {
                aux(graph, state, context, constraint, dependent, visited_kinds)?;
                aux(graph, state, context, inner, dependent, visited_kinds)?;
            }
            Type::Function(argument, result) => {
                aux(graph, state, context, argument, dependent, visited_kinds)?;
                aux(graph, state, context, result, dependent, visited_kinds)?;
            }
            Type::Kinded(inner, kind) => {
                aux(graph, state, context, inner, dependent, visited_kinds)?;
                aux(graph, state, context, kind, dependent, visited_kinds)?;
            }
            Type::Row(row_id) => {
                let row = context.lookup_row_type(row_id);
                for field in row.fields.iter() {
                    aux(graph, state, context, field.id, dependent, visited_kinds)?;
                }
                if let Some(tail) = row.tail {
                    aux(graph, state, context, tail, dependent, visited_kinds)?;
                }
            }
            Type::Rigid(_, _, kind) => {
                aux(graph, state, context, kind, dependent, visited_kinds)?;
            }
            Type::Unification(unification_id) => {
                graph.add_node(unification_id);

                if let Some(dependent_id) = dependent {
                    graph.add_edge(dependent_id, unification_id, ());
                }

                if visited_kinds.insert(unification_id) {
                    let entry = state.unifications.get(unification_id);
                    aux(graph, state, context, entry.kind, Some(unification_id), visited_kinds)?;
                }
            }
            Type::Constructor(_, _)
            | Type::Integer(_)
            | Type::String(_, _)
            | Type::Free(_)
            | Type::Unknown(_) => {}
        }

        Ok(())
    }

    let mut visited_kinds = FxHashSet::default();
    aux(graph, state, context, id, None, &mut visited_kinds)
}

/// Collect the unsolved unification variables in a type.
///
/// This function returns the unification variables topologically sorted
/// based on their dependencies, such as when unification variables appear
/// in another unification variable's kind.
pub fn unsolved_unifications<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Vec<u32>>
where
    Q: ExternalQueries,
{
    let mut graph = UniGraph::new();
    collect_unification_into(&mut graph, state, context, id)?;

    if graph.node_count() == 0 {
        return Ok(vec![]);
    }

    let Ok(unsolved) = algo::toposort(&graph, None) else {
        return Ok(vec![]);
    };

    Ok(unsolved)
}

fn collect_unification<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<UniGraph>
where
    Q: ExternalQueries,
{
    let mut graph = UniGraph::new();
    collect_unification_into(&mut graph, state, context, id)?;
    Ok(graph)
}

/// Generalise a type with the given unification variables.
///
/// The `unsolved` parameter should be sourced from [`unsolved_unifications`].
/// This split is necessary for generalisation on mutually-recursive bindings.
/// Note that while this function expects unsolved unification variables, it
/// also handles solved ones gracefully in the event that they become solved
/// before being generalised.
pub fn generalise_unsolved<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
    unsolved: &[u32],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if unsolved.is_empty() {
        return Ok(id);
    }

    let mut quantified = id;

    // All rigid type variables in a single generalisation share the same
    // depth, one level deeper than the ambient scope. Note that the depth
    // refers to the nesting level of forall scopes with respect to higher
    // rank types, not the number of bindings introduced. For example,
    //
    //   forall a b. a -> b -> a
    //
    // has `a` and `b` on the same depth, whereas,
    //
    //   forall a. (forall r. ST r a) -> a
    //   forall a. a -> (forall b. b -> a)
    //
    // have `r` and `b` one level deeper. Note that the latter example is
    // actually still a Rank-1 type; a forall can be floated trivially when
    // it occurs to the right of the function arrow.
    //
    //   forall a. a -> (forall b. b -> a)
    //   forall a b. a -> b -> a
    //
    // See also: https://wiki.haskell.org/Rank-N_types
    let depth = state.depth.increment();

    for &unification_id in unsolved.iter() {
        let UnificationEntry { kind, state: unification_state, .. } =
            *state.unifications.get(unification_id);

        let (name, kind) = match unification_state {
            UnificationState::Unsolved => {
                let name = state.names.fresh();
                let rigid = context.intern_rigid(name, depth, kind);
                state.unifications.solve(unification_id, rigid);
                (name, kind)
            }
            UnificationState::Solved(solution) => {
                let solution = normalise::expand(state, context, solution)?;
                let Type::Rigid(name, _, kind) = context.lookup_type(solution) else {
                    continue;
                };
                (name, kind)
            }
        };

        let binder = ForallBinder { visible: false, name, kind };
        let binder = context.intern_forall_binder(binder);
        quantified = context.intern_forall(binder, quantified);
    }

    zonk::zonk(state, context, quantified)
}

/// Generalises a given type. See also module-level documentation.
pub fn generalise<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let unsolved = unsolved_unifications(state, context, id)?;
    generalise_unsolved(state, context, id, &unsolved)
}

#[derive(Default)]
pub struct ConstraintErrors {
    pub ambiguous: Vec<CanonicalConstraintId>,
    pub unsatisfied: Vec<CanonicalConstraintId>,
}

pub fn insert_inferred_residuals<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    residuals: Vec<CanonicalConstraintId>,
    errors: &mut ConstraintErrors,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if residuals.is_empty() {
        return Ok(type_id);
    }

    let mut pending = vec![];
    let mut latent = vec![];

    for residual in residuals {
        let residual = canonical::zonk_canonical(state, context, residual)?;

        if is_partial_constraint(state, context, residual) {
            latent.push(residual);
            continue;
        }

        let residual_type = state.canonicals.type_id(context, residual);
        let unification: FxHashSet<_> =
            collect_unification(state, context, residual_type)?.nodes().collect();

        if unification.is_empty() {
            errors.unsatisfied.push(residual);
        } else {
            pending.push((residual, unification));
        }
    }

    let unifications = collect_unification(state, context, type_id)?.nodes().collect();
    let generalised = classify_constraints(pending, unifications, errors);

    let generalised = latent.into_iter().chain(generalised).sorted().collect_vec();
    let generalised = minimize_by_superclasses(state, context, generalised)?;

    let constrained = generalised.into_iter().rfold(type_id, |inner, constraint| {
        let constraint = state.canonicals.type_id(context, constraint);
        context.intern_constrained(constraint, inner)
    });

    Ok(constrained)
}

fn is_partial_constraint<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    constraint: CanonicalConstraintId,
) -> bool
where
    Q: ExternalQueries,
{
    let Type::Constructor(file_id, type_id) = context.lookup_type(context.prim.partial) else {
        return false;
    };
    let constraint = &state.canonicals[constraint];
    constraint.file_id == file_id
        && constraint.type_id == type_id
        && constraint.arguments.is_empty()
}

fn minimize_by_superclasses<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraints: Vec<CanonicalConstraintId>,
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    if constraints.len() <= 1 {
        return Ok(constraints);
    }

    let mut superclasses = FxHashSet::default();
    for &constraint in &constraints {
        for superclass in elaborate::elaborate_superclasses(state, context, &[constraint])? {
            if superclass != constraint {
                superclasses.insert(superclass);
            }
        }
    }

    Ok(constraints.into_iter().filter(|constraint| !superclasses.contains(constraint)).collect())
}

fn classify_constraints(
    pending: Vec<(CanonicalConstraintId, FxHashSet<u32>)>,
    unifications: FxHashSet<u32>,
    errors: &mut ConstraintErrors,
) -> FxHashSet<CanonicalConstraintId> {
    let mut reachable = unifications;
    let mut valid = FxHashSet::default();
    let mut remaining = pending;

    safe_loop! {
        let (connected, disconnected): (Vec<_>, Vec<_>) =
            remaining.into_iter().partition(|(_, unification)| {
                unification.iter().any(|variable| reachable.contains(variable))
            });

        if connected.is_empty() {
            break errors.ambiguous.extend(disconnected.into_iter().map(|(id, _)| id));
        }

        for (constraint, unification) in connected {
            valid.insert(constraint);
            reachable.extend(unification);
        }

        remaining = disconnected;
    }

    valid
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ImplicitOrUnification {
    Implicit(Name, TypeId),
    Unification(u32, TypeId),
}

#[derive(Default)]
struct GeneraliseImplicit {
    owner: Option<ImplicitOrUnification>,
    graph: DiGraphMap<ImplicitOrUnification, ()>,
    bound: FxHashSet<Name>,
}

impl TypeWalker for GeneraliseImplicit {
    fn visit<Q>(
        &mut self,
        state: &mut CheckState,
        context: &CheckContext<Q>,
        _id: TypeId,
        t: &Type,
    ) -> QueryResult<WalkAction>
    where
        Q: ExternalQueries,
    {
        match t {
            Type::Rigid(name, _, kind) => {
                let next_owner = ImplicitOrUnification::Implicit(*name, *kind);
                let prev_owner = self.owner.replace(next_owner);

                self.graph.add_node(next_owner);
                if let Some(prev_owner) = prev_owner {
                    self.graph.add_edge(prev_owner, next_owner, ());
                }

                walk_type(state, context, *kind, self)?;
                self.owner = prev_owner;
            }
            Type::Unification(id) => {
                let UnificationEntry { kind, .. } = state.unifications.get(*id);

                let next_owner = ImplicitOrUnification::Unification(*id, *kind);
                let prev_owner = self.owner.replace(next_owner);

                self.graph.add_node(next_owner);
                if let Some(prev_owner) = prev_owner {
                    self.graph.add_edge(prev_owner, next_owner, ());
                }

                walk_type(state, context, *kind, self)?;
                self.owner = prev_owner;
            }
            _ => {}
        }
        Ok(WalkAction::Continue)
    }

    fn visit_binder(&mut self, binder: &ForallBinder) {
        self.bound.insert(binder.name);
    }
}

pub fn generalise_implicit<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut walker = GeneraliseImplicit::default();
    walk_type(state, context, id, &mut walker)?;

    let Ok(implicits_unifications) = algo::toposort(&walker.graph, None) else {
        return Ok(context.unknown("invalid recursive graph"));
    };

    let depth = state.depth.increment();

    let mut binders = vec![];
    for implicit_unification in implicits_unifications {
        match implicit_unification {
            ImplicitOrUnification::Implicit(name, kind) => {
                binders.push(ForallBinder { visible: false, name, kind })
            }
            ImplicitOrUnification::Unification(id, kind) => {
                let name = state.names.fresh();
                let rigid = context.intern_rigid(name, depth, kind);
                state.unifications.solve(id, rigid);
                binders.push(ForallBinder { visible: false, name, kind })
            }
        }
    }

    let id = binders.into_iter().fold(id, |inner, binder| {
        let binder = context.intern_forall_binder(binder);
        context.intern_forall(binder, inner)
    });

    zonk::zonk(state, context, id)
}
