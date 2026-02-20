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
use petgraph::algo;
use petgraph::prelude::DiGraphMap;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;

use crate::core::substitute::{SubstituteUnification, UnificationToType};
use crate::core::{ForallBinder, Type, TypeId, normalise};

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::state::{CheckState, UnificationEntry};

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
            Type::OperatorApplication(_, _, left, right) => {
                aux(graph, state, context, left, dependent, visited_kinds)?;
                aux(graph, state, context, right, dependent, visited_kinds)?;
            }
            Type::SynonymApplication(synonym_id) => {
                let synonym = context.lookup_synonym(synonym_id);
                for &argument in synonym.arguments.iter() {
                    aux(graph, state, context, argument, dependent, visited_kinds)?;
                }
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
            | Type::OperatorConstructor(_, _)
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

fn generate_type_name(id: u32) -> SmolStr {
    SmolStr::new(format!("t{id}"))
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
    let mut graph = UniGraph::new();
    collect_unification_into(&mut graph, state, context, id)?;

    if graph.node_count() == 0 {
        return Ok(id);
    }

    let Ok(unsolved) = algo::toposort(&graph, None) else {
        return Ok(id);
    };

    let mut quantified = id;
    let mut substitutions = UnificationToType::default();

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
        let UnificationEntry { kind, .. } = *state.unifications.get(unification_id);

        let text = generate_type_name(unification_id);
        let name = state.names.fresh();

        let binder = ForallBinder { visible: false, name, text, kind };
        let binder = context.intern_forall_binder(binder);
        quantified = context.intern_forall(binder, quantified);

        let rigid = context.intern_rigid(name, depth, kind);
        substitutions.insert(unification_id, rigid);
    }

    SubstituteUnification::on(state, context, &substitutions, quantified)
}
