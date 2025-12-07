//! Implements type quantification.

use std::fmt::Write;

use indexmap::IndexSet;
use petgraph::prelude::DiGraphMap;
use petgraph::visit::{DfsPostOrder, Reversed};
use rustc_hash::FxHashMap;
use smol_str::SmolStrBuilder;

use crate::algorithm::state::CheckState;
use crate::core::{ForallBinder, Type, TypeId, Variable, debruijn};

pub fn quantify(state: &mut CheckState, id: TypeId) -> Option<(TypeId, debruijn::Size)> {
    let graph = collect_unification(state, id);

    if graph.node_count() == 0 {
        return Some((id, debruijn::Size(0)));
    }

    let unsolved = ordered_toposort(&graph, state)?;

    let size = {
        let size = unsolved.len();
        debruijn::Size(size as u32)
    };

    let mut quantified = id;
    let mut substitutions = UniToLevel::default();

    for (index, &id) in unsolved.iter().rev().enumerate() {
        let kind = state.unification.get(id).kind;
        let name = generate_type_name(id);

        let index = debruijn::Index(index as u32);
        let level = index
            .to_level(size)
            .unwrap_or_else(|| unreachable!("invariant violated: invalid {index} for {size}"));

        let binder = ForallBinder { visible: false, name, level, kind };
        quantified = state.storage.intern(Type::Forall(binder, quantified));

        substitutions.insert(id, level);
    }

    let quantified = substitute_unification(&substitutions, state, quantified);

    Some((quantified, size))
}

fn generate_type_name(id: u32) -> smol_str::SmolStr {
    let mut builder = SmolStrBuilder::default();
    write!(builder, "t{id}").unwrap();
    builder.finish()
}

/// Builds a topological sort of the [`UniGraph`].
///
/// This function uses the domain-based sorting of the unification variables
/// as the base for the post-order traversal. In turn, this ensures that
/// unconnected nodes are ordered by domain while connected ones are sorted
/// topologically. The resulting [`IndexSet`] can be iterated in reverse to
/// build the `forall` binders during quantification.
fn ordered_toposort(graph: &UniGraph, state: &CheckState) -> Option<IndexSet<u32>> {
    let mut nodes: Vec<u32> = graph.nodes().collect();
    nodes.sort_by_key(|&id| (state.unification.get(id).domain, id));

    let mut dfs = DfsPostOrder::empty(graph);
    let mut unsolved = IndexSet::new();

    for node in nodes {
        if graph.contains_edge(node, node) {
            return None;
        }
        dfs.move_to(node);
        while let Some(visited) = dfs.next(graph) {
            unsolved.insert(visited);
        }
    }

    dfs.reset(graph);
    for &node in unsolved.iter().rev() {
        dfs.move_to(node);
        let mut cycle = false;
        while dfs.next(Reversed(graph)).is_some() {
            if cycle {
                return None;
            }
            cycle = true;
        }
    }

    Some(unsolved)
}

type UniGraph = DiGraphMap<u32, ()>;

/// Collects unification variables in a [`Type`].
///
/// This function also tracks the dependencies between unification
/// variables such as when unification variables appear in another
/// unification variable's kind.
fn collect_unification(state: &mut CheckState, id: TypeId) -> UniGraph {
    fn aux(graph: &mut UniGraph, state: &mut CheckState, id: TypeId, dependent: Option<u32>) {
        let id = state.normalize_type(id);
        match state.storage[id] {
            Type::Application(function, argument) => {
                aux(graph, state, function, dependent);
                aux(graph, state, argument, dependent);
            }
            Type::Constrained(constraint, inner) => {
                aux(graph, state, constraint, dependent);
                aux(graph, state, inner, dependent);
            }
            Type::Constructor(_, _) => (),
            Type::Forall(ref binder, inner) => {
                aux(graph, state, binder.kind, dependent);
                aux(graph, state, inner, dependent);
            }
            Type::Function(argument, result) => {
                aux(graph, state, argument, dependent);
                aux(graph, state, result, dependent);
            }
            Type::Integer(_) => (),
            Type::KindApplication(function, argument) => {
                aux(graph, state, function, dependent);
                aux(graph, state, argument, dependent);
            }
            Type::Kinded(inner, kind) => {
                aux(graph, state, inner, dependent);
                aux(graph, state, kind, dependent);
            }
            Type::Operator(_, _) => (),
            Type::OperatorApplication(_, _, left, right) => {
                aux(graph, state, left, dependent);
                aux(graph, state, right, dependent);
            }
            Type::String(_, _) => (),
            Type::Unification(unification_id) => {
                graph.add_node(unification_id);

                if let Some(dependent_id) = dependent {
                    graph.add_edge(dependent_id, unification_id, ());
                }

                let entry = state.unification.get(unification_id);
                aux(graph, state, entry.kind, Some(unification_id));
            }
            Type::Variable(_) => (),
            Type::Unknown => (),
        }
    }

    let mut graph = UniGraph::default();
    aux(&mut graph, state, id, None);
    graph
}

type UniToLevel = FxHashMap<u32, debruijn::Level>;

/// Level-based substitution over a [`Type`].
///
/// A direct mapping from unification variables to [`debruijn::Index`] is
/// insufficient because of the scoping rule for [`ForallBinder::kind`].
///
/// Instead, we use a [`debruijn::Level`]-based approach alongside
/// explicit [`debruijn::Size`] tracking to get the proper indices.
fn substitute_unification(
    substitutions: &UniToLevel,
    state: &mut CheckState,
    id: TypeId,
) -> TypeId {
    fn aux(
        substitutions: &UniToLevel,
        state: &mut CheckState,
        id: TypeId,
        size: debruijn::Size,
    ) -> TypeId {
        let id = state.normalize_type(id);
        match state.storage[id] {
            Type::Application(function, argument) => {
                let function = aux(substitutions, state, function, size);
                let argument = aux(substitutions, state, argument, size);
                state.storage.intern(Type::Application(function, argument))
            }
            Type::Constrained(constraint, inner) => {
                let constraint = aux(substitutions, state, constraint, size);
                let inner = aux(substitutions, state, inner, size);
                state.storage.intern(Type::Constrained(constraint, inner))
            }
            Type::Constructor(_, _) => id,
            Type::Forall(ref binder, inner) => {
                let mut binder = binder.clone();

                binder.kind = aux(substitutions, state, binder.kind, size);
                let inner = aux(substitutions, state, inner, size.increment());

                state.storage.intern(Type::Forall(binder, inner))
            }
            Type::Function(argument, result) => {
                let argument = aux(substitutions, state, argument, size);
                let result = aux(substitutions, state, result, size);
                state.storage.intern(Type::Function(argument, result))
            }
            Type::Integer(_) => id,
            Type::KindApplication(function, argument) => {
                let function = aux(substitutions, state, function, size);
                let argument = aux(substitutions, state, argument, size);
                state.storage.intern(Type::KindApplication(function, argument))
            }
            Type::Kinded(inner, kind) => {
                let inner = aux(substitutions, state, inner, size);
                let kind = aux(substitutions, state, kind, size);
                state.storage.intern(Type::Kinded(inner, kind))
            }
            Type::Operator(_, _) => id,
            Type::OperatorApplication(file_id, type_id, left, right) => {
                let left = aux(substitutions, state, left, size);
                let right = aux(substitutions, state, right, size);
                state.storage.intern(Type::OperatorApplication(file_id, type_id, left, right))
            }
            Type::String(_, _) => id,
            Type::Unification(unification_id) => {
                let Some(level) = substitutions.get(&unification_id) else { return id };
                let index = level.to_index(size).unwrap_or_else(|| {
                    unreachable!("invariant violated: invalid {level} for {size}")
                });
                state.storage.intern(Type::Variable(Variable::Bound(index)))
            }
            Type::Variable(_) => id,
            Type::Unknown => id,
        }
    }

    aux(substitutions, state, id, debruijn::Size(0))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn add_unification(state: &mut CheckState, domain: u32) -> u32 {
        let kind = state.storage.intern(Type::Unknown);
        state.unification.fresh(debruijn::Size(domain), kind)
    }

    #[test]
    fn test_toposort_dag() {
        let mut state = CheckState::default();
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 0);
        let id1 = add_unification(&mut state, 0);
        let id2 = add_unification(&mut state, 0);

        // id0 -> id1 -> id2
        graph.add_edge(id0, id1, ());
        graph.add_edge(id1, id2, ());

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_some());

        let sorted: Vec<u32> = result.unwrap().into_iter().collect();
        assert_eq!(sorted, vec![id2, id1, id0]);
    }

    #[test]
    fn test_toposort_tuple_cycle() {
        let mut state = CheckState::default();
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 0);
        let id1 = add_unification(&mut state, 0);

        // id0 -> id1
        // id1 -> id0
        graph.add_edge(id0, id1, ());
        graph.add_edge(id1, id0, ());

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_none(), "Should detect simple 2-node cycle");
    }

    #[test]
    fn test_toposort_self_cycle() {
        let mut state = CheckState::default();
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 0);

        // id0 -> id0
        graph.add_edge(id0, id0, ());

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_none(), "Should detect self-loop as a cycle");
    }

    #[test]
    fn test_toposort_triple_cycle() {
        let mut state = CheckState::default();
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 0);
        let id1 = add_unification(&mut state, 0);
        let id2 = add_unification(&mut state, 0);

        // id0 -> id1 -> id2 -> id0
        graph.add_edge(id0, id1, ());
        graph.add_edge(id1, id2, ());
        graph.add_edge(id2, id0, ());

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_none(), "Should detect 3-node cycle");
    }

    #[test]
    fn test_toposort_domain_ordering() {
        let mut state = CheckState::default();
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 1);
        let id1 = add_unification(&mut state, 2);

        graph.add_node(id0);
        graph.add_node(id1);

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_some());

        let sorted: Vec<u32> = result.unwrap().into_iter().collect();
        assert_eq!(sorted, vec![id0, id1]);
    }

    #[test]
    fn test_toposort_id_ordering() {
        let mut state = CheckState::default();
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 0);
        let id1 = add_unification(&mut state, 0);

        graph.add_node(id0);
        graph.add_node(id1);

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_some());

        let sorted: Vec<u32> = result.unwrap().into_iter().collect();
        assert_eq!(sorted, vec![id0, id1]);
    }

    #[test]
    fn test_toposort_dependency_ordering() {
        let mut state = CheckState::default();
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 2);
        let id1 = add_unification(&mut state, 1);

        // id0 -> id1
        graph.add_edge(id0, id1, ());

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_some());

        // Dependency order takes precedence
        let sorted: Vec<u32> = result.unwrap().into_iter().collect();
        assert_eq!(sorted, vec![id1, id0]);
    }

    #[test]
    fn test_toposort_diamond() {
        let mut state = CheckState::default();
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 0);
        let id1 = add_unification(&mut state, 0);
        let id2 = add_unification(&mut state, 0);
        let id3 = add_unification(&mut state, 0);

        graph.add_edge(id0, id2, ());
        graph.add_edge(id0, id3, ());
        graph.add_edge(id1, id2, ());
        graph.add_edge(id1, id3, ());

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_some());

        let sorted: Vec<u32> = result.unwrap().into_iter().collect();

        // All have the same domain,
        assert_eq!(sorted, vec![id3, id2, id0, id1]);
    }
}
