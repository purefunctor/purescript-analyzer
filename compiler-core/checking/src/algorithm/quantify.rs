use std::fmt::Write;
use std::sync::Arc;

use building_types::QueryResult;
use indexmap::IndexSet;
use itertools::Itertools;
use petgraph::prelude::DiGraphMap;
use petgraph::visit::{DfsPostOrder, Reversed};
use rustc_hash::FxHashSet;
use smol_str::SmolStrBuilder;

use crate::ExternalQueries;
use crate::algorithm::constraint::{
    ConstraintApplication, constraint_application, elaborate_superclasses,
};
use crate::algorithm::fold::Zonk;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::substitute::{ShiftLevels, SubstituteUnification, UniToLevel};
use crate::core::{ForallBinder, RowType, Type, TypeId, debruijn};

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

    // Shift existing bound variable levels to make room for new quantifiers
    let mut quantified = ShiftLevels::on(state, id, size.0);
    let mut substitutions = UniToLevel::default();

    for (index, &id) in unsolved.iter().rev().enumerate() {
        let kind = state.unification.get(id).kind;
        let kind = ShiftLevels::on(state, kind, size.0);
        let name = generate_type_name(id);

        let index = debruijn::Index(index as u32);
        let level = index
            .to_level(size)
            .unwrap_or_else(|| unreachable!("invariant violated: invalid {index} for {size}"));

        let binder = ForallBinder { visible: false, name, level, kind };
        quantified = state.storage.intern(Type::Forall(binder, quantified));

        substitutions.insert(id, level);
    }

    let quantified = SubstituteUnification::on(&substitutions, state, quantified);

    Some((quantified, size))
}

/// The result of generalisation including constraints.
pub struct QuantifiedWithConstraints {
    /// The quantified type with generalisable constraints.
    pub quantified: TypeId,
    /// The number of quantified type variables.
    pub size: debruijn::Size,
    /// Constraints with unification variables not appearing in the signature.
    pub ambiguous: Vec<TypeId>,
    /// Constraints with no unification variables (fully concrete & unsatisfied).
    pub unsatisfied: Vec<TypeId>,
}

/// Quantifies a type while incorporating residual constraints.
///
/// This function partitions the residual constraints into three categories:
/// - Generalisable: has unification variables that all appear in the signature
/// - Ambiguous: has unification variables that don't appear in the signature
/// - Unsatisfied: has no unification variables, a concrete constraint
///
/// Generalisable constraints are added to the signature before generalisation.
/// Ambiguous and unsatisfied constraints are returned for error reporting.
pub fn quantify_with_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    constraints: Vec<TypeId>,
) -> QueryResult<Option<QuantifiedWithConstraints>>
where
    Q: ExternalQueries,
{
    if constraints.is_empty() {
        let Some((quantified, size)) = quantify(state, type_id) else {
            return Ok(None);
        };

        let quantified_with_constraints =
            QuantifiedWithConstraints { quantified, size, ambiguous: vec![], unsatisfied: vec![] };

        return Ok(Some(quantified_with_constraints));
    }

    let unsolved_graph = collect_unification(state, type_id);
    let unsolved_nodes: FxHashSet<u32> = unsolved_graph.nodes().collect();

    let mut valid: FxHashSet<TypeId> = FxHashSet::default();
    let mut ambiguous = vec![];
    let mut unsatisfied = vec![];

    for constraint in constraints {
        let constraint = Zonk::on(state, constraint);
        let unsolved_graph = collect_unification(state, constraint);
        if unsolved_graph.node_count() == 0 {
            unsatisfied.push(constraint);
        } else if unsolved_graph.nodes().all(|unification| unsolved_nodes.contains(&unification)) {
            valid.insert(constraint);
        } else {
            ambiguous.push(constraint);
        }
    }

    // Subtle: stable ordering for consistent output
    let valid = valid.into_iter().sorted().collect_vec();
    let minimized = minimize_by_superclasses(state, context, valid)?;

    let constrained_type = minimized.into_iter().rfold(type_id, |constrained, constraint| {
        state.storage.intern(Type::Constrained(constraint, constrained))
    });

    let Some((quantified, size)) = quantify(state, constrained_type) else {
        return Ok(None);
    };

    let quantified_with_constraints =
        QuantifiedWithConstraints { quantified, size, ambiguous, unsatisfied };

    Ok(Some(quantified_with_constraints))
}

/// Removes constraints that are implied by other constraints via superclass relationships.
///
/// For example, given constraints `[Apply f, Applicative f, Functor f]`, this function
/// returns only `[Applicative f]` because `Applicative` implies both `Apply` and `Functor`.
///
/// Uses GHC's algorithm (mkMinimalBySCs): O(n × superclass_depth)
/// 1. Build the union of all superclass constraints
/// 2. Filter out constraints that appear in the union
fn minimize_by_superclasses<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraints: Vec<TypeId>,
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    if constraints.len() <= 1 {
        return Ok(constraints);
    }

    // Collect the set of all superclasses, including transitive ones.
    let mut superclasses = FxHashSet::default();
    for &constraint in &constraints {
        for application in superclass_applications(state, context, constraint)? {
            superclasses.insert(application);
        }
    }

    // Remove constraints found in the superclasses, keeping the most specific.
    let minimized = constraints.into_iter().filter(|&constraint| {
        constraint_application(state, constraint)
            .map_or(true, |constraint| !superclasses.contains(&constraint))
    });

    Ok(minimized.collect_vec())
}

fn superclass_applications<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraint: TypeId,
) -> QueryResult<Vec<ConstraintApplication>>
where
    Q: ExternalQueries,
{
    let mut superclasses = vec![];
    elaborate_superclasses(state, context, constraint, &mut superclasses)?;
    Ok(superclasses
        .into_iter()
        .filter_map(|constraint| constraint_application(state, constraint))
        .collect())
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
            Type::Row(RowType { ref fields, tail }) => {
                let fields = Arc::clone(fields);
                for field in fields.iter() {
                    aux(graph, state, field.id, dependent);
                }
                if let Some(tail) = tail {
                    aux(graph, state, tail, dependent);
                }
            }
            Type::String(_, _) => (),
            Type::SynonymApplication(_, _, _, ref arguments) => {
                let arguments = Arc::clone(arguments);
                for argument in arguments.iter() {
                    aux(graph, state, *argument, dependent);
                }
            }
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
