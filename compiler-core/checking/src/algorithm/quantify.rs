use std::fmt::Write;
use std::sync::Arc;

use building_types::QueryResult;
use indexmap::IndexSet;
use itertools::Itertools;
use petgraph::prelude::DiGraphMap;
use petgraph::visit::{DfsPostOrder, Reversed};
use rustc_hash::FxHashSet;
use smol_str::SmolStrBuilder;

use crate::algorithm::constraint::{self, ConstraintApplication};
use crate::algorithm::fold::Zonk;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::substitute::{SubstituteUnification, UniToName};
use crate::core::{Class, ForallBinder, Instance, RowType, Type, TypeId, Variable, debruijn};
use crate::{ExternalQueries, safe_loop};

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
    let mut substitutions = UniToName::default();

    for &id in unsolved.iter().rev() {
        let kind = state.unification.get(id).kind;
        let text = generate_type_name(id);
        let mut name = state.fresh_name(&text);
        name.depth = debruijn::Size(0);

        let binder =
            ForallBinder { visible: false, implicit: true, text, variable: name.clone(), kind };
        quantified = state.storage.intern(Type::Forall(binder, quantified));

        substitutions.insert(id, (name, kind));
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

    let mut pending = vec![];
    let mut unsatisfied = vec![];
    let mut latent = vec![];

    for constraint in constraints {
        let constraint = Zonk::on(state, constraint);

        // Partial is a latent constraint, it has no type arguments so it
        // never has unification variables, but should be generalised
        // rather than reported as unsatisfied. This allows inferring
        // `Partial => Int` for expressions with non-exhaustive patterns.
        if constraint == context.prim.partial {
            latent.push(constraint);
            continue;
        }

        let unification: FxHashSet<u32> = collect_unification(state, constraint).nodes().collect();
        if unification.is_empty() {
            unsatisfied.push(constraint);
        } else {
            pending.push((constraint, unification));
        }
    }

    let in_signature = collect_unification(state, type_id).nodes().collect();
    let (generalised, ambiguous) = classify_constraints_by_reachability(pending, in_signature);

    // Subtle: stable ordering for consistent output
    let generalised = latent.into_iter().chain(generalised).sorted().collect_vec();
    let minimized = minimize_by_superclasses(state, context, generalised)?;

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
        constraint::constraint_application(state, constraint)
            .is_none_or(|constraint| !superclasses.contains(&constraint))
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
    constraint::elaborate_superclasses(state, context, constraint, &mut superclasses)?;
    Ok(superclasses
        .into_iter()
        .filter_map(|constraint| constraint::constraint_application(state, constraint))
        .collect())
}

/// Classifies constraints as valid or ambiguous based on variable reachability.
///
/// A constraint is valid if its variables are transitively reachable from the
/// signature variables. The algorithm uses fixed-point iteration. As long as
/// a constraint shares any variable with the reachable set, all its variables
/// become reachable.
fn classify_constraints_by_reachability(
    pending: Vec<(TypeId, FxHashSet<u32>)>,
    in_signature: FxHashSet<u32>,
) -> (FxHashSet<TypeId>, Vec<TypeId>) {
    let mut reachable = in_signature;
    let mut valid = FxHashSet::default();
    let mut remaining = pending;

    safe_loop! {
        let (connected, disconnected): (Vec<_>, Vec<_>) =
            remaining.into_iter().partition(|(_, unification)| {
                unification.iter().any(|variable| reachable.contains(variable))
            });

        if connected.is_empty() {
            let ambiguous = disconnected.into_iter().map(|(id, _)| id).collect();
            return (valid, ambiguous);
        }

        for (constraint, unification) in connected {
            valid.insert(constraint);
            reachable.extend(unification);
        }

        remaining = disconnected;
    }
}

pub(crate) fn generate_type_name(id: u32) -> smol_str::SmolStr {
    let mut builder = SmolStrBuilder::default();
    write!(builder, "t{id}").unwrap();
    builder.finish()
}

/// Quantifies unification variables in class type variable kinds.
///
/// When a class type variable does not have an explicit kind, a unification
/// variable is created and potentially generalised if left unsolved. This
/// function applies the same generalisation algorithm used in the kind signature.
///
/// This function returns the number of additional kind variables introduced.
pub fn quantify_class(state: &mut CheckState, class: &mut Class) -> Option<debruijn::Size> {
    let mut graph = UniGraph::default();
    for &kind in &class.type_variable_kinds {
        collect_unification_into(&mut graph, state, kind);
    }

    for (t, k) in class.superclasses.iter() {
        collect_unification_into(&mut graph, state, *t);
        collect_unification_into(&mut graph, state, *k);
    }

    if graph.node_count() == 0 {
        return Some(debruijn::Size(0));
    }

    let unsolved = ordered_toposort(&graph, state)?;
    let size = debruijn::Size(unsolved.len() as u32);

    let mut substitutions = UniToName::default();
    for &id in unsolved.iter().rev() {
        let kind = state.unification.get(id).kind;
        let text = generate_type_name(id);
        let mut name = state.fresh_name(&text);
        name.depth = debruijn::Size(0);
        substitutions.insert(id, (name, kind));
    }

    let type_variable_kinds = class
        .type_variable_kinds
        .iter()
        .map(|&kind| SubstituteUnification::on(&substitutions, state, kind));

    class.type_variable_kinds = type_variable_kinds.collect();

    let superclasses = class.superclasses.iter().map(|&(t, k)| {
        let t = SubstituteUnification::on(&substitutions, state, t);
        let k = SubstituteUnification::on(&substitutions, state, k);
        (t, k)
    });

    class.superclasses = superclasses.collect();

    Some(size)
}

/// Quantifies unification variables in instance argument and constraint kinds.
///
/// When an instance type variable does not have an explicit kind, a unification
/// variable is created and potentially generalised if left unsolved. This function
/// applies the same generalisation algorithm as [`quantify`].
///
/// This function returns the number of additional kind variables introduced.
pub fn quantify_instance(state: &mut CheckState, instance: &mut Instance) -> Option<()> {
    let mut graph = UniGraph::default();

    for (t, k) in &instance.arguments {
        collect_unification_into(&mut graph, state, *t);
        collect_unification_into(&mut graph, state, *k);
    }

    for (t, k) in &instance.constraints {
        collect_unification_into(&mut graph, state, *t);
        collect_unification_into(&mut graph, state, *k);
    }

    if graph.node_count() == 0 {
        return Some(());
    }

    let unsolved = ordered_toposort(&graph, state)?;

    let mut substitutions = UniToName::default();
    for &id in unsolved.iter().rev() {
        let kind = state.unification.get(id).kind;
        let text = generate_type_name(id);
        let mut name = state.fresh_name(&text);
        name.depth = debruijn::Size(0);
        substitutions.insert(id, (name, kind));
    }

    let kind_variables = substitutions.values().cloned();
    let kind_variables = kind_variables.sorted_by_key(|(name, _)| name.unique);
    let kind_variables = kind_variables.map(|(name, kind)| (name, kind)).collect_vec();

    let arguments = instance.arguments.iter().map(|&(t, k)| {
        let t = SubstituteUnification::on(&substitutions, state, t);
        let k = SubstituteUnification::on(&substitutions, state, k);
        (t, k)
    });

    instance.arguments = arguments.collect();

    let constraints = instance.constraints.iter().map(|&(t, k)| {
        let t = SubstituteUnification::on(&substitutions, state, t);
        let k = SubstituteUnification::on(&substitutions, state, k);
        (t, k)
    });

    instance.constraints = constraints.collect();

    instance.kind_variables = kind_variables;

    Some(())
}

/// Builds a topological sort of the [`UniGraph`].
///
/// This function uses the depth-based sorting of the unification variables
/// as the base for the post-order traversal. In turn, this ensures that
/// unconnected nodes are ordered by depth while connected ones are sorted
/// topologically. The resulting [`IndexSet`] can be iterated in reverse to
/// build the `forall` binders during quantification.
fn ordered_toposort(graph: &UniGraph, state: &CheckState) -> Option<IndexSet<u32>> {
    let mut nodes: Vec<u32> = graph.nodes().collect();
    nodes.sort_by_key(|&id| (state.unification.get(id).depth, id));

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

/// Collects unification variables from a [`Type`] into an existing graph.
///
/// This function also tracks the dependencies between unification
/// variables such as when unification variables appear in another
/// unification variable's kind.
pub fn collect_unification_into(graph: &mut UniGraph, state: &mut CheckState, id: TypeId) {
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
            Type::Variable(ref variable) => match variable {
                Variable::Bound(_, kind) | Variable::Skolem(_, kind) => {
                    aux(graph, state, *kind, dependent);
                }
                Variable::Free(_) => {}
            },
            Type::Unknown => (),
        }
    }

    aux(graph, state, id, None);
}

/// Collects unification variables in a [`Type`].
///
/// This function also tracks the dependencies between unification
/// variables such as when unification variables appear in another
/// unification variable's kind.
fn collect_unification(state: &mut CheckState, id: TypeId) -> UniGraph {
    let mut graph = UniGraph::default();
    collect_unification_into(&mut graph, state, id);
    graph
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_file_id() -> files::FileId {
        let mut files = files::Files::default();
        files.insert("Test.purs", "module Test where\n\n")
    }

    fn add_unification(state: &mut CheckState, depth: u32) -> u32 {
        let kind = state.storage.intern(Type::Unknown);
        state.unification.fresh(debruijn::Size(depth), kind)
    }

    #[test]
    fn test_toposort_dag() {
        let mut state = CheckState::new(test_file_id());
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
        let mut state = CheckState::new(test_file_id());
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
        let mut state = CheckState::new(test_file_id());
        let mut graph = UniGraph::default();

        let id0 = add_unification(&mut state, 0);

        // id0 -> id0
        graph.add_edge(id0, id0, ());

        let result = ordered_toposort(&graph, &state);
        assert!(result.is_none(), "Should detect self-loop as a cycle");
    }

    #[test]
    fn test_toposort_triple_cycle() {
        let mut state = CheckState::new(test_file_id());
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
        let mut state = CheckState::new(test_file_id());
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
        let mut state = CheckState::new(test_file_id());
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
        let mut state = CheckState::new(test_file_id());
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
        let mut state = CheckState::new(test_file_id());
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

        // All have the same depth,
        assert_eq!(sorted, vec![id3, id2, id0, id1]);
    }
}
