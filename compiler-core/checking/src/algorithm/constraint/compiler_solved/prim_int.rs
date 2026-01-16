use std::cmp::Ordering;

use lowering::StringKind;
use petgraph::algo::has_path_connecting;
use petgraph::graphmap::DiGraphMap;
use smol_str::SmolStr;

use crate::algorithm::constraint::{self, MatchInstance};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::{ExternalQueries, Type, TypeId};

use super::extract_integer;

pub fn prim_int_add(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[left, right, sum] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let sum = state.normalize_type(sum);

    let left_int = extract_integer(state, left);
    let right_int = extract_integer(state, right);
    let sum_int = extract_integer(state, sum);

    match (left_int, right_int, sum_int) {
        (Some(left), Some(right), _) => {
            let result = state.storage.intern(Type::Integer(left + right));
            if constraint::can_unify(state, sum, result).is_apart() {
                return Some(MatchInstance::Apart);
            }
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(sum, result)] })
        }
        (Some(left), _, Some(sum)) => {
            let result = state.storage.intern(Type::Integer(sum - left));
            if constraint::can_unify(state, right, result).is_apart() {
                return Some(MatchInstance::Apart);
            }
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(right, result)] })
        }
        (_, Some(right), Some(sum)) => {
            let result = state.storage.intern(Type::Integer(sum - right));
            if constraint::can_unify(state, left, result).is_apart() {
                return Some(MatchInstance::Apart);
            }
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(left, result)] })
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_int_mul(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[left, right, product] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let product = state.normalize_type(product);

    let Some(left_int) = extract_integer(state, left) else {
        return Some(MatchInstance::Stuck);
    };

    let Some(right_int) = extract_integer(state, right) else {
        return Some(MatchInstance::Stuck);
    };

    let result = state.storage.intern(Type::Integer(left_int * right_int));

    if constraint::can_unify(state, product, result).is_apart() {
        return Some(MatchInstance::Apart);
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(product, result)] })
}

pub fn prim_int_compare<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
    given: &[constraint::ConstraintApplication],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[left, right, ordering] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let ordering = state.normalize_type(ordering);

    let left_int = extract_integer(state, left);
    let right_int = extract_integer(state, right);

    if let (Some(left_int), Some(right_int)) = (left_int, right_int) {
        let result = match left_int.cmp(&right_int) {
            Ordering::Less => context.prim_ordering.lt,
            Ordering::Equal => context.prim_ordering.eq,
            Ordering::Greater => context.prim_ordering.gt,
        };

        if constraint::can_unify(state, ordering, result).is_apart() {
            return Some(MatchInstance::Apart);
        }

        return Some(MatchInstance::Match {
            constraints: vec![],
            equalities: vec![(ordering, result)],
        });
    }

    prim_int_compare_transitive(state, context, left, right, ordering, given)
}

/// Uses a graph-based approach to derive ordering relationships transitively.
///
/// We build a directed graph where `a -> b` means `a < b`:
/// - `Compare a b LT`: add edge `a -> b`
/// - `Compare a b EQ`: add edges `a -> b` and `b -> a`
/// - `Compare a b GT`: add edge `b -> a` (since `a > b` means `b < a`)
///
/// Then we compute reachability to determine the ordering:
/// - Path from `left` to `right` only: LT
/// - Path from `right` to `left` only: GT
/// - Paths in both directions: EQ
/// - No path: Unknown/Stuck
fn prim_int_compare_transitive<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right: TypeId,
    ordering: TypeId,
    given: &[constraint::ConstraintApplication],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let prim_int = &context.prim_int;
    let lt = context.prim_ordering.lt;
    let eq = context.prim_ordering.eq;
    let gt = context.prim_ordering.gt;

    let mut graph: DiGraphMap<TypeId, ()> = DiGraphMap::new();

    for constraint in given {
        if constraint.file_id != prim_int.file_id || constraint.item_id != prim_int.compare {
            continue;
        }

        let &[a, b, ordering] = constraint.arguments.as_slice() else { continue };

        let a = state.normalize_type(a);
        let b = state.normalize_type(b);

        let ordering = state.normalize_type(ordering);

        if ordering == lt {
            // a < b: add edge a -> b
            graph.add_edge(a, b, ());
        } else if ordering == eq {
            // a = b: add edges both ways
            graph.add_edge(a, b, ());
            graph.add_edge(b, a, ());
        } else if ordering == gt {
            // a > b means b < a: add edge b -> a
            graph.add_edge(b, a, ());
        }
    }

    // Check reachability in both directions
    let left_reaches_right = has_path_connecting(&graph, left, right, None);
    let right_reaches_left = has_path_connecting(&graph, right, left, None);

    let result = match (left_reaches_right, right_reaches_left) {
        (true, true) => eq,
        (true, false) => lt,
        (false, true) => gt,
        (false, false) => return Some(MatchInstance::Stuck),
    };

    if constraint::can_unify(state, ordering, result).is_apart() {
        return Some(MatchInstance::Apart);
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(ordering, result)] })
}

pub fn prim_int_to_string(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[int, symbol] = arguments else {
        return None;
    };

    let int = state.normalize_type(int);
    let symbol = state.normalize_type(symbol);

    let Some(value) = extract_integer(state, int) else {
        return Some(MatchInstance::Stuck);
    };

    let value: SmolStr = value.to_string().into();
    let result = state.storage.intern(Type::String(StringKind::String, value));

    if constraint::can_unify(state, symbol, result).is_apart() {
        return Some(MatchInstance::Apart);
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(symbol, result)] })
}
