use std::cmp::Ordering;

use lowering::StringKind;
use smol_str::SmolStr;

use super::MatchInstance;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::{ExternalQueries, Type, TypeId};

fn extract_integer(state: &CheckState, id: TypeId) -> Option<i32> {
    let Type::Integer(n) = state.storage[id] else { return None };
    Some(n)
}

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
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(sum, result)] })
        }
        (Some(left), _, Some(sum)) => {
            let result = state.storage.intern(Type::Integer(sum - left));
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(right, result)] })
        }
        (_, Some(right), Some(sum)) => {
            let result = state.storage.intern(Type::Integer(sum - right));
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

    let left_int = extract_integer(state, left);
    let right_int = extract_integer(state, right);

    match (left_int, right_int) {
        (Some(left), Some(right)) => {
            let result = state.storage.intern(Type::Integer(left * right));
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(product, result)] })
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_int_compare<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
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

    match (left_int, right_int) {
        (Some(left), Some(right)) => {
            let result = match left.cmp(&right) {
                Ordering::Less => context.prim_ordering.lt,
                Ordering::Equal => context.prim_ordering.eq,
                Ordering::Greater => context.prim_ordering.gt,
            };
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(ordering, result)] })
        }
        _ => Some(MatchInstance::Stuck),
    }
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

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(symbol, result)] })
}
