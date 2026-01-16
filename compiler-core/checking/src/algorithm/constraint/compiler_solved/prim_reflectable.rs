use crate::algorithm::constraint::{self, MatchInstance};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::{ExternalQueries, Type, TypeId};

use super::{extract_integer, extract_symbol};

pub fn prim_reflectable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[v, t] = arguments else { return None };

    let v = state.normalize_type(v);
    let t = state.normalize_type(t);

    if extract_symbol(state, v).is_some() {
        let expected = context.prim.string;
        return check_reflectable_match(state, t, expected);
    }

    if extract_integer(state, v).is_some() {
        let expected = context.prim.int;
        return check_reflectable_match(state, t, expected);
    }

    if v == context.prim_boolean.true_ || v == context.prim_boolean.false_ {
        let expected = context.prim.boolean;
        return check_reflectable_match(state, t, expected);
    }

    if v == context.prim_ordering.lt
        || v == context.prim_ordering.eq
        || v == context.prim_ordering.gt
    {
        let Some(expected) = context.known_reflectable.ordering else {
            return Some(MatchInstance::Stuck);
        };
        return check_reflectable_match(state, t, expected);
    }

    if matches!(state.storage[v], Type::Unification(_)) {
        return Some(MatchInstance::Stuck);
    }

    Some(MatchInstance::Apart)
}

fn check_reflectable_match(
    state: &mut CheckState,
    actual: TypeId,
    expected: TypeId,
) -> Option<MatchInstance> {
    if constraint::can_unify(state, actual, expected).is_apart() {
        Some(MatchInstance::Apart)
    } else {
        Some(MatchInstance::Match { constraints: vec![], equalities: vec![(actual, expected)] })
    }
}
