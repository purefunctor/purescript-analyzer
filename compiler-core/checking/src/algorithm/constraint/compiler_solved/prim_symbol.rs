use std::cmp::Ordering;

use lowering::StringKind;
use smol_str::SmolStr;

use crate::algorithm::constraint::MatchInstance;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::{ExternalQueries, Type, TypeId};

use super::extract_symbol;

pub fn prim_symbol_append(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[left, right, appended] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let appended = state.normalize_type(appended);

    let left_symbol = extract_symbol(state, left);
    let right_symbol = extract_symbol(state, right);
    let appended_symbol = extract_symbol(state, appended);

    match (left_symbol, right_symbol, appended_symbol) {
        (Some(left), Some(right), _) => {
            let result: SmolStr = format!("{left}{right}").into();
            let result = state.storage.intern(Type::String(StringKind::String, result));
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(appended, result)] })
        }
        (_, Some(right), Some(appended)) => {
            if let Some(left_value) = appended.strip_suffix(right.as_str()) {
                let result: SmolStr = left_value.into();
                let result = state.storage.intern(Type::String(StringKind::String, result));
                Some(MatchInstance::Match { constraints: vec![], equalities: vec![(left, result)] })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        (Some(left), _, Some(appended)) => {
            if let Some(right_value) = appended.strip_prefix(left.as_str()) {
                let result: SmolStr = right_value.into();
                let result = state.storage.intern(Type::String(StringKind::String, result));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(right, result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_symbol_compare<Q>(
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

    let Some(left_symbol) = extract_symbol(state, left) else {
        return Some(MatchInstance::Stuck);
    };

    let Some(right_symbol) = extract_symbol(state, right) else {
        return Some(MatchInstance::Stuck);
    };

    let result = match left_symbol.cmp(&right_symbol) {
        Ordering::Less => context.prim_ordering.lt,
        Ordering::Equal => context.prim_ordering.eq,
        Ordering::Greater => context.prim_ordering.gt,
    };

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(ordering, result)] })
}

pub fn prim_symbol_cons(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[head, tail, symbol] = arguments else {
        return None;
    };

    let head = state.normalize_type(head);
    let tail = state.normalize_type(tail);
    let symbol = state.normalize_type(symbol);

    let head_symbol = extract_symbol(state, head);
    let tail_symbol = extract_symbol(state, tail);
    let symbol_symbol = extract_symbol(state, symbol);

    match (&head_symbol, &tail_symbol, &symbol_symbol) {
        (Some(head), Some(tail), _) => {
            let mut chars = head.chars();
            if let (Some(c), None) = (chars.next(), chars.next()) {
                let result: SmolStr = format!("{c}{tail}").into();
                let result = state.storage.intern(Type::String(StringKind::String, result));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(symbol, result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        (_, _, Some(symbol_value)) => {
            let mut chars = symbol_value.chars();
            if let Some(c) = chars.next() {
                if let Some(head_symbol) = head_symbol {
                    let mut head_chars = head_symbol.chars();
                    if head_chars.next() != Some(c) || head_chars.next().is_some() {
                        return Some(MatchInstance::Apart);
                    }
                }

                let head_result: SmolStr = c.to_string().into();
                let tail_result: SmolStr = chars.as_str().into();
                let head_result =
                    state.storage.intern(Type::String(StringKind::String, head_result));
                let tail_result =
                    state.storage.intern(Type::String(StringKind::String, tail_result));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(head, head_result), (tail, tail_result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_is_symbol(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[symbol] = arguments else { return None };
    let symbol = state.normalize_type(symbol);

    if extract_symbol(state, symbol).is_some() {
        Some(MatchInstance::Match { constraints: vec![], equalities: vec![] })
    } else if matches!(state.storage[symbol], Type::Unification(_)) {
        Some(MatchInstance::Stuck)
    } else {
        Some(MatchInstance::Apart)
    }
}
