use std::cmp::Ordering;

use itertools::EitherOrBoth;
use lowering::StringKind;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;

use super::MatchInstance;
use crate::algorithm::kind;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::{RowField, RowType};
use crate::{ExternalQueries, Type, TypeId};

fn extract_integer(state: &CheckState, id: TypeId) -> Option<i32> {
    let Type::Integer(n) = state.storage[id] else { return None };
    Some(n)
}

fn extract_symbol(state: &CheckState, id: TypeId) -> Option<SmolStr> {
    let Type::String(_, s) = &state.storage[id] else { return None };
    Some(s.clone())
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

    let Some(left_int) = extract_integer(state, left) else {
        return Some(MatchInstance::Stuck);
    };

    let Some(right_int) = extract_integer(state, right) else {
        return Some(MatchInstance::Stuck);
    };

    let result = state.storage.intern(Type::Integer(left_int * right_int));
    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(product, result)] })
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

    let Some(left_int) = extract_integer(state, left) else {
        return Some(MatchInstance::Stuck);
    };

    let Some(right_int) = extract_integer(state, right) else {
        return Some(MatchInstance::Stuck);
    };

    let result = match left_int.cmp(&right_int) {
        Ordering::Less => context.prim_ordering.lt,
        Ordering::Equal => context.prim_ordering.eq,
        Ordering::Greater => context.prim_ordering.gt,
    };

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

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(symbol, result)] })
}

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

    match (head_symbol, tail_symbol, symbol_symbol) {
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

fn extract_closed_row(state: &CheckState, id: TypeId) -> Option<RowType> {
    let Type::Row(row) = &state.storage[id] else { return None };
    if row.tail.is_some() {
        return None;
    }
    Some(row.clone())
}

fn extract_row(state: &CheckState, id: TypeId) -> Option<RowType> {
    let Type::Row(row) = &state.storage[id] else { return None };
    Some(row.clone())
}

fn merge_row_fields(left: &[RowField], right: &[RowField]) -> Vec<RowField> {
    let left = left.iter();
    let right = right.iter();

    let iter = itertools::merge_join_by(left, right, |left, right| left.label.cmp(&right.label));
    let iter = iter.flat_map(|either| {
        let (left, right) = match either {
            EitherOrBoth::Left(left) => (Some(left), None),
            EitherOrBoth::Right(right) => (Some(right), None),
            EitherOrBoth::Both(left, right) => (Some(left), Some(right)),
        };
        left.into_iter().chain(right).cloned()
    });

    iter.collect()
}

type SubtractResult = (Vec<RowField>, Vec<(TypeId, TypeId)>);

fn subtract_row_fields(source: &[RowField], to_remove: &[RowField]) -> Option<SubtractResult> {
    let mut result = vec![];
    let mut equalities = vec![];
    let mut to_remove_iter = to_remove.iter().peekable();

    for field in source {
        if let Some(remove_field) = to_remove_iter.peek() {
            match field.label.cmp(&remove_field.label) {
                Ordering::Less => {
                    result.push(field.clone());
                }
                Ordering::Equal => {
                    equalities.push((field.id, remove_field.id));
                    to_remove_iter.next();
                }
                Ordering::Greater => {
                    return None;
                }
            }
        } else {
            result.push(field.clone());
        }
    }

    if to_remove_iter.next().is_some() {
        return None;
    }

    Some((result, equalities))
}

pub fn prim_row_union(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[left, right, union] = arguments else {
        return None;
    };

    let left = state.normalize_type(left);
    let right = state.normalize_type(right);
    let union = state.normalize_type(union);

    let left_row = extract_closed_row(state, left);
    let right_row = extract_closed_row(state, right);
    let union_row = extract_closed_row(state, union);

    match (left_row, right_row, union_row) {
        (Some(left_row), Some(right_row), _) => {
            let merged = merge_row_fields(&left_row.fields, &right_row.fields);
            let result = state.storage.intern(Type::Row(RowType::closed(merged)));
            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(union, result)] })
        }
        (_, Some(right_row), Some(union_row)) => {
            match subtract_row_fields(&union_row.fields, &right_row.fields) {
                Some((remaining, equalities)) => {
                    let result = state.storage.intern(Type::Row(RowType::closed(remaining)));
                    let mut all_equalities = vec![(left, result)];
                    all_equalities.extend(equalities);
                    Some(MatchInstance::Match { constraints: vec![], equalities: all_equalities })
                }
                None => Some(MatchInstance::Apart),
            }
        }
        (Some(left_row), _, Some(union_row)) => {
            match subtract_row_fields(&union_row.fields, &left_row.fields) {
                Some((remaining, equalities)) => {
                    let result = state.storage.intern(Type::Row(RowType::closed(remaining)));
                    let mut all_equalities = vec![(right, result)];
                    all_equalities.extend(equalities);
                    Some(MatchInstance::Match { constraints: vec![], equalities: all_equalities })
                }
                None => Some(MatchInstance::Apart),
            }
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_row_cons(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[label, a, tail, row] = arguments else {
        return None;
    };

    let label = state.normalize_type(label);
    let a = state.normalize_type(a);
    let tail = state.normalize_type(tail);
    let row = state.normalize_type(row);

    let label_symbol = extract_symbol(state, label);
    let tail_row = extract_closed_row(state, tail);
    let row_row = extract_closed_row(state, row);

    match (label_symbol, tail_row, row_row) {
        (Some(label_value), Some(tail_row), _) => {
            let mut fields = vec![RowField { label: label_value, id: a }];
            fields.extend(tail_row.fields.iter().cloned());

            let result_row = RowType::from_unsorted(fields, None);
            let result = state.storage.intern(Type::Row(result_row));

            Some(MatchInstance::Match { constraints: vec![], equalities: vec![(row, result)] })
        }

        (Some(label_value), _, Some(row_row)) => {
            let mut remaining = vec![];
            let mut found_type = None;

            for field in row_row.fields.iter() {
                if field.label == label_value && found_type.is_none() {
                    found_type = Some(field.id);
                } else {
                    remaining.push(field.clone());
                }
            }

            if let Some(field_type) = found_type {
                let tail_result = state.storage.intern(Type::Row(RowType::closed(remaining)));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(a, field_type), (tail, tail_result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        _ => Some(MatchInstance::Stuck),
    }
}

pub fn prim_row_lacks(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[label, row] = arguments else {
        return None;
    };

    let label = state.normalize_type(label);
    let row = state.normalize_type(row);

    let Some(label_value) = extract_symbol(state, label) else {
        return Some(MatchInstance::Stuck);
    };

    let Some(row_row) = extract_row(state, row) else {
        return Some(MatchInstance::Stuck);
    };

    let has_label = row_row.fields.iter().any(|field| field.label == label_value);

    if has_label {
        Some(MatchInstance::Apart)
    } else if row_row.tail.is_some() {
        Some(MatchInstance::Stuck)
    } else {
        Some(MatchInstance::Match { constraints: vec![], equalities: vec![] })
    }
}

pub fn prim_row_nub(state: &mut CheckState, arguments: &[TypeId]) -> Option<MatchInstance> {
    let &[original, nubbed] = arguments else {
        return None;
    };

    let original = state.normalize_type(original);
    let nubbed = state.normalize_type(nubbed);

    let Some(original_row) = extract_closed_row(state, original) else {
        return Some(MatchInstance::Stuck);
    };

    let mut seen = FxHashSet::default();
    let mut fields = vec![];

    for field in original_row.fields.iter() {
        if seen.insert(field.label.clone()) {
            fields.push(field.clone());
        }
    }

    let result = state.storage.intern(Type::Row(RowType::closed(fields)));
    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(nubbed, result)] })
}

fn extract_row_element_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);

    if let Type::Row(ref row_type) = state.storage[type_id]
        && let Some(field) = row_type.fields.first()
        && let Ok(kind) = kind::elaborate_kind(state, context, field.id)
    {
        return kind;
    }

    state.fresh_unification_type(context)
}

pub fn prim_rowlist_row_to_list<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[row, list] = arguments else {
        return None;
    };

    let row = state.normalize_type(row);
    let list = state.normalize_type(list);

    let Some(row_row) = extract_closed_row(state, row) else {
        return Some(MatchInstance::Stuck);
    };

    let element_kind = extract_row_element_kind(state, context, row);

    let mut result =
        state.storage.intern(Type::KindApplication(context.prim_row_list.nil, element_kind));

    let cons_kinded =
        state.storage.intern(Type::KindApplication(context.prim_row_list.cons, element_kind));

    for field in row_row.fields.iter().rev() {
        let label_type =
            state.storage.intern(Type::String(StringKind::String, field.label.clone()));

        let cons_label = state.storage.intern(Type::Application(cons_kinded, label_type));
        let cons_type = state.storage.intern(Type::Application(cons_label, field.id));

        result = state.storage.intern(Type::Application(cons_type, result));
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(list, result)] })
}
