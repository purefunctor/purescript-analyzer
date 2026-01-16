use std::cmp::Ordering;

use itertools::EitherOrBoth;
use rustc_hash::FxHashSet;

use crate::algorithm::constraint::{self, MatchInstance};
use crate::algorithm::state::CheckState;
use crate::core::{RowField, RowType};
use crate::{Type, TypeId};

use super::extract_symbol;

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

fn merge_row_fields(
    state: &mut CheckState,
    left: &[RowField],
    right: &[RowField],
) -> Option<Vec<RowField>> {
    let left = left.iter();
    let right = right.iter();

    let merged_by_label =
        itertools::merge_join_by(left, right, |left, right| left.label.cmp(&right.label));

    let mut result = vec![];
    for field in merged_by_label {
        match field {
            EitherOrBoth::Left(left) => result.push(left.clone()),
            EitherOrBoth::Right(right) => result.push(right.clone()),
            EitherOrBoth::Both(left, right) => {
                let left_type = state.normalize_type(left.id);
                let right_type = state.normalize_type(right.id);
                if constraint::can_unify(state, left_type, right_type).is_apart() {
                    return None;
                }
                result.push(left.clone());
            }
        }
    }

    Some(result)
}

type SubtractResult = (Vec<RowField>, Vec<(TypeId, TypeId)>);

fn subtract_row_fields(
    state: &mut CheckState,
    source: &[RowField],
    to_remove: &[RowField],
) -> Option<SubtractResult> {
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
                    let field_ty = state.normalize_type(field.id);
                    let removed_ty = state.normalize_type(remove_field.id);
                    if constraint::can_unify(state, field_ty, removed_ty).is_apart() {
                        return None;
                    }
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
            if let Some(merged) = merge_row_fields(state, &left_row.fields, &right_row.fields) {
                let result = state.storage.intern(Type::Row(RowType::closed(merged)));
                Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(union, result)],
                })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        (_, Some(right_row), Some(union_row)) => {
            if let Some((remaining, mut equalities)) =
                subtract_row_fields(state, &union_row.fields, &right_row.fields)
            {
                let result = state.storage.intern(Type::Row(RowType::closed(remaining)));
                equalities.push((left, result));
                Some(MatchInstance::Match { constraints: vec![], equalities })
            } else {
                Some(MatchInstance::Apart)
            }
        }
        (Some(left_row), _, Some(union_row)) => {
            if let Some((remaining, mut equalities)) =
                subtract_row_fields(state, &union_row.fields, &left_row.fields)
            {
                let result = state.storage.intern(Type::Row(RowType::closed(remaining)));
                equalities.push((right, result));
                Some(MatchInstance::Match { constraints: vec![], equalities })
            } else {
                Some(MatchInstance::Apart)
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
