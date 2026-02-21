use std::cmp::Ordering;
use std::iter;

use building_types::QueryResult;
use rustc_hash::FxHashSet;

use crate::algorithm::constraint::{self, MatchInstance};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::toolkit;
use crate::core::{RowField, RowType};
use crate::{ExternalQueries, Type, TypeId};

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

pub fn prim_row_union<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[left, right, union] = arguments else {
        return Ok(None);
    };

    let left = toolkit::normalise_expand_type(state, context, left)?;
    let right = toolkit::normalise_expand_type(state, context, right)?;
    let union = toolkit::normalise_expand_type(state, context, union)?;

    let left_row = extract_row(state, left);
    let right_row = extract_row(state, right);
    let union_row = extract_row(state, union);

    match (left_row, right_row, union_row) {
        (Some(left_row), Some(right_row), _) => {
            if let Some(rest) = left_row.tail {
                if left_row.fields.is_empty() {
                    return Ok(Some(MatchInstance::Stuck));
                }

                let fresh_tail = state.fresh_unification_kinded(context.prim.row_type);

                let result = state.storage.intern(Type::Row(RowType::from_unsorted(
                    left_row.fields.to_vec(),
                    Some(fresh_tail),
                )));

                let prim_row = &context.prim_row;

                let constraint =
                    state.storage.intern(Type::Constructor(prim_row.file_id, prim_row.union));

                let constraint = state.storage.intern(Type::Application(constraint, rest));
                let constraint = state.storage.intern(Type::Application(constraint, right));
                let constraint = state.storage.intern(Type::Application(constraint, fresh_tail));

                return Ok(Some(MatchInstance::Match {
                    constraints: vec![constraint],
                    equalities: vec![(union, result)],
                }));
            }

            let union_fields = {
                let left = left_row.fields.iter();
                let right = right_row.fields.iter();
                iter::chain(left, right).cloned().collect()
            };

            let result = state
                .storage
                .intern(Type::Row(RowType::from_unsorted(union_fields, right_row.tail)));

            Ok(Some(MatchInstance::Match {
                constraints: vec![],
                equalities: vec![(union, result)],
            }))
        }
        (_, Some(right_row), Some(union_row)) => {
            if right_row.tail.is_some() {
                return Ok(Some(MatchInstance::Stuck));
            }
            if let Some((remaining, mut equalities)) =
                subtract_row_fields(state, &union_row.fields, &right_row.fields)
            {
                let result = state
                    .storage
                    .intern(Type::Row(RowType::from_unsorted(remaining, union_row.tail)));
                equalities.push((left, result));
                Ok(Some(MatchInstance::Match { constraints: vec![], equalities }))
            } else {
                Ok(Some(MatchInstance::Apart))
            }
        }
        (Some(left_row), _, Some(union_row)) => {
            if left_row.tail.is_some() {
                return Ok(Some(MatchInstance::Stuck));
            }
            if let Some((remaining, mut equalities)) =
                subtract_row_fields(state, &union_row.fields, &left_row.fields)
            {
                let result = state
                    .storage
                    .intern(Type::Row(RowType::from_unsorted(remaining, union_row.tail)));
                equalities.push((right, result));
                Ok(Some(MatchInstance::Match { constraints: vec![], equalities }))
            } else {
                Ok(Some(MatchInstance::Apart))
            }
        }
        _ => Ok(Some(MatchInstance::Stuck)),
    }
}

pub fn prim_row_cons<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[label, a, tail, row] = arguments else {
        return Ok(None);
    };

    let label = toolkit::normalise_expand_type(state, context, label)?;
    let a = toolkit::normalise_expand_type(state, context, a)?;
    let tail = toolkit::normalise_expand_type(state, context, tail)?;
    let row = toolkit::normalise_expand_type(state, context, row)?;

    let label_symbol = extract_symbol(state, label);
    let tail_row = extract_row(state, tail);
    let row_row = extract_row(state, row);

    match (label_symbol, tail_row, row_row) {
        (Some(label_value), Some(tail_row), _) => {
            let mut fields = vec![RowField { label: label_value, id: a }];
            fields.extend(tail_row.fields.iter().cloned());

            let result_row = RowType::from_unsorted(fields, tail_row.tail);
            let result = state.storage.intern(Type::Row(result_row));

            Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![(row, result)] }))
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
                let tail_result = state
                    .storage
                    .intern(Type::Row(RowType::from_unsorted(remaining, row_row.tail)));
                Ok(Some(MatchInstance::Match {
                    constraints: vec![],
                    equalities: vec![(a, field_type), (tail, tail_result)],
                }))
            } else {
                Ok(Some(MatchInstance::Apart))
            }
        }
        _ => Ok(Some(MatchInstance::Stuck)),
    }
}

pub fn prim_row_lacks<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[label, row] = arguments else {
        return Ok(None);
    };

    let label = toolkit::normalise_expand_type(state, context, label)?;
    let row = toolkit::normalise_expand_type(state, context, row)?;

    let Some(label_value) = extract_symbol(state, label) else {
        return Ok(Some(MatchInstance::Stuck));
    };

    let Some(row_row) = extract_row(state, row) else {
        return Ok(Some(MatchInstance::Stuck));
    };

    let has_label = row_row.fields.iter().any(|field| field.label == label_value);

    if has_label {
        Ok(Some(MatchInstance::Apart))
    } else if let Some(tail) = row_row.tail {
        if row_row.fields.is_empty() {
            return Ok(Some(MatchInstance::Stuck));
        }

        let prim_row = &context.prim_row;

        let constraint = state.storage.intern(Type::Constructor(prim_row.file_id, prim_row.lacks));
        let constraint = state.storage.intern(Type::Application(constraint, label));
        let constraint = state.storage.intern(Type::Application(constraint, tail));

        Ok(Some(MatchInstance::Match { constraints: vec![constraint], equalities: vec![] }))
    } else {
        Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![] }))
    }
}

pub fn prim_row_nub<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[original, nubbed] = arguments else {
        return Ok(None);
    };

    let original = toolkit::normalise_expand_type(state, context, original)?;
    let nubbed = toolkit::normalise_expand_type(state, context, nubbed)?;

    let Some(original_row) = extract_closed_row(state, original) else {
        return Ok(Some(MatchInstance::Stuck));
    };

    let mut seen = FxHashSet::default();
    let mut fields = vec![];

    for field in original_row.fields.iter() {
        if seen.insert(field.label.clone()) {
            fields.push(field.clone());
        }
    }

    let result = state.storage.intern(Type::Row(RowType::closed(fields)));
    Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![(nubbed, result)] }))
}
