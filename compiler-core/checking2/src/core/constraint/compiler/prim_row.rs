use std::cmp::Ordering;

use building_types::QueryResult;
use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint::MatchInstance;
use crate::core::unification::{CanUnify, can_unify};
use crate::core::{RowField, RowType, Type, TypeId, normalise};
use crate::state::CheckState;

use super::{extract_row, extract_symbol, match_equality};

fn intern_row_value<Q>(context: &CheckContext<Q>, row: RowType) -> TypeId
where
    Q: ExternalQueries,
{
    let row_id = context.intern_row_type(row);
    context.intern_row(row_id)
}

fn extract_closed_row<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<RowType>>
where
    Q: ExternalQueries,
{
    let Some(row) = extract_row(state, context, id)? else { return Ok(None) };
    if row.tail.is_some() {
        return Ok(None);
    }
    Ok(Some(row))
}

type SubtractResult = (Vec<RowField>, Vec<(TypeId, TypeId)>);

fn subtract_row_fields<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    source: &[RowField],
    to_remove: &[RowField],
) -> QueryResult<Option<SubtractResult>>
where
    Q: ExternalQueries,
{
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
                    if let CanUnify::Apart = can_unify(state, context, field.id, remove_field.id)? {
                        return Ok(None);
                    }
                    equalities.push((field.id, remove_field.id));
                    to_remove_iter.next();
                }
                Ordering::Greater => {
                    return Ok(None);
                }
            }
        } else {
            result.push(field.clone());
        }
    }

    if to_remove_iter.next().is_some() {
        return Ok(None);
    }

    Ok(Some((result, equalities)))
}

pub fn match_union<Q>(
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

    let left = normalise::normalise(state, context, left)?;
    let right = normalise::normalise(state, context, right)?;
    let union = normalise::normalise(state, context, union)?;

    let left_row = extract_row(state, context, left)?;
    let right_row = extract_row(state, context, right)?;
    let union_row = extract_row(state, context, union)?;

    match (left_row, right_row, union_row) {
        (Some(left_row), Some(right_row), _) => {
            if let Some(rest) = left_row.tail {
                if left_row.fields.is_empty() {
                    return Ok(Some(MatchInstance::Stuck));
                }

                let fresh_tail = state.fresh_unification(context.queries, context.prim.row_type);

                let result = intern_row_value(
                    context,
                    RowType::new(left_row.fields.iter().cloned(), Some(fresh_tail)),
                );

                let prim_row = &context.prim_row;

                let constraint = context
                    .queries
                    .intern_type(Type::Constructor(prim_row.file_id, prim_row.union));
                let constraint = context.intern_application(constraint, rest);
                let constraint = context.intern_application(constraint, right);
                let constraint = context.intern_application(constraint, fresh_tail);

                return Ok(Some(MatchInstance::Match {
                    constraints: vec![constraint],
                    equalities: vec![(union, result)],
                }));
            }

            let union_fields =
                left_row.fields.iter().chain(right_row.fields.iter()).cloned().collect_vec();

            let result = intern_row_value(context, RowType::new(union_fields, right_row.tail));

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
                subtract_row_fields(state, context, &union_row.fields, &right_row.fields)?
            {
                let result = intern_row_value(context, RowType::new(remaining, union_row.tail));
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
                subtract_row_fields(state, context, &union_row.fields, &left_row.fields)?
            {
                let result = intern_row_value(context, RowType::new(remaining, union_row.tail));
                equalities.push((right, result));
                Ok(Some(MatchInstance::Match { constraints: vec![], equalities }))
            } else {
                Ok(Some(MatchInstance::Apart))
            }
        }
        _ => Ok(Some(MatchInstance::Stuck)),
    }
}

pub fn match_cons<Q>(
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

    let label = normalise::normalise(state, context, label)?;
    let a = normalise::normalise(state, context, a)?;
    let tail = normalise::normalise(state, context, tail)?;
    let row = normalise::normalise(state, context, row)?;

    let label_symbol = extract_symbol(state, context, label)?;
    let tail_row = extract_row(state, context, tail)?;
    let row_row = extract_row(state, context, row)?;

    match (label_symbol, tail_row, row_row) {
        (Some(label_value), Some(tail_row), _) => {
            let mut fields = vec![RowField { label: label_value, id: a }];
            fields.extend(tail_row.fields.iter().cloned());

            let result = intern_row_value(context, RowType::new(fields, tail_row.tail));

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
                let tail_result = intern_row_value(context, RowType::new(remaining, row_row.tail));
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

pub fn match_lacks<Q>(
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

    let label = normalise::normalise(state, context, label)?;
    let row = normalise::normalise(state, context, row)?;

    let Some(label_value) = extract_symbol(state, context, label)? else {
        return Ok(Some(MatchInstance::Stuck));
    };

    let Some(row_row) = extract_row(state, context, row)? else {
        return Ok(Some(MatchInstance::Stuck));
    };

    let has_label = row_row.fields.iter().any(|field| field.label == label_value);

    if has_label {
        Ok(Some(MatchInstance::Apart))
    } else if let Some(tail) = row_row.tail {
        if row_row.fields.is_empty() {
            return Ok(Some(MatchInstance::Stuck));
        }

        let constraint = context
            .queries
            .intern_type(Type::Constructor(context.prim_row.file_id, context.prim_row.lacks));
        let constraint = context.intern_application(constraint, label);
        let constraint = context.intern_application(constraint, tail);

        Ok(Some(MatchInstance::Match { constraints: vec![constraint], equalities: vec![] }))
    } else {
        Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![] }))
    }
}

pub fn match_nub<Q>(
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

    let original = normalise::normalise(state, context, original)?;
    let nubbed = normalise::normalise(state, context, nubbed)?;

    let Some(original_row) = extract_closed_row(state, context, original)? else {
        return Ok(Some(MatchInstance::Stuck));
    };

    let mut seen = FxHashSet::default();
    let mut fields = vec![];

    for field in original_row.fields.iter() {
        if seen.insert(field.label.clone()) {
            fields.push(field.clone());
        }
    }

    let result = intern_row_value(context, RowType::new(fields, None));
    Ok(Some(match_equality(state, context, nubbed, result)?))
}
