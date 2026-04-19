use std::cmp::Ordering;
use std::iter;

use building_types::QueryResult;
use indexing::TypeItemId;
use rustc_hash::FxHashSet;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint2::matching::{InstanceMatch, MatchInstance};
use crate::core::constraint2::{WorkItem, canonical};
use crate::core::unification::{CanUnify, can_unify};
use crate::core::{RowField, RowType, Type, TypeId, normalise};
use crate::source::types;
use crate::state::CheckState;

use super::{extract_row, extract_symbol, match_equality, stuck_on};

fn intern_row_value<Q>(context: &CheckContext<Q>, row: RowType) -> TypeId
where
    Q: ExternalQueries,
{
    let row_id = context.intern_row_type(row);
    context.intern_row(row_id)
}

fn make_prim_row_constraint<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_id: TypeItemId,
    arguments: &[TypeId],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let row_kind = infer_row_constraint_kind(state, context, arguments)?;

    let constructor =
        context.queries.intern_type(Type::Constructor(context.prim_row.file_id, class_id));
    let mut constraint = context.intern_kind_application(constructor, row_kind);
    for &argument in arguments {
        constraint = context.intern_application(constraint, argument);
    }
    Ok(constraint)
}

fn infer_row_constraint_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    for &argument in arguments {
        let argument_kind = types::elaborate_kind(state, context, argument)?;
        let argument_kind = normalise::expand(state, context, argument_kind)?;

        if let Type::Application(row_constructor, row_kind) = context.lookup_type(argument_kind) {
            let row_constructor = normalise::expand(state, context, row_constructor)?;
            if row_constructor == context.prim.row {
                return Ok(row_kind);
            }
        }
    }

    Ok(state.fresh_unification(context.queries, context.prim.t))
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

fn solve_union_left_empty_prefix<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right_row: &RowType,
    union_row: &Option<RowType>,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    if let Some(union_row) = union_row
        && right_row.tail.is_none()
    {
        if let Some((remaining, mut equalities)) =
            subtract_row_fields(state, context, &union_row.fields, &right_row.fields)?
        {
            let row_type = RowType::new(remaining, union_row.tail);
            let result = intern_row_value(context, row_type);

            equalities.push((left, result));
            let goals: Vec<WorkItem> =
                equalities.into_iter().map(|(t1, t2)| WorkItem::Unify(t1, t2)).collect();
            return Ok(MatchInstance::Match(InstanceMatch { goals }));
        }
        return Ok(MatchInstance::Apart);
    }
    let tails: Vec<TypeId> =
        [right_row.tail, union_row.as_ref().and_then(|r| r.tail)].into_iter().flatten().collect();
    stuck_on(state, context, &tails)
}

fn solve_union_left_open_prefix<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left_row: &RowType,
    rest: TypeId,
    right: TypeId,
    union: TypeId,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let fresh_tail = state.fresh_unification(context.queries, context.prim.row_type);

    let fields = left_row.fields.iter().cloned();
    let row_type = RowType::new(fields, Some(fresh_tail));
    let result = intern_row_value(context, row_type);

    let constraint = make_prim_row_constraint(
        state,
        context,
        context.prim_row.union,
        &[rest, right, fresh_tail],
    )?;

    // Canonicalize the constraint before adding to goals
    let canonical_id = canonical::canonicalise(state, context, constraint)?;
    let goals = if let Some(canonical_id) = canonical_id {
        vec![WorkItem::Constraint(canonical_id), WorkItem::Unify(union, result)]
    } else {
        vec![WorkItem::Unify(union, result)]
    };

    Ok(MatchInstance::Match(InstanceMatch { goals }))
}

fn solve_union_left_closed<Q>(
    context: &CheckContext<Q>,
    left_row: &RowType,
    right_row: &RowType,
    union: TypeId,
) -> MatchInstance
where
    Q: ExternalQueries,
{
    let left = left_row.fields.iter();
    let right = right_row.fields.iter();

    let fields = iter::chain(left, right).cloned();
    let row_type = RowType::new(fields, right_row.tail);
    let result = intern_row_value(context, row_type);

    MatchInstance::Match(InstanceMatch { goals: vec![WorkItem::Unify(union, result)] })
}

fn solve_union_from_right_and_union<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: TypeId,
    right_row: &RowType,
    union_row: &RowType,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    if let Some(tail) = right_row.tail {
        return stuck_on(state, context, &[tail]);
    }
    if let Some((remaining, mut equalities)) =
        subtract_row_fields(state, context, &union_row.fields, &right_row.fields)?
    {
        let row_type = RowType::new(remaining, union_row.tail);
        let result = intern_row_value(context, row_type);

        equalities.push((left, result));
        let goals: Vec<WorkItem> =
            equalities.into_iter().map(|(t1, t2)| WorkItem::Unify(t1, t2)).collect();
        Ok(MatchInstance::Match(InstanceMatch { goals }))
    } else {
        Ok(MatchInstance::Apart)
    }
}

fn solve_union_from_left_and_union<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    right: TypeId,
    left_row: &RowType,
    union_row: &RowType,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    if let Some(tail) = left_row.tail {
        return stuck_on(state, context, &[tail]);
    }
    if let Some((remaining, mut equalities)) =
        subtract_row_fields(state, context, &union_row.fields, &left_row.fields)?
    {
        let row_type = RowType::new(remaining, union_row.tail);
        let result = intern_row_value(context, row_type);

        equalities.push((right, result));
        let goals: Vec<WorkItem> =
            equalities.into_iter().map(|(t1, t2)| WorkItem::Unify(t1, t2)).collect();
        Ok(MatchInstance::Match(InstanceMatch { goals }))
    } else {
        Ok(MatchInstance::Apart)
    }
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

    let left_row = extract_row(state, context, left)?;
    let right_row = extract_row(state, context, right)?;
    let union_row = extract_row(state, context, union)?;

    match (left_row, right_row, union_row) {
        (Some(left_row), Some(right_row), union_row) => {
            if let Some(rest) = left_row.tail {
                if left_row.fields.is_empty() {
                    let solution = solve_union_left_empty_prefix(
                        state, context, left, &right_row, &union_row,
                    )?;
                    return Ok(Some(solution));
                }

                let solution =
                    solve_union_left_open_prefix(state, context, &left_row, rest, right, union)?;
                Ok(Some(solution))
            } else {
                let solution = solve_union_left_closed(context, &left_row, &right_row, union);
                Ok(Some(solution))
            }
        }
        (_, Some(right_row), Some(union_row)) => {
            let solution =
                solve_union_from_right_and_union(state, context, left, &right_row, &union_row)?;
            Ok(Some(solution))
        }
        (Some(left_row), _, Some(union_row)) => {
            let solution =
                solve_union_from_left_and_union(state, context, right, &left_row, &union_row)?;
            Ok(Some(solution))
        }
        _ => Ok(Some(stuck_on(state, context, &[left, right, union])?)),
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

    let label_symbol = extract_symbol(state, context, label)?;
    let tail_row = extract_row(state, context, tail)?;
    let row_row = extract_row(state, context, row)?;

    match (label_symbol, tail_row, row_row) {
        (Some(label_value), Some(tail_row), _) => {
            let mut fields = vec![RowField { label: label_value, id: a }];
            fields.extend(tail_row.fields.iter().cloned());

            let result = intern_row_value(context, RowType::new(fields, tail_row.tail));

            Ok(Some(MatchInstance::Match(InstanceMatch {
                goals: vec![WorkItem::Unify(row, result)],
            })))
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
                Ok(Some(MatchInstance::Match(InstanceMatch {
                    goals: vec![WorkItem::Unify(a, field_type), WorkItem::Unify(tail, tail_result)],
                })))
            } else {
                Ok(Some(MatchInstance::Apart))
            }
        }
        _ => Ok(Some(stuck_on(state, context, &[label, tail, row])?)),
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

    let Some(label_value) = extract_symbol(state, context, label)? else {
        return Ok(Some(stuck_on(state, context, &[label])?));
    };

    let Some(row_row) = extract_row(state, context, row)? else {
        return Ok(Some(stuck_on(state, context, &[row])?));
    };

    let has_label = row_row.fields.iter().any(|field| field.label == label_value);

    if has_label {
        Ok(Some(MatchInstance::Apart))
    } else if let Some(tail) = row_row.tail {
        if row_row.fields.is_empty() {
            return Ok(Some(stuck_on(state, context, &[tail])?));
        }

        let constraint =
            make_prim_row_constraint(state, context, context.prim_row.lacks, &[label, tail])?;

        let canonical_id = canonical::canonicalise(state, context, constraint)?;
        let goals = if let Some(canonical_id) = canonical_id {
            vec![WorkItem::Constraint(canonical_id)]
        } else {
            vec![]
        };

        Ok(Some(MatchInstance::Match(InstanceMatch { goals })))
    } else {
        Ok(Some(MatchInstance::Match(InstanceMatch { goals: vec![] })))
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

    let Some(original_row) = extract_closed_row(state, context, original)? else {
        return Ok(Some(stuck_on(state, context, &[original])?));
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
