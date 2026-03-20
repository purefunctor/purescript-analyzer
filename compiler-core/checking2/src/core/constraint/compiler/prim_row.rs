use std::cmp::Ordering;

use building_types::QueryResult;
use indexing::TypeItemId;
use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint::MatchInstance;
use crate::core::unification::{CanUnify, can_unify};
use crate::core::{RowField, RowType, Type, TypeId, normalise};
use crate::source::types;
use crate::state::CheckState;

use super::{extract_row, extract_symbol, match_equality};

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
                    if let Some(union_row) = union_row {
                        if right_row.tail.is_none() {
                            if let Some((remaining, mut equalities)) =
                                subtract_row_fields(state, context, &union_row.fields, &right_row.fields)?
                            {
                                let result = intern_row_value(context, RowType::new(remaining, union_row.tail));
                                equalities.push((left, result));
                                return Ok(Some(MatchInstance::Match { constraints: vec![], equalities }));
                            }

                            return Ok(Some(MatchInstance::Apart));
                        }
                    }

                    return Ok(Some(MatchInstance::Stuck));
                }

                let fresh_tail = state.fresh_unification(context.queries, context.prim.row_type);

                let result = intern_row_value(
                    context,
                    RowType::new(left_row.fields.iter().cloned(), Some(fresh_tail)),
                );

                let constraint = make_prim_row_constraint(
                    state,
                    context,
                    context.prim_row.union,
                    &[rest, right, fresh_tail],
                )?;

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

        let constraint =
            make_prim_row_constraint(state, context, context.prim_row.lacks, &[label, tail])?;

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
