use std::iter;

use building_types::QueryResult;
use indexing::TypeItemId;
use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint::matching::{self, InstanceMatch, MatchInstance};
use crate::core::constraint::{WorkItem, canonical};
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

fn partition_union_output(
    output: &[RowField],
    right: &[RowField],
) -> Option<(Vec<RowField>, Vec<RowField>)> {
    let mut right_labels = FxHashMap::default();
    for field in right {
        let label = SmolStr::clone(&field.label);
        *right_labels.entry(label).or_insert(0usize) += 1;
    }

    let mut left_fields = vec![];
    let mut right_fields = vec![];

    // Union is left-biased, so when solving `Union left right output` from a closed
    // `right` and `output`, the right-hand fields are taken from the end of output.
    // Example: `Union ( | l ) ( x :: R ) ( x :: L, x :: R )` leaves
    // `( x :: L )` for `l`.
    for field in output.iter().rev() {
        if let Some(demand) = right_labels.get_mut(&field.label)
            && *demand > 0
        {
            *demand -= 1;
            let field = RowField::clone(field);
            right_fields.push(field);
        } else {
            let field = RowField::clone(field);
            left_fields.push(field);
        }
    }

    if right_labels.values().any(|&demand| demand > 0) {
        return None;
    }

    left_fields.reverse();
    right_fields.reverse();

    Some((left_fields, right_fields))
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

    match (&left_row, &right_row, &union_row) {
        // Matches when the left row is closed and the right row is known.
        // The known fields from both sides are merged, and the right tail,
        // if any, stays as the tail of the union output.
        //
        // `Union ( a :: A ) ( | r ) ( | u )` solves `u ~ ( a :: A | r )`.
        // `Union ( a :: A ) ( b :: B ) ( | u )` solves `u ~ ( a :: A, b :: B )`.
        // `Union () ( b :: B | r ) ( | u )` solves `u ~ ( b :: B | r )`.
        (Some(left_row @ RowType { tail: None, .. }), Some(right_row), _) => {
            let left = left_row.fields.iter();
            let right = right_row.fields.iter();

            let fields = iter::chain(left, right).cloned();
            let row_type = RowType::new(fields, right_row.tail);
            let result = intern_row_value(context, row_type);

            Ok(Some(MatchInstance::Match(InstanceMatch {
                goals: vec![WorkItem::Unify(union, result)],
            })))
        }

        // Matches when the left row has a tail and both the right row and output
        // row are closed. We can solve the left row by subtracting the right
        // fields from the output.
        //
        // `Union ( | l ) ( b :: B ) ( a :: A, b :: B )` solves `l ~ ( a :: A )`.
        // `Union ( a :: A | l ) ( b :: B ) ( a :: A, b :: B, c :: C )` solves `l ~ ( c :: C )`.
        // `Union ( | l ) ( x :: R ) ( x :: L, x :: R )` solves `l ~ ( x :: L )`.
        (
            Some(RowType { tail: Some(_), .. }),
            Some(right_row @ RowType { tail: None, .. }),
            Some(union_row @ RowType { tail: None, .. }),
        ) => {
            let Some((left_fields, right_fields)) =
                partition_union_output(&union_row.fields, &right_row.fields)
            else {
                return Ok(Some(MatchInstance::Apart));
            };

            let left_result = intern_row_value(context, RowType::new(left_fields, None));
            let right_result = intern_row_value(context, RowType::new(right_fields, None));

            Ok(Some(MatchInstance::Match(InstanceMatch {
                goals: vec![
                    WorkItem::Unify(left, left_result),
                    WorkItem::Unify(right, right_result),
                ],
            })))
        }

        // Matches when the left row has a tail and at least one known field.
        // These known fields are moved into the output and we recurse on the
        // left tail. With no known fields, this would not make progress, so
        // the constraint must stay blocked.
        //
        // `Union ( a :: A | t ) ( | r ) ( | u )` solves `u ~ ( a :: A | f )`,
        // plus the remaining `Union ( | t ) ( | r ) ( | f )` constraint.
        //
        // `Union ( a :: A, b :: B | t ) ( c :: C ) ( | u )` solves `u ~ ( a :: A, b :: B | f )`,
        // plus the remaining `Union ( | t ) ( c :: C ) ( | f )` constraint.
        (Some(left_row @ RowType { tail: Some(t), .. }), Some(_), _)
            if !left_row.fields.is_empty() =>
        {
            let f = state.fresh_unification(context.queries, context.prim.row_type);

            let fields = left_row.fields.iter().cloned();
            let row_type = RowType::new(fields, Some(f));
            let result = intern_row_value(context, row_type);

            let constraint =
                make_prim_row_constraint(state, context, context.prim_row.union, &[*t, right, f])?;

            let constraint = canonical::canonicalise(state, context, constraint)?;
            let goals = if let Some(constraint) = constraint {
                vec![WorkItem::Constraint(constraint), WorkItem::Unify(union, result)]
            } else {
                vec![WorkItem::Unify(union, result)]
            };

            Ok(Some(MatchInstance::Match(InstanceMatch { goals })))
        }

        _ => Ok(Some(matching::blocking_constraint(state, context, &[left, right, union])?)),
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
        _ => Ok(Some(matching::blocking_constraint(state, context, &[label, tail, row])?)),
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
        return Ok(Some(matching::blocking_constraint(state, context, &[label])?));
    };

    let Some(row_row) = extract_row(state, context, row)? else {
        return Ok(Some(matching::blocking_constraint(state, context, &[row])?));
    };

    let has_label = row_row.fields.iter().any(|field| field.label == label_value);

    if has_label {
        Ok(Some(MatchInstance::Apart))
    } else if let Some(tail) = row_row.tail {
        if row_row.fields.is_empty() {
            return Ok(Some(matching::blocking_constraint(state, context, &[tail])?));
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
        return Ok(Some(matching::blocking_constraint(state, context, &[original])?));
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
