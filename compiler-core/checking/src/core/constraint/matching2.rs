use std::iter;

use building_types::QueryResult;
use rustc_hash::FxHashSet;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{Name, RowField, RowTypeId, Type, TypeId, normalise};
use crate::state::CheckState;

pub enum MatchType {
    Match { bindings: Vec<(Name, TypeId)> },
    Apart,
    Stuck { stuck: Vec<u32> },
    Skolem,
}

impl MatchType {
    pub fn combine(self, other: MatchType) -> MatchType {
        match (self, other) {
            (MatchType::Match { bindings: left }, MatchType::Match { bindings: right }) => {
                MatchType::Match { bindings: iter::chain(left, right).collect() }
            }

            (MatchType::Apart, _) | (_, MatchType::Apart) => MatchType::Apart,

            (MatchType::Stuck { stuck: left }, MatchType::Stuck { stuck: right }) => {
                MatchType::Stuck { stuck: iter::chain(left, right).collect() }
            }

            (MatchType::Stuck { stuck }, _) | (_, MatchType::Stuck { stuck }) => {
                MatchType::Stuck { stuck }
            }

            (MatchType::Skolem, _) | (_, MatchType::Skolem) => MatchType::Skolem,
        }
    }
}

pub fn types_match<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    pattern: &FxHashSet<Name>,
    left: TypeId,
    right: TypeId,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
{
    let left = normalise::expand(state, context, left)?;
    let right = normalise::expand(state, context, right)?;

    let left_core = context.lookup_type(left);
    let right_core = context.lookup_type(right);

    match (left_core, right_core) {
        (Type::Kinded(left, _), _) => types_match(state, context, pattern, left, right),
        (_, Type::Kinded(right, _)) => types_match(state, context, pattern, left, right),

        (Type::Unification(left), Type::Unification(right)) => {
            if left == right {
                Ok(MatchType::Match { bindings: vec![] })
            } else {
                Ok(MatchType::Stuck { stuck: vec![left, right] })
            }
        }

        (Type::Rigid(left, _, _), Type::Rigid(right, _, _))
            if !pattern.contains(&left) && !pattern.contains(&right) && left == right =>
        {
            Ok(MatchType::Match { bindings: vec![] })
        }

        (_, Type::Rigid(right, _, _)) if pattern.contains(&right) => {
            Ok(MatchType::Match { bindings: vec![(right, left)] })
        }

        (Type::Unification(unification), _) | (_, Type::Unification(unification)) => {
            Ok(MatchType::Stuck { stuck: vec![unification] })
        }

        (Type::Rigid(name, _, _), _) | (_, Type::Rigid(name, _, _)) if !pattern.contains(&name) => {
            Ok(MatchType::Skolem)
        }

        (Type::Constructor(left_file, left_item), Type::Constructor(right_file, right_item))
            if (left_file, left_item) == (right_file, right_item) =>
        {
            Ok(MatchType::Match { bindings: vec![] })
        }

        (Type::String(_, left), Type::String(_, right)) if left == right => {
            Ok(MatchType::Match { bindings: vec![] })
        }

        (Type::Integer(left), Type::Integer(right)) if left == right => {
            Ok(MatchType::Match { bindings: vec![] })
        }

        (
            Type::Application(left_function, left_argument),
            Type::Application(right_function, right_argument),
        ) => {
            let function = types_match(state, context, pattern, left_function, right_function)?;
            let argument = types_match(state, context, pattern, left_argument, right_argument)?;
            Ok(function.combine(argument))
        }

        (
            Type::KindApplication(left_function, left_argument),
            Type::KindApplication(right_function, right_argument),
        ) => {
            let function = types_match(state, context, pattern, left_function, right_function)?;
            let argument = types_match(state, context, pattern, left_argument, right_argument)?;
            Ok(function.combine(argument))
        }

        (
            Type::Function(left_argument, left_result),
            Type::Function(right_argument, right_result),
        ) => {
            let argument = types_match(state, context, pattern, left_argument, right_argument)?;
            let result = types_match(state, context, pattern, left_result, right_result)?;
            Ok(argument.combine(result))
        }

        (Type::Row(left), Type::Row(right)) => compare_row_types_with(
            state,
            context,
            left,
            right,
            &mut |state, context, left, right| types_match(state, context, pattern, left, right),
        ),

        (_, _) => Ok(MatchType::Apart),
    }
}

fn compare_row_types_with<Q, Compare>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left: RowTypeId,
    right: RowTypeId,
    compare: &mut Compare,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
    Compare: FnMut(&mut CheckState, &CheckContext<Q>, TypeId, TypeId) -> QueryResult<MatchType>,
{
    let left = context.lookup_row_type(left);
    let right = context.lookup_row_type(right);

    let mut row_result = MatchType::Match { bindings: vec![] };
    let mut left_fields = vec![];
    let mut right_fields = vec![];

    for field in itertools::merge_join_by(left.fields.iter(), right.fields.iter(), |left, right| {
        left.label.cmp(&right.label)
    }) {
        match field {
            itertools::EitherOrBoth::Left(left) => {
                left_fields.push(left.clone());
            }
            itertools::EitherOrBoth::Both(left, right) => {
                let field_result = compare(state, context, left.id, right.id)?;
                row_result = row_result.combine(field_result);
            }
            itertools::EitherOrBoth::Right(right) => {
                right_fields.push(right.clone());
            }
        }
    }

    let tail_result = compare_row_tails_with(
        state,
        context,
        left_fields,
        left.tail,
        right_fields,
        right.tail,
        compare,
    )?;

    Ok(row_result.combine(tail_result))
}

fn compare_row_tails_with<Q, Compare>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    left_fields: Vec<RowField>,
    left_tail: Option<TypeId>,
    right_fields: Vec<RowField>,
    right_tail: Option<TypeId>,
    compare: &mut Compare,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
    Compare: FnMut(&mut CheckState, &CheckContext<Q>, TypeId, TypeId) -> QueryResult<MatchType>,
{
    let left_tail = context.intern_row(left_fields, left_tail);
    let right_tail = context.intern_row(right_fields, right_tail);

    if let (Type::Row(left_row), Type::Row(right_row)) =
        (context.lookup_type(left_tail), context.lookup_type(right_tail))
    {
        let left_row = context.lookup_row_type(left_row);
        let right_row = context.lookup_row_type(right_row);

        if left_row.fields.is_empty()
            && left_row.tail.is_none()
            && right_row.fields.is_empty()
            && right_row.tail.is_none()
        {
            Ok(MatchType::Match { bindings: vec![] })
        } else {
            Ok(MatchType::Apart)
        }
    } else {
        compare(state, context, left_tail, right_tail)
    }
}
