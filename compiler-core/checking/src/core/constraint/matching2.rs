use std::iter;

use building_types::QueryResult;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::fd::{Fd, compute_closure, get_all_determined};
use crate::core::{Name, RowField, RowTypeId, Type, TypeId, normalise, toolkit};
use crate::state::CheckState;

#[derive(PartialEq, Eq)]
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

    pub fn is_match(&self) -> bool {
        matches!(self, MatchType::Match { .. })
    }

    pub fn is_apart(&self) -> bool {
        matches!(self, MatchType::Apart)
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, MatchType::Stuck { .. } | MatchType::Skolem)
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

pub fn types_equal<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
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
        (Type::Kinded(left, _), _) => types_equal(state, context, left, right),
        (_, Type::Kinded(right, _)) => types_equal(state, context, left, right),

        (Type::Unification(left), Type::Unification(right)) => {
            if left == right {
                Ok(MatchType::Match { bindings: vec![] })
            } else {
                Ok(MatchType::Stuck { stuck: vec![left, right] })
            }
        }

        (Type::Rigid(left, _, _), Type::Rigid(right, _, _)) => {
            if left == right {
                Ok(MatchType::Match { bindings: vec![] })
            } else {
                Ok(MatchType::Skolem)
            }
        }

        (Type::Unification(left), _) => {
            if toolkit::contains_unification(state, context, right, left)? {
                Ok(MatchType::Apart)
            } else {
                Ok(MatchType::Stuck { stuck: vec![left] })
            }
        }

        (_, Type::Unification(right)) => {
            if toolkit::contains_unification(state, context, left, right)? {
                Ok(MatchType::Apart)
            } else {
                Ok(MatchType::Stuck { stuck: vec![right] })
            }
        }

        (Type::Rigid(left, _, _), _) => {
            if toolkit::contains_rigid(state, context, right, left)? {
                Ok(MatchType::Apart)
            } else {
                Ok(MatchType::Skolem)
            }
        }

        (_, Type::Rigid(right, _, _)) => {
            if toolkit::contains_rigid(state, context, left, right)? {
                Ok(MatchType::Apart)
            } else {
                Ok(MatchType::Skolem)
            }
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
            let function = types_equal(state, context, left_function, right_function)?;
            let argument = types_equal(state, context, left_argument, right_argument)?;
            Ok(function.combine(argument))
        }

        (
            Type::KindApplication(left_function, left_argument),
            Type::KindApplication(right_function, right_argument),
        ) => {
            let function = types_equal(state, context, left_function, right_function)?;
            let argument = types_equal(state, context, left_argument, right_argument)?;
            Ok(function.combine(argument))
        }

        (
            Type::Function(left_argument, left_result),
            Type::Function(right_argument, right_result),
        ) => {
            let argument = types_equal(state, context, left_argument, right_argument)?;
            let result = types_equal(state, context, left_result, right_result)?;
            Ok(argument.combine(result))
        }

        (Type::Row(left), Type::Row(right)) => compare_row_types_with(
            state,
            context,
            left,
            right,
            &mut |state, context, left, right| types_equal(state, context, left, right),
        ),

        (_, _) => Ok(MatchType::Apart),
    }
}

pub fn verify_substitution<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: Vec<(Name, TypeId)>,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
{
    let mut map: FxHashMap<_, Vec<_>> = FxHashMap::default();
    for &(name, substitution) in &bindings {
        map.entry(name).or_default().push(substitution);
    }

    let mut outcome = MatchType::Match { bindings };
    for substitution in map.values() {
        for (&left, &right) in substitution.iter().tuple_combinations() {
            outcome = outcome.combine(types_equal(state, context, left, right)?);
        }
    }

    Ok(outcome)
}

pub fn match_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    patterns: &FxHashSet<Name>,
    functional_dependencies: &[Fd],
    wanted_arguments: &[TypeId],
    given_arguments: &[TypeId],
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
{
    let mut arguments = vec![];

    for (&wanted, &given) in iter::zip(wanted_arguments, given_arguments) {
        arguments.push(types_match(state, context, patterns, wanted, given)?);
    }

    if !covers(functional_dependencies, &arguments)? {
        return Ok(combine_arguments(arguments));
    }

    let determined = get_all_determined(functional_dependencies);
    let arguments = arguments.into_iter().enumerate().filter_map(|(index, argument)| {
        let non_determined = !determined.contains(&index);
        non_determined.then_some(argument)
    });

    let outcome = combine_arguments(arguments);
    if let MatchType::Match { bindings } = outcome {
        return verify_substitution(state, context, bindings);
    }

    Ok(outcome)
}

fn combine_arguments(arguments: impl IntoIterator<Item = MatchType>) -> MatchType {
    let seed = MatchType::Match { bindings: vec![] };
    arguments.into_iter().fold(seed, MatchType::combine)
}

fn covers(fd: &[Fd], types: &[MatchType]) -> QueryResult<bool> {
    let match_indices: FxHashSet<_> = types
        .iter()
        .enumerate()
        .filter_map(|(index, argument)| argument.is_match().then_some(index))
        .collect();

    let determined = compute_closure(fd, &match_indices);
    Ok(types.iter().enumerate().all(|(index, _)| determined.contains(&index)))
}
