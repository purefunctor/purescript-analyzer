use std::iter;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use lowering::TypeItemIr;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint::instances::InstanceCandidate;
use crate::core::constraint::{CanonicalConstraintId, canonical};
use crate::core::fd::{Fd, compute_closure, get_all_determined};
use crate::core::substitute::SubstituteName;
use crate::core::walk::{TypeWalker, WalkAction, walk_type};
use crate::core::{KindOrType, Name, RowField, RowTypeId, Type, TypeId, normalise, toolkit};
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

pub enum MatchInstance {
    Match { unifications: Vec<(TypeId, TypeId)>, constraints: Vec<CanonicalConstraintId> },
    Apart,
    Stuck { stuck: Vec<u32> },
    Skolem,
}

impl MatchInstance {
    pub fn empty() -> MatchInstance {
        MatchInstance::Match { unifications: vec![], constraints: vec![] }
    }

    pub fn from_unifications(unifications: Vec<(TypeId, TypeId)>) -> MatchInstance {
        MatchInstance::Match { unifications, constraints: vec![] }
    }

    pub fn from_constraints(constraints: Vec<CanonicalConstraintId>) -> MatchInstance {
        MatchInstance::Match { unifications: vec![], constraints }
    }

    pub fn from_parts(
        unifications: Vec<(TypeId, TypeId)>,
        constraints: Vec<CanonicalConstraintId>,
    ) -> MatchInstance {
        MatchInstance::Match { unifications, constraints }
    }
}

struct CollectBlocking {
    blocking: Vec<u32>,
}

impl TypeWalker for CollectBlocking {
    fn visit<Q: ExternalQueries>(
        &mut self,
        _state: &mut CheckState,
        _context: &CheckContext<Q>,
        _id: TypeId,
        t: &Type,
    ) -> QueryResult<WalkAction> {
        if let Type::Unification(id) = t {
            self.blocking.push(*id);
        }
        Ok(WalkAction::Continue)
    }
}

pub fn collect_blocking<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: &[TypeId],
) -> QueryResult<Vec<u32>>
where
    Q: ExternalQueries,
{
    let mut walker = CollectBlocking { blocking: vec![] };

    for &id in id {
        walk_type(state, context, id, &mut walker)?;
    }

    Ok(walker.blocking)
}

pub fn blocking_constraint<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: &[TypeId],
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let stuck = collect_blocking(state, context, id)?;
    if !stuck.is_empty() { Ok(MatchInstance::Stuck { stuck }) } else { Ok(MatchInstance::Apart) }
}

pub fn match_provided<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: CanonicalConstraintId,
    provided: CanonicalConstraintId,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let wanted = &state.canonicals[wanted];
    let provided = &state.canonicals[provided];

    if (wanted.file_id, wanted.type_id) != (provided.file_id, provided.type_id)
        || wanted.arguments.len() != provided.arguments.len()
    {
        return Ok(MatchInstance::Apart);
    }

    let wanted = wanted.clone();
    let provided = provided.clone();

    let pattern_variables = FxHashSet::default();

    let functional_dependencies =
        get_functional_dependencies(context, wanted.file_id, wanted.type_id)?;

    let wanted_arguments = wanted
        .arguments
        .iter()
        .filter_map(|argument| if let KindOrType::Type(id) = argument { Some(*id) } else { None })
        .collect_vec();

    let provided_arguments = provided
        .arguments
        .iter()
        .filter_map(|argument| if let KindOrType::Type(id) = argument { Some(*id) } else { None })
        .collect_vec();

    match match_instance(
        state,
        context,
        &pattern_variables,
        &functional_dependencies,
        &wanted_arguments,
        &provided_arguments,
    )? {
        MatchType::Match { .. } => {
            let unifications = iter::zip(wanted_arguments, provided_arguments).collect_vec();
            Ok(MatchInstance::Match { unifications, constraints: vec![] })
        }
        MatchType::Apart => Ok(MatchInstance::Apart),
        MatchType::Stuck { stuck } => Ok(MatchInstance::Stuck { stuck }),
        MatchType::Skolem => Ok(MatchInstance::Skolem),
    }
}

pub fn match_declared<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: CanonicalConstraintId,
    candidate: InstanceCandidate,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let Some(declared) = toolkit::instance_info(
        state,
        context,
        candidate.instance.matchable,
        candidate.instance.resolution,
    )?
    else {
        return Ok(MatchInstance::Apart);
    };

    let wanted = state.canonicals[wanted].clone();

    let pattern_variables: FxHashSet<Name> =
        declared.binders.iter().map(|binder| binder.name).collect();

    let functional_dependencies =
        get_functional_dependencies(context, wanted.file_id, wanted.type_id)?;

    let wanted_arguments = wanted
        .arguments
        .iter()
        .filter_map(|argument| if let KindOrType::Type(id) = argument { Some(*id) } else { None })
        .collect_vec();

    let declared_arguments = declared
        .arguments
        .iter()
        .filter_map(|argument| if let KindOrType::Type(id) = argument { Some(*id) } else { None })
        .collect_vec();

    match match_instance(
        state,
        context,
        &pattern_variables,
        &functional_dependencies,
        &wanted_arguments,
        &declared_arguments,
    )? {
        MatchType::Match { bindings } => {
            let mut substitution = FxHashMap::default();
            for &(name, bound) in &bindings {
                substitution.entry(name).or_insert(bound);
            }

            for binder in &declared.binders {
                if substitution.contains_key(&binder.name) {
                    continue;
                }
                let binder_kind = SubstituteName::many(state, context, &substitution, binder.kind)?;
                let binder_type = state.fresh_unification(context.queries, binder_kind);
                substitution.insert(binder.name, binder_type);
            }

            let mut unifications = vec![];
            for (wanted, declared) in iter::zip(wanted_arguments, declared_arguments) {
                let wanted = SubstituteName::many(state, context, &substitution, wanted)?;
                let declared = SubstituteName::many(state, context, &substitution, declared)?;
                unifications.push((wanted, declared));
            }

            let mut constraints = vec![];
            for constraint in declared.constraints {
                let constraint = SubstituteName::many(state, context, &substitution, constraint)?;
                if let Some(constraint) = canonical::canonicalise(state, context, constraint)? {
                    constraints.push(constraint);
                }
            }

            Ok(MatchInstance::Match { unifications, constraints })
        }
        MatchType::Apart => Ok(MatchInstance::Apart),
        MatchType::Stuck { stuck } => Ok(MatchInstance::Stuck { stuck }),
        MatchType::Skolem => Ok(MatchInstance::Skolem),
    }
}

fn get_functional_dependencies<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<Vec<Fd>>
where
    Q: ExternalQueries,
{
    fn extract(type_item: Option<&TypeItemIr>) -> Vec<Fd> {
        let Some(TypeItemIr::ClassGroup { class: Some(class), .. }) = type_item else {
            return vec![];
        };

        let fd = class.functional_dependencies.iter().map(|functional_dependency| {
            Fd::new(
                functional_dependency.determiners.iter().map(|&x| x as usize),
                functional_dependency.determined.iter().map(|&x| x as usize),
            )
        });

        fd.collect_vec()
    }

    if file_id == context.id {
        Ok(extract(context.lowered.info.get_type_item(type_id)))
    } else {
        let lowered = context.queries.lowered(file_id)?;
        Ok(extract(lowered.info.get_type_item(type_id)))
    }
}
