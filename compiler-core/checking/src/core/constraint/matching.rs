//! Implements matching functions for constraints.

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
use crate::core::fd::{Fd, compute_closure};
use crate::core::substitute::SubstituteName;
use crate::core::unification::{CanUnify, can_unify};
use crate::core::walk::{TypeWalker, WalkAction, walk_type};
use crate::core::{KindOrType, Name, RowField, RowType, Type, TypeId, normalise, toolkit};
use crate::state::CheckState;

/// The result of matching a wanted type against a given type.
#[derive(Clone, Copy)]
pub enum MatchType {
    /// The types match.
    Match,
    /// The match has a wanted-side unification variable.
    Stuck(u32),
    /// The match has a wanted-side skolem variable.
    ///
    /// Skolem variables represent types chosen by the caller and they usually
    /// appear inside of a universal quantifier like `forall`. Since the caller
    /// decides how the skolem variable is instantiated, we cannot prove that
    /// it is [`MatchType::Apart`] from any given type.
    ///
    /// For example, when attempting to match `Solve ~a`, `a` can be instantiated
    /// into `Int`, so perhaps rejecting `Solve Int` is unsound. In practice, we
    /// simply report that no matching instance was found for `Solve ~a`.
    Skolem,
    /// The types do not match.
    Apart,
}

impl MatchType {
    fn and_then(self, f: impl FnOnce() -> QueryResult<MatchType>) -> QueryResult<MatchType> {
        if matches!(self, MatchType::Match) { f() } else { Ok(self) }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, MatchType::Stuck(_) | MatchType::Skolem)
    }
}

pub struct InstanceMatch {
    pub unifications: Vec<(TypeId, TypeId)>,
    pub constraints: Vec<CanonicalConstraintId>,
}

impl InstanceMatch {
    pub fn empty() -> InstanceMatch {
        InstanceMatch { unifications: vec![], constraints: vec![] }
    }

    pub fn from_unifications(unifications: Vec<(TypeId, TypeId)>) -> InstanceMatch {
        InstanceMatch { unifications, constraints: vec![] }
    }

    pub fn from_constraints(constraints: Vec<CanonicalConstraintId>) -> InstanceMatch {
        InstanceMatch { unifications: vec![], constraints }
    }
}

/// The result of matching a wanted constraint against a given constraint.
pub enum MatchInstance {
    /// The instance matches.
    Match(InstanceMatch),
    /// The match depends on unification variables.
    Stuck(Vec<u32>),
    /// The types do not match.
    Apart,
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

pub fn blocking_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
{
    let stuck = collect_blocking(state, context, &[id])?;
    if let Some(&stuck) = stuck.first() {
        Ok(MatchType::Stuck(stuck))
    } else {
        Ok(MatchType::Apart)
    }
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
    if !stuck.is_empty() { Ok(MatchInstance::Stuck(stuck)) } else { Ok(MatchInstance::Apart) }
}

/// Matches a wanted type against a given type.
pub fn match_given_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: TypeId,
    given: TypeId,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
{
    let wanted = normalise::expand(state, context, wanted)?;
    let given = normalise::expand(state, context, given)?;

    if wanted == given {
        return Ok(MatchType::Match);
    }

    let wanted_core = context.lookup_type(wanted);
    let given_core = context.lookup_type(given);

    if let (&Type::Rigid(wanted_name, _, wanted_kind), &Type::Rigid(given_name, _, given_kind)) =
        (&wanted_core, &given_core)
    {
        if wanted_name == given_name {
            match_given_type(state, context, wanted_kind, given_kind)
        } else {
            Ok(MatchType::Apart)
        }
    } else {
        let mut recurse =
            |state: &mut CheckState, context: &CheckContext<Q>, wanted: TypeId, given: TypeId| {
                match_given_type(state, context, wanted, given)
            };
        match_core(state, context, &mut recurse, wanted, given, wanted_core, given_core)
    }
}

/// Matches a wanted constraint against a given constraint.
pub fn match_given_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: CanonicalConstraintId,
    given: &[CanonicalConstraintId],
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let wanted = state.canonicals[wanted].clone();

    'given: for &given in given {
        let given = state.canonicals[given].clone();

        if (wanted.file_id, wanted.type_id) != (given.file_id, given.type_id) {
            continue;
        }

        if wanted.arguments.len() != given.arguments.len() {
            continue;
        }

        let mut results = vec![];
        let mut stuck = vec![];

        for (&wanted_argument, &given_argument) in
            iter::zip(wanted.arguments.iter(), given.arguments.iter())
        {
            let match_result =
                if let (KindOrType::Kind(wanted_argument), KindOrType::Kind(given_argument))
                | (KindOrType::Type(wanted_argument), KindOrType::Type(given_argument)) =
                    (wanted_argument, given_argument)
                {
                    match_given_type(state, context, wanted_argument, given_argument)?
                } else {
                    continue 'given;
                };

            if matches!(match_result, MatchType::Apart) {
                continue 'given;
            }

            if let MatchType::Stuck(id) = match_result {
                stuck.push(id);
            }

            if let (KindOrType::Type(wanted_argument), KindOrType::Type(given_argument)) =
                (wanted_argument, given_argument)
            {
                results.push((wanted_argument, given_argument, match_result));
            }
        }

        let match_results = results.iter().map(|(_, _, result)| *result).collect_vec();

        if !can_determine_stuck(context, wanted.file_id, wanted.type_id, &match_results)? {
            return Ok(MatchInstance::Stuck(stuck));
        }

        let mut unifications = vec![];

        for (wanted, given, result) in results {
            if matches!(result, MatchType::Stuck(_)) {
                unifications.push((wanted, given));
            }
        }

        return Ok(MatchInstance::Match(InstanceMatch::from_unifications(unifications)));
    }

    Ok(MatchInstance::Apart)
}

pub type InstanceBindings = FxHashMap<Name, TypeId>;

/// Matches a wanted type against an instance type.
///
/// This function also collects [`InstanceBindings`] and unifications
/// during matching. The bindings will be used to substitute variables
/// appearing in the subgoals of the instance declaration.
pub fn match_instance_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    patterns: &FxHashSet<Name>,
    bindings: &mut InstanceBindings,
    unifications: &mut Vec<(TypeId, TypeId)>,
    wanted: TypeId,
    given: TypeId,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
{
    let wanted = normalise::expand(state, context, wanted)?;
    let given = normalise::expand(state, context, given)?;

    if wanted == given {
        return Ok(MatchType::Match);
    }

    let wanted_core = context.lookup_type(wanted);
    let given_core = context.lookup_type(given);

    if let Type::Rigid(name, _, _) = given_core
        && patterns.contains(&name)
    {
        return if let Some(&bound) = bindings.get(&name) {
            match can_unify(state, context, wanted, bound)? {
                CanUnify::Equal => Ok(MatchType::Match),
                CanUnify::Apart => Ok(MatchType::Apart),
                CanUnify::Unify => {
                    unifications.push((wanted, bound));
                    Ok(MatchType::Match)
                }
            }
        } else {
            bindings.insert(name, wanted);
            Ok(MatchType::Match)
        };
    }

    // See documentation for [`MatchType::Skolem`].
    if matches!(wanted_core, Type::Rigid(_, _, _)) {
        return Ok(MatchType::Skolem);
    }

    let mut recurse =
        |state: &mut CheckState, context: &CheckContext<Q>, wanted: TypeId, given: TypeId| {
            match_instance_type(state, context, patterns, bindings, unifications, wanted, given)
        };

    match_core(state, context, &mut recurse, wanted, given, wanted_core, given_core)
}

/// Matches a wanted constraint against an instance chain.
pub fn match_instance_chain<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: CanonicalConstraintId,
    chain: &[InstanceCandidate],
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let wanted = state.canonicals[wanted].clone();

    'chain: for candidate in chain {
        let Some(given) = toolkit::instance_info(
            state,
            context,
            candidate.instance.matchable,
            candidate.instance.resolution,
        )?
        else {
            continue 'chain;
        };

        let pattern_variables: FxHashSet<Name> =
            given.binders.iter().map(|binder| binder.name).collect();

        let mut results = vec![];
        let mut stuck = vec![];

        let mut bindings = FxHashMap::default();
        let mut unifications = vec![];

        for (&wanted_argument, &given_argument) in
            iter::zip(wanted.arguments.iter(), given.arguments.iter())
        {
            let match_result =
                if let (KindOrType::Kind(wanted_argument), KindOrType::Kind(given_argument))
                | (KindOrType::Type(wanted_argument), KindOrType::Type(given_argument)) =
                    (wanted_argument, given_argument)
                {
                    match_instance_type(
                        state,
                        context,
                        &pattern_variables,
                        &mut bindings,
                        &mut unifications,
                        wanted_argument,
                        given_argument,
                    )?
                } else {
                    continue 'chain;
                };

            if matches!(match_result, MatchType::Apart) {
                continue 'chain;
            }

            if let MatchType::Stuck(id) = match_result {
                stuck.push(id);
            }

            if let (KindOrType::Type(wanted_argument), KindOrType::Type(given_argument)) =
                (wanted_argument, given_argument)
            {
                results.push((wanted_argument, given_argument, match_result));
            }
        }

        let match_results = results.iter().map(|(_, _, result)| *result).collect_vec();

        if !can_determine_stuck(context, wanted.file_id, wanted.type_id, &match_results)? {
            return Ok(MatchInstance::Stuck(stuck));
        }

        for binder in given.binders {
            if bindings.contains_key(&binder.name) {
                continue;
            }

            let binder_kind = SubstituteName::many(state, context, &bindings, binder.kind)?;
            let binder_type = state.fresh_unification(context.queries, binder_kind);
            bindings.insert(binder.name, binder_type);
        }

        for (wanted, given, result) in results {
            if matches!(result, MatchType::Stuck(_)) {
                let given = SubstituteName::many(state, context, &bindings, given)?;
                unifications.push((wanted, given));
            }
        }

        let mut constraints = vec![];
        for constraint in given.constraints {
            let constraint = SubstituteName::many(state, context, &bindings, constraint)?;
            if let Some(constraint) = canonical::canonicalise(state, context, constraint)? {
                constraints.push(constraint);
            };
        }

        return Ok(MatchInstance::Match(InstanceMatch { unifications, constraints }));
    }

    Ok(MatchInstance::Apart)
}

fn match_core<Q, R>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    recurse: &mut R,
    wanted: TypeId,
    given: TypeId,
    wanted_core: Type,
    given_core: Type,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
    R: for<'s, 'c> FnMut(
        &'s mut CheckState,
        &'c CheckContext<Q>,
        TypeId,
        TypeId,
    ) -> QueryResult<MatchType>,
{
    match (wanted_core, given_core) {
        (Type::Unification(id), _) => Ok(MatchType::Stuck(id)),

        (Type::Row(wanted_row_id), Type::Row(given_row_id)) => {
            let wanted_row = context.lookup_row_type(wanted_row_id);
            let given_row = context.lookup_row_type(given_row_id);
            match_row_type(state, context, recurse, wanted_row, given_row)
        }

        (
            Type::Application(wanted_function, wanted_argument),
            Type::Application(given_function, given_argument),
        ) => recurse(state, context, wanted_function, given_function)?
            .and_then(|| recurse(state, context, wanted_argument, given_argument)),

        (
            Type::Function(wanted_argument, wanted_result),
            Type::Function(given_argument, given_result),
        ) => recurse(state, context, wanted_argument, given_argument)?
            .and_then(|| recurse(state, context, wanted_result, given_result)),

        (Type::Function(wanted_argument, wanted_result), Type::Application(given_function, _)) => {
            if is_nested_application(state, context, given_function)? {
                let wanted = context.intern_function_application(wanted_argument, wanted_result);
                recurse(state, context, wanted, given)
            } else {
                Ok(MatchType::Apart)
            }
        }

        (Type::Application(wanted_function, _), Type::Function(given_argument, given_result)) => {
            if is_nested_application(state, context, wanted_function)? {
                let given = context.intern_function_application(given_argument, given_result);
                recurse(state, context, wanted, given)
            } else {
                Ok(MatchType::Apart)
            }
        }

        (
            Type::KindApplication(wanted_function, wanted_argument),
            Type::KindApplication(given_function, given_argument),
        ) => recurse(state, context, wanted_function, given_function)?
            .and_then(|| recurse(state, context, wanted_argument, given_argument)),

        (Type::Kinded(wanted_inner, wanted_kind), Type::Kinded(given_inner, given_kind)) => {
            recurse(state, context, wanted_inner, given_inner)?
                .and_then(|| recurse(state, context, wanted_kind, given_kind))
        }

        _ => Ok(MatchType::Apart),
    }
}

fn match_row_type<Q, R>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    recurse: &mut R,
    wanted_row: RowType,
    given_row: RowType,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
    R: for<'s, 'c> FnMut(
        &'s mut CheckState,
        &'c CheckContext<Q>,
        TypeId,
        TypeId,
    ) -> QueryResult<MatchType>,
{
    let mut wanted_only = vec![];
    let mut given_only = vec![];
    let mut result = MatchType::Match;

    for field in itertools::merge_join_by(
        wanted_row.fields.iter(),
        given_row.fields.iter(),
        |wanted, given| wanted.label.cmp(&given.label),
    ) {
        match field {
            itertools::EitherOrBoth::Both(wanted, given) => {
                result = result.and_then(|| recurse(state, context, wanted.id, given.id))?;
                // Given an open wanted row, additional fields from the
                // given row can be absorbed into the wanted row's tail.
                if matches!(result, MatchType::Apart) && wanted_row.tail.is_none() {
                    return Ok(MatchType::Apart);
                }
            }
            itertools::EitherOrBoth::Left(wanted) => wanted_only.push(wanted),
            itertools::EitherOrBoth::Right(given) => given_only.push(given),
        }
    }

    enum RowRest {
        /// `( a :: Int )` and `( a :: Int | r )`
        Additional,
        /// `( | r )`
        Open(TypeId),
        /// `( )`
        Closed,
    }

    impl RowRest {
        fn new(only: &[&RowField], tail: Option<TypeId>) -> RowRest {
            if !only.is_empty() {
                RowRest::Additional
            } else if let Some(tail) = tail {
                RowRest::Open(tail)
            } else {
                RowRest::Closed
            }
        }
    }

    use RowRest::*;

    let given_rest = RowRest::new(&given_only, given_row.tail);
    let wanted_rest = RowRest::new(&wanted_only, wanted_row.tail);

    match given_rest {
        // If there are additional given fields
        Additional => match wanted_rest {
            // we cannot match it against a tail-less wanted,
            // nor against the additional wanted fields.
            Closed | Additional => Ok(MatchType::Apart),
            // we could potentially make progress by having the
            // wanted tail absorb the additional given fields
            Open(wanted_tail) => result.and_then(|| blocking_type(state, context, wanted_tail)),
        },
        // If the given row has a tail, match it against the
        // additional fields and tail from the wanted row
        Open(given_tail) => {
            let fields = wanted_only.into_iter().cloned().collect_vec();
            let row = context.intern_row(fields, wanted_row.tail);
            result.and_then(|| recurse(state, context, row, given_tail))
        }
        // If we have a closed given row
        Closed => match wanted_rest {
            // we cannot match it against fields in the wanted row
            Additional => Ok(MatchType::Apart),
            // we could make progress with an open wanted row
            Open(wanted_tail) => result.and_then(|| blocking_type(state, context, wanted_tail)),
            // we can match it directly with a closed wanted row
            Closed => Ok(result),
        },
    }
}

fn is_nested_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let t = normalise::expand(state, context, t)?;
    Ok(matches!(context.lookup_type(t), Type::Application(_, _)))
}

fn can_determine_stuck<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
    results: &[MatchType],
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let stuck_indices = results
        .iter()
        .enumerate()
        .filter_map(|(index, result)| result.is_unknown().then_some(index));

    let stuck_indices = stuck_indices.collect_vec();

    if stuck_indices.is_empty() {
        return Ok(true);
    }

    let functional_dependencies = get_functional_dependencies(context, file_id, type_id)?;
    let match_indices: FxHashSet<_> = results
        .iter()
        .enumerate()
        .filter_map(|(index, result)| matches!(result, MatchType::Match).then_some(index))
        .collect();

    let determined = compute_closure(&functional_dependencies, &match_indices);
    Ok(stuck_indices.iter().all(|index| determined.contains(index)))
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
