use std::iter;

use building_types::QueryResult;
use files::FileId;
use indexing::{InstanceChainId, TypeItemId};
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::unification::{CanUnify, can_unify};
use crate::core::walk::{TypeWalker, WalkAction, walk_type};
use crate::core::{
    CheckedInstance, KindOrType, Name, RowField, RowType, Type, TypeId, normalise, toolkit,
};
use crate::error::ErrorKind;
use crate::state::CheckState;
use crate::{CheckedModule, ExternalQueries};

use super::fd::{Fd, compute_closure, get_all_determined};
use super::{ConstraintApplication, MatchInstance, MatchType};

#[derive(Clone)]
pub struct CandidateInstance {
    chain_id: Option<InstanceChainId>,
    position: u32,
    checked: CheckedInstance,
}

/// Collects instance chains for a constraint from all eligible files.
pub fn collect_instance_chains<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    application: &ConstraintApplication,
) -> QueryResult<Vec<Vec<CandidateInstance>>>
where
    Q: ExternalQueries,
{
    let mut files_to_search = FxHashSet::default();
    files_to_search.insert(application.file_id);

    for &argument in &application.arguments {
        let argument = match argument {
            KindOrType::Kind(argument) | KindOrType::Type(argument) => argument,
        };
        CollectFileReferences::collect(state, context, argument, &mut files_to_search)?;
    }

    let mut instances = vec![];

    for &file_id in &files_to_search {
        if file_id == context.id {
            collect_instances_from_checked(
                &mut instances,
                &state.checked,
                &context.indexed,
                application.file_id,
                application.item_id,
            );
        } else {
            let checked = context.queries.checked(file_id)?;
            let indexed = context.queries.indexed(file_id)?;
            collect_instances_from_checked(
                &mut instances,
                &checked,
                &indexed,
                application.file_id,
                application.item_id,
            );
        }
    }

    let mut grouped: FxHashMap<InstanceChainId, Vec<CandidateInstance>> = FxHashMap::default();
    let mut singleton = vec![];

    for instance in instances {
        if let Some(chain_id) = instance.chain_id {
            grouped.entry(chain_id).or_default().push(instance);
        } else {
            singleton.push(vec![instance]);
        }
    }

    let mut chains = singleton;
    for (_, mut chain) in grouped {
        chain.sort_by_key(|instance| instance.position);
        chains.push(chain);
    }

    Ok(chains)
}

fn collect_instances_from_checked(
    output: &mut Vec<CandidateInstance>,
    checked: &CheckedModule,
    indexed: &indexing::IndexedModule,
    class_file: FileId,
    class_id: TypeItemId,
) {
    output.extend(
        checked
            .instances
            .iter()
            .filter(|(_, instance)| instance.resolution == (class_file, class_id))
            .map(|(&id, checked)| CandidateInstance {
                chain_id: indexed.pairs.instance_chain_id(id),
                position: indexed.pairs.instance_chain_position(id).unwrap_or(0),
                checked: checked.clone(),
            }),
    );

    output.extend(
        checked
            .derived
            .values()
            .filter(|instance| instance.resolution == (class_file, class_id))
            .cloned()
            .map(|checked| CandidateInstance { chain_id: None, position: 0, checked }),
    );
}

fn get_functional_dependencies<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<Vec<Fd>>
where
    Q: ExternalQueries,
{
    fn extract(type_item: Option<&lowering::TypeItemIr>) -> Vec<Fd> {
        let Some(lowering::TypeItemIr::ClassGroup { class: Some(class), .. }) = type_item else {
            return vec![];
        };

        class
            .functional_dependencies
            .iter()
            .map(|functional_dependency| {
                Fd::new(
                    functional_dependency.determiners.iter().map(|&x| x as usize),
                    functional_dependency.determined.iter().map(|&x| x as usize),
                )
            })
            .collect()
    }

    if file_id == context.id {
        Ok(extract(context.lowered.info.get_type_item(item_id)))
    } else {
        let lowered = context.queries.lowered(file_id)?;
        Ok(extract(lowered.info.get_type_item(item_id)))
    }
}

/// Determines if [`MatchType::Stuck`] arguments can be determined by functional dependencies.
pub fn can_determine_stuck<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
    match_results: &[MatchType],
    stuck_positions: &[usize],
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    if stuck_positions.is_empty() {
        return Ok(true);
    }

    let functional_dependencies = get_functional_dependencies(context, file_id, item_id)?;
    let initial: FxHashSet<_> = match_results
        .iter()
        .enumerate()
        .filter_map(|(index, result)| matches!(result, MatchType::Match).then_some(index))
        .collect();

    let determined = compute_closure(&functional_dependencies, &initial);
    Ok(stuck_positions.iter().all(|index| determined.contains(index)))
}

/// Matches a wanted constraint to an instance.
pub fn match_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    instance: &CandidateInstance,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let Some(decomposed) = toolkit::decompose_instance(state, context, &instance.checked)? else {
        return Ok(MatchInstance::Apart);
    };

    if wanted.arguments.len() != decomposed.arguments.len() {
        return Ok(MatchInstance::Apart);
    }

    let mut bindings = FxHashMap::default();
    let mut equalities = vec![];
    let mut type_match_results = vec![];

    let mut stuck_positions = vec![];
    let mut stuck_type_positions = vec![];

    for (index, (&wanted_argument, &instance_argument)) in
        iter::zip(wanted.arguments.iter(), decomposed.arguments.iter()).enumerate()
    {
        let match_result = match (wanted_argument, instance_argument) {
            (KindOrType::Kind(wanted_argument), KindOrType::Kind(instance_argument))
            | (KindOrType::Type(wanted_argument), KindOrType::Type(instance_argument)) => {
                match_type(
                    state,
                    context,
                    &mut bindings,
                    &mut equalities,
                    wanted_argument,
                    instance_argument,
                )?
            }
            _ => return Ok(MatchInstance::Apart),
        };

        if matches!(match_result, MatchType::Apart) {
            return Ok(MatchInstance::Apart);
        }

        if matches!(match_result, MatchType::Stuck) {
            stuck_positions.push(index);
        }

        if matches!(wanted_argument, KindOrType::Type(_)) {
            let type_index = type_match_results.len();
            if matches!(match_result, MatchType::Stuck) {
                stuck_type_positions.push(type_index);
            }
            type_match_results.push(match_result);
        }
    }

    for binder in &decomposed.binders {
        if bindings.contains_key(&binder.name) {
            continue;
        }

        let binder_kind = SubstituteName::many(state, context, &bindings, binder.kind)?;
        let fresh_unification = state.fresh_unification(context.queries, binder_kind);
        bindings.insert(binder.name, fresh_unification);
    }

    if !stuck_type_positions.is_empty()
        && !can_determine_stuck(
            context,
            wanted.file_id,
            wanted.item_id,
            &type_match_results,
            &stuck_type_positions,
        )?
    {
        return Ok(MatchInstance::Stuck);
    }

    for &index in &stuck_positions {
        let wanted_argument = match wanted.arguments[index] {
            KindOrType::Kind(argument) | KindOrType::Type(argument) => argument,
        };
        let substituted = match decomposed.arguments[index] {
            KindOrType::Kind(argument) | KindOrType::Type(argument) => {
                SubstituteName::many(state, context, &bindings, argument)?
            }
        };
        equalities.push((wanted_argument, substituted));
    }

    let constraints = decomposed
        .constraints
        .into_iter()
        .map(|constraint| SubstituteName::many(state, context, &bindings, constraint))
        .collect::<QueryResult<Vec<_>>>()?;

    Ok(MatchInstance::Match { constraints, equalities })
}

/// Matches an argument from a wanted constraint to one from an instance.
///
/// This function emits substitutions and equalities when matching against
/// instances, for example:
///
/// ```purescript
/// instance TypeEq a a True
/// ```
///
/// When matching the wanted constraint `TypeEq Int ?0` against this instance,
/// it creates the binding `a := Int`. This means that subsequent usages of `a`
/// are equal to `Int`, turning the second `match(a, ?0)` into `match(Int, ?0)`.
/// We use the [`can_unify`] function to speculate if these two types can be
/// unified, or if unifying them solves unification variables, encoded by the
/// [`CanUnify::Unify`] variant.
fn match_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &mut FxHashMap<Name, TypeId>,
    equalities: &mut Vec<(TypeId, TypeId)>,
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

    let wanted_t = context.lookup_type(wanted);
    let given_t = context.lookup_type(given);

    match (wanted_t, given_t) {
        (_, Type::Rigid(name, _, _)) => {
            if let Some(&bound) = bindings.get(&name) {
                match can_unify(state, context, wanted, bound)? {
                    CanUnify::Equal => Ok(MatchType::Match),
                    CanUnify::Apart => Ok(MatchType::Apart),
                    CanUnify::Unify => {
                        equalities.push((wanted, bound));
                        Ok(MatchType::Match)
                    }
                }
            } else {
                bindings.insert(name, wanted);
                Ok(MatchType::Match)
            }
        }

        (Type::Unification(_), _) => Ok(MatchType::Stuck),

        (Type::Row(wanted_row_id), Type::Row(given_row_id)) => {
            let wanted_row = context.lookup_row_type(wanted_row_id);
            let given_row = context.lookup_row_type(given_row_id);
            match_row_type(state, context, bindings, equalities, wanted_row, given_row)
        }

        (
            Type::Application(wanted_function, wanted_argument),
            Type::Application(given_function, given_argument),
        ) => match_type(state, context, bindings, equalities, wanted_function, given_function)?
            .and_then(|| {
                match_type(state, context, bindings, equalities, wanted_argument, given_argument)
            }),

        (
            Type::Function(wanted_argument, wanted_result),
            Type::Function(given_argument, given_result),
        ) => match_type(state, context, bindings, equalities, wanted_argument, given_argument)?
            .and_then(|| {
                match_type(state, context, bindings, equalities, wanted_result, given_result)
            }),

        (Type::Function(wanted_argument, wanted_result), Type::Application(given_function, _)) => {
            let given_function = normalise::expand(state, context, given_function)?;
            if matches!(context.lookup_type(given_function), Type::Application(_, _)) {
                let wanted = context.intern_function_application(wanted_argument, wanted_result);
                match_type(state, context, bindings, equalities, wanted, given)
            } else {
                Ok(MatchType::Apart)
            }
        }

        (Type::Application(wanted_function, _), Type::Function(given_argument, given_result)) => {
            let wanted_function = normalise::expand(state, context, wanted_function)?;
            if matches!(context.lookup_type(wanted_function), Type::Application(_, _)) {
                let given = context.intern_function_application(given_argument, given_result);
                match_type(state, context, bindings, equalities, wanted, given)
            } else {
                Ok(MatchType::Apart)
            }
        }

        (
            Type::KindApplication(wanted_function, wanted_argument),
            Type::KindApplication(given_function, given_argument),
        ) => match_type(state, context, bindings, equalities, wanted_function, given_function)?
            .and_then(|| {
                match_type(state, context, bindings, equalities, wanted_argument, given_argument)
            }),

        (Type::Kinded(wanted_inner, wanted_kind), Type::Kinded(given_inner, given_kind)) => {
            match_type(state, context, bindings, equalities, wanted_inner, given_inner)?.and_then(
                || match_type(state, context, bindings, equalities, wanted_kind, given_kind),
            )
        }

        _ => Ok(MatchType::Apart),
    }
}

/// Matches row types in instance heads.
///
/// This function handles structural row matching for both the tail variable
/// form `( | r )` in determiner positions and labeled rows in determined
/// positions `( x :: T | r )`. This function partitions the two row types,
/// matches the shared fields, and handles the row tail.
fn match_row_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &mut FxHashMap<Name, TypeId>,
    equalities: &mut Vec<(TypeId, TypeId)>,
    wanted_row: RowType,
    given_row: RowType,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
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
                result = result.and_then(|| {
                    match_type(state, context, bindings, equalities, wanted.id, given.id)
                })?;
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
            Open(_) => Ok(MatchType::Stuck),
        },
        // If the given row has a tail, match it against the
        // additional fields and tail from the wanted row
        Open(given_tail) => {
            let fields = wanted_only.into_iter().cloned().collect_vec();
            let row =
                context.intern_row(context.intern_row_type(RowType::new(fields, wanted_row.tail)));
            result.and_then(|| match_type(state, context, bindings, equalities, row, given_tail))
        }
        // If we have a closed given row
        Closed => match wanted_rest {
            // we cannot match it against fields in the wanted row
            Additional => Ok(MatchType::Apart),
            // we could make progress with an open wanted row
            Open(_) => Ok(MatchType::Stuck),
            // we can match it directly with a closed wanted row
            Closed => Ok(result),
        },
    }
}

/// Validates that all rows in instance declaration arguments
/// do not have labels in non-determined positions.
///
/// In PureScript, instance declarations can only contain rows with labels
/// in positions that are determined by functional dependencies. In the
/// determiner position, only row variables such as `( | r )` are valid.
pub fn validate_rows<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_file: FileId,
    class_item: TypeItemId,
    arguments: &[TypeId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let functional_dependencies = get_functional_dependencies(context, class_file, class_item)?;
    let all_determined = get_all_determined(&functional_dependencies);

    for (position, &argument_type) in arguments.iter().enumerate() {
        if all_determined.contains(&position) {
            continue;
        }

        if HasLabeledRole::contains(state, context, argument_type)? {
            let type_message = state.pretty_id(context, argument_type)?;
            state.insert_error(ErrorKind::InstanceHeadLabeledRow {
                class_file,
                class_item,
                position,
                type_message,
            });
        }
    }

    Ok(())
}

struct CollectFileReferences<'a> {
    files: &'a mut FxHashSet<FileId>,
}

impl<'a> CollectFileReferences<'a> {
    fn collect<Q>(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: TypeId,
        files: &'a mut FxHashSet<FileId>,
    ) -> QueryResult<()>
    where
        Q: ExternalQueries,
    {
        let id = normalise::expand(state, context, id)?;
        walk_type(state, context, id, &mut CollectFileReferences { files })
    }
}

impl TypeWalker for CollectFileReferences<'_> {
    fn visit<Q>(
        &mut self,
        _state: &mut CheckState,
        _context: &CheckContext<Q>,
        _id: TypeId,
        t: &Type,
    ) -> QueryResult<WalkAction>
    where
        Q: ExternalQueries,
    {
        if let Type::Constructor(file_id, _) = t {
            self.files.insert(*file_id);
        }
        Ok(WalkAction::Continue)
    }
}

struct HasLabeledRole {
    contains: bool,
}

impl HasLabeledRole {
    fn contains<Q>(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: TypeId,
    ) -> QueryResult<bool>
    where
        Q: ExternalQueries,
    {
        let mut walker = HasLabeledRole { contains: false };
        walk_type(state, context, id, &mut walker)?;
        Ok(walker.contains)
    }
}

impl TypeWalker for HasLabeledRole {
    fn visit<Q>(
        &mut self,
        _state: &mut CheckState,
        context: &CheckContext<Q>,
        _id: TypeId,
        t: &Type,
    ) -> QueryResult<WalkAction>
    where
        Q: ExternalQueries,
    {
        if let Type::Row(row_id) = t {
            let row = context.lookup_row_type(*row_id);
            if !row.fields.is_empty() {
                self.contains = true;
                return Ok(WalkAction::Stop);
            }
        }
        Ok(WalkAction::Continue)
    }
}
