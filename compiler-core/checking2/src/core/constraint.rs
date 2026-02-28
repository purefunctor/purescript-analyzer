pub mod functional_dependency;

use std::collections::{HashSet, VecDeque};
use std::mem;

use building_types::QueryResult;
use files::FileId;
use indexing::{InstanceChainId, TypeItemId};
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::context::CheckContext;
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::walk::{self, TypeWalker, WalkAction};
use crate::core::{
    CheckedInstance, ForallBinder, Name, Type, TypeId, normalise, toolkit, unification,
};
use crate::error::ErrorKind;
use crate::implication::ImplicationId;
use crate::state::CheckState;
use crate::{CheckedModule, ExternalQueries, safe_loop};

use functional_dependency::{Fd, compute_closure, get_all_determined};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConstraintApplication {
    pub file_id: FileId,
    pub item_id: TypeItemId,
    pub arguments: Vec<TypeId>,
}

#[derive(Debug, Clone)]
enum MatchInstance {
    Match { constraints: Vec<TypeId>, equalities: Vec<(TypeId, TypeId)> },
    Apart,
    Stuck,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MatchType {
    Match,
    Apart,
    Stuck,
}

impl MatchType {
    fn and_then(self, f: impl FnOnce() -> QueryResult<MatchType>) -> QueryResult<MatchType> {
        if let MatchType::Match = self { f() } else { Ok(self) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CanUnify {
    Equal,
    Apart,
    Unify,
}

impl CanUnify {
    fn and_then(self, f: impl FnOnce() -> QueryResult<CanUnify>) -> QueryResult<CanUnify> {
        if let CanUnify::Equal = self { f() } else { Ok(self) }
    }
}

#[derive(Clone)]
struct CandidateInstance {
    chain_id: Option<InstanceChainId>,
    position: u32,
    checked: CheckedInstance,
}

struct DecomposedInstance {
    binders: Vec<ForallBinder>,
    arguments: Vec<TypeId>,
    constraints: Vec<TypeId>,
}

pub fn solve_implication<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let implication = state.implications.current();
    solve_implication_id(state, context, implication, &[])
}

/// Recursively solves an implication and its children.
fn solve_implication_id<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    implication: ImplicationId,
    inherited: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let (wanted, given, children) = {
        let node = &mut state.implications[implication];
        (mem::take(&mut node.wanted), mem::take(&mut node.given), node.children.clone())
    };

    let all_given = inherited.iter().copied().chain(given.iter().copied()).collect_vec();

    // Solve this implication's children with all_given.
    for child in &children {
        let residual = solve_implication_id(state, context, *child, &all_given)?;

        // TODO: partition_by_skolem_escape once skolems are introduced.
        state.implications[implication].wanted.extend(residual);
    }

    // Solve this implication's wanted constraints with all_given.
    let remaining = mem::take(&mut state.implications[implication].wanted);
    let wanted: VecDeque<_> = wanted.into_iter().chain(remaining).collect();
    let residuals = solve_constraints(state, context, wanted, &all_given)?;

    let implication = &mut state.implications[implication];
    implication.given = given;
    implication.wanted = residuals.iter().copied().collect();

    Ok(residuals)
}

fn solve_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: VecDeque<TypeId>,
    given: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let given = elaborate_given(state, context, given)?;
    let mut work_queue = wanted;
    let mut residual = vec![];

    loop {
        let mut made_progress = false;

        'work: while let Some(wanted) = work_queue.pop_front() {
            let Some(application) = constraint_application(state, context, wanted)? else {
                residual.push(wanted);
                continue;
            };

            match match_given_instances(state, context, &application, &given)? {
                Some(MatchInstance::Match { equalities, .. }) => {
                    for (t1, t2) in equalities {
                        if unification::unify(state, context, t1, t2)? {
                            made_progress = true;
                        }
                    }
                    continue 'work;
                }
                Some(MatchInstance::Apart | MatchInstance::Stuck) | None => {}
            }

            let instance_chains = collect_instance_chains(state, context, &application)?;

            for chain in instance_chains {
                'chain: for instance in chain {
                    match match_instance(state, context, &application, &instance)? {
                        MatchInstance::Match { constraints, equalities } => {
                            for (t1, t2) in equalities {
                                if unification::unify(state, context, t1, t2)? {
                                    made_progress = true;
                                }
                            }
                            work_queue.extend(constraints);
                            continue 'work;
                        }
                        MatchInstance::Apart => continue 'chain,
                        MatchInstance::Stuck => break 'chain,
                    }
                }
            }

            residual.push(wanted);
        }

        if made_progress && !residual.is_empty() {
            work_queue.extend(residual.drain(..));
        } else {
            break;
        }
    }

    Ok(residual)
}

pub fn constraint_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<ConstraintApplication>>
where
    Q: ExternalQueries,
{
    let (constructor, arguments) = toolkit::extract_type_application(state, context, id)?;
    let constructor = normalise::normalise(state, context, constructor)?;
    Ok(match context.lookup_type(constructor) {
        Type::Constructor(file_id, item_id) => {
            Some(ConstraintApplication { file_id, item_id, arguments })
        }
        _ => None,
    })
}

/// Discovers implied constraints from given constraints.
fn elaborate_given<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[TypeId],
) -> QueryResult<Vec<ConstraintApplication>>
where
    Q: ExternalQueries,
{
    let mut elaborated = vec![];

    for &constraint in given {
        elaborated.push(constraint);
        elaborate_superclasses(state, context, constraint, &mut elaborated)?;
    }

    elaborated
        .into_iter()
        .map(|constraint| constraint_application(state, context, constraint))
        .filter_map_ok(|constraint| constraint)
        .collect()
}

/// Discovers superclass constraints for a given constraint.
pub fn elaborate_superclasses<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraint: TypeId,
    constraints: &mut Vec<TypeId>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    fn aux<Q>(
        state: &mut CheckState,
        context: &CheckContext<Q>,
        constraint: TypeId,
        constraints: &mut Vec<TypeId>,
        seen: &mut HashSet<(FileId, TypeItemId)>,
    ) -> QueryResult<()>
    where
        Q: ExternalQueries,
    {
        let Some(application) = constraint_application(state, context, constraint)? else {
            return Ok(());
        };

        if !seen.insert((application.file_id, application.item_id)) {
            return Ok(());
        }

        let Some(class_info) =
            toolkit::lookup_file_class(state, context, application.file_id, application.item_id)?
        else {
            return Ok(());
        };

        if class_info.superclasses.is_empty() {
            return Ok(());
        }

        let mut bindings = NameToType::default();
        for (binder_id, &argument) in
            class_info.type_parameters.iter().zip(application.arguments.iter())
        {
            let binder = context.lookup_forall_binder(*binder_id);
            bindings.insert(binder.name, argument);
        }

        for &superclass in &class_info.superclasses {
            let substituted = SubstituteName::many(state, context, &bindings, superclass)?;
            constraints.push(substituted);
            aux(state, context, substituted, constraints, seen)?;
        }

        Ok(())
    }

    let mut seen = HashSet::new();
    aux(state, context, constraint, constraints, &mut seen)
}

/// Collects instance chains for a constraint from all eligible files.
fn collect_instance_chains<Q>(
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
            let checked = context.queries.checked2(file_id)?;
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
fn can_determine_stuck<Q>(
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
    let initial: HashSet<_> = match_results
        .iter()
        .enumerate()
        .filter_map(|(index, result)| matches!(result, MatchType::Match).then_some(index))
        .collect();

    let determined = compute_closure(&functional_dependencies, &initial);
    Ok(stuck_positions.iter().all(|index| determined.contains(index)))
}

fn decompose_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    instance: &CheckedInstance,
) -> QueryResult<Option<DecomposedInstance>>
where
    Q: ExternalQueries,
{
    let toolkit::InspectQuantified { binders, quantified } =
        toolkit::inspect_quantified(state, context, instance.canonical)?;

    let mut current = quantified;
    let mut constraints = vec![];

    safe_loop! {
        current = normalise::normalise(state, context, current)?;
        match context.lookup_type(current) {
            Type::Constrained(constraint, constrained) => {
                constraints.push(constraint);
                current = constrained;
            }
            _ => break,
        }
    }

    let Some(application) = constraint_application(state, context, current)? else {
        return Ok(None);
    };

    if (application.file_id, application.item_id) != instance.resolution {
        return Ok(None);
    }

    Ok(Some(DecomposedInstance { binders, arguments: application.arguments, constraints }))
}

/// Matches a wanted constraint to an instance.
fn match_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    instance: &CandidateInstance,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let Some(decomposed) = decompose_instance(state, context, &instance.checked)? else {
        return Ok(MatchInstance::Apart);
    };

    if wanted.arguments.len() != decomposed.arguments.len() {
        return Ok(MatchInstance::Apart);
    }

    let mut bindings = FxHashMap::default();
    let mut equalities = vec![];
    let mut match_results = vec![];
    let mut stuck_positions = vec![];

    for (index, (&wanted_argument, &instance_argument)) in
        wanted.arguments.iter().zip(decomposed.arguments.iter()).enumerate()
    {
        let match_result = match_type(
            state,
            context,
            &mut bindings,
            &mut equalities,
            wanted_argument,
            instance_argument,
        )?;

        if matches!(match_result, MatchType::Apart) {
            return Ok(MatchInstance::Apart);
        }

        if matches!(match_result, MatchType::Stuck) {
            stuck_positions.push(index);
        }

        match_results.push(match_result);
    }

    if !stuck_positions.is_empty()
        && !can_determine_stuck(
            context,
            wanted.file_id,
            wanted.item_id,
            &match_results,
            &stuck_positions,
        )?
    {
        return Ok(MatchInstance::Stuck);
    }

    for &index in &stuck_positions {
        let substituted =
            SubstituteName::many(state, context, &bindings, decomposed.arguments[index])?;
        equalities.push((wanted.arguments[index], substituted));
    }

    for binder in &decomposed.binders {
        if !bindings.contains_key(&binder.name) {
            let unification = state.fresh_unification(context.queries, binder.kind);
            bindings.insert(binder.name, unification);
        }
    }

    let constraints = decomposed
        .constraints
        .into_iter()
        .map(|constraint| SubstituteName::many(state, context, &bindings, constraint))
        .collect::<QueryResult<Vec<_>>>()?;

    Ok(MatchInstance::Match { constraints, equalities })
}

/// Matches a wanted constraint to given constraints.
fn match_given_instances<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    given: &[ConstraintApplication],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    'given: for given in given {
        if wanted.file_id != given.file_id || wanted.item_id != given.item_id {
            continue;
        }

        if wanted.arguments.len() != given.arguments.len() {
            continue;
        }

        let mut stuck_positions = vec![];

        for (index, (&wanted_argument, &given_argument)) in
            wanted.arguments.iter().zip(given.arguments.iter()).enumerate()
        {
            let match_result = match_given_type(state, context, wanted_argument, given_argument)?;

            if matches!(match_result, MatchType::Apart) {
                continue 'given;
            }

            if matches!(match_result, MatchType::Stuck) {
                stuck_positions.push(index);
            }
        }

        // Given constraints are valid by construction. When a unification
        // variable makes a position stuck, it's safe to emit an equality
        // rather than require functional dependencies to cover it.
        let equalities = stuck_positions
            .into_iter()
            .map(|index| (wanted.arguments[index], given.arguments[index]));
        return Ok(Some(MatchInstance::Match {
            constraints: vec![],
            equalities: equalities.collect(),
        }));
    }

    Ok(None)
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
    let wanted = normalise::normalise(state, context, wanted)?;
    let given = normalise::normalise(state, context, given)?;

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

        (Type::Application(wf, wa), Type::Application(gf, ga)) => {
            match_type(state, context, bindings, equalities, wf, gf)?
                .and_then(|| match_type(state, context, bindings, equalities, wa, ga))
        }

        (Type::Function(wa, wr), Type::Function(ga, gr)) => {
            match_type(state, context, bindings, equalities, wa, ga)?
                .and_then(|| match_type(state, context, bindings, equalities, wr, gr))
        }

        (Type::Function(wa, wr), Type::Application(_, _)) => {
            let wanted = context.intern_function_application(wa, wr);
            match_type(state, context, bindings, equalities, wanted, given)
        }

        (Type::Application(_, _), Type::Function(ga, gr)) => {
            let given = context.intern_function_application(ga, gr);
            match_type(state, context, bindings, equalities, wanted, given)
        }

        (Type::KindApplication(wf, wa), Type::KindApplication(gf, ga)) => {
            match_type(state, context, bindings, equalities, wf, gf)?
                .and_then(|| match_type(state, context, bindings, equalities, wa, ga))
        }

        (Type::Kinded(wi, wk), Type::Kinded(gi, gk)) => {
            match_type(state, context, bindings, equalities, wi, gi)?
                .and_then(|| match_type(state, context, bindings, equalities, wk, gk))
        }

        (Type::OperatorApplication(wf, wi, wl, wr), Type::OperatorApplication(gf, gi, gl, gr))
            if wf == gf && wi == gi =>
        {
            match_type(state, context, bindings, equalities, wl, gl)?
                .and_then(|| match_type(state, context, bindings, equalities, wr, gr))
        }

        (Type::SynonymApplication(wsyn), Type::SynonymApplication(gsyn)) => {
            let wsyn = context.lookup_synonym(wsyn);
            let gsyn = context.lookup_synonym(gsyn);

            if wsyn.reference != gsyn.reference || wsyn.arguments.len() != gsyn.arguments.len() {
                return Ok(MatchType::Apart);
            }

            wsyn.arguments.iter().zip(gsyn.arguments.iter()).try_fold(
                MatchType::Match,
                |result, (&wa, &ga)| {
                    result.and_then(|| match_type(state, context, bindings, equalities, wa, ga))
                },
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
    wanted_row: crate::core::RowType,
    given_row: crate::core::RowType,
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
        fn new(only: &[&crate::core::RowField], tail: Option<TypeId>) -> RowRest {
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
            let row = context.intern_row(
                context.intern_row_type(crate::core::RowType::new(fields, wanted_row.tail)),
            );
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

/// Matches an argument from a wanted constraint to one from a given constraint.
///
/// This function is specialised for matching given constraints, like those
/// found in value signatures rather than top-level instance declarations;
/// unlike [`match_type`], this function does not build bindings or equalities
/// for rigid variables.
fn match_given_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: TypeId,
    given: TypeId,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
{
    let wanted = normalise::normalise(state, context, wanted)?;
    let given = normalise::normalise(state, context, given)?;

    if wanted == given {
        return Ok(MatchType::Match);
    }

    let wanted_t = context.lookup_type(wanted);
    let given_t = context.lookup_type(given);

    match (wanted_t, given_t) {
        (Type::Unification(_), _) => Ok(MatchType::Stuck),

        (Type::Rigid(wname, _, wkind), Type::Rigid(gname, _, gkind)) => {
            if wname == gname {
                match_given_type(state, context, wkind, gkind)
            } else {
                Ok(MatchType::Apart)
            }
        }

        (Type::Application(wf, wa), Type::Application(gf, ga)) => {
            match_given_type(state, context, wf, gf)?
                .and_then(|| match_given_type(state, context, wa, ga))
        }

        (Type::Function(wa, wr), Type::Function(ga, gr)) => {
            match_given_type(state, context, wa, ga)?
                .and_then(|| match_given_type(state, context, wr, gr))
        }

        (Type::Function(wa, wr), Type::Application(_, _)) => {
            let wanted = context.intern_function_application(wa, wr);
            match_given_type(state, context, wanted, given)
        }

        (Type::Application(_, _), Type::Function(ga, gr)) => {
            let given = context.intern_function_application(ga, gr);
            match_given_type(state, context, wanted, given)
        }

        (Type::Row(wanted_row_id), Type::Row(given_row_id)) => {
            let wanted_row = context.lookup_row_type(wanted_row_id);
            let given_row = context.lookup_row_type(given_row_id);

            if wanted_row.fields.len() != given_row.fields.len() {
                return Ok(MatchType::Apart);
            }

            let mut result = MatchType::Match;
            for (wanted_field, given_field) in wanted_row.fields.iter().zip(given_row.fields.iter())
            {
                if wanted_field.label != given_field.label {
                    return Ok(MatchType::Apart);
                }
                result = result.and_then(|| {
                    match_given_type(state, context, wanted_field.id, given_field.id)
                })?;
            }

            match (wanted_row.tail, given_row.tail) {
                (Some(wanted_tail), Some(given_tail)) => {
                    result.and_then(|| match_given_type(state, context, wanted_tail, given_tail))
                }
                (Some(wanted_tail), None) => {
                    let wanted_tail = normalise::normalise(state, context, wanted_tail)?;
                    if matches!(context.lookup_type(wanted_tail), Type::Unification(_)) {
                        Ok(MatchType::Stuck)
                    } else {
                        Ok(MatchType::Apart)
                    }
                }
                (None, Some(given_tail)) => {
                    let given_tail = normalise::normalise(state, context, given_tail)?;
                    if matches!(context.lookup_type(given_tail), Type::Unification(_)) {
                        Ok(MatchType::Stuck)
                    } else {
                        Ok(MatchType::Apart)
                    }
                }
                (None, None) => Ok(result),
            }
        }

        (Type::KindApplication(wf, wa), Type::KindApplication(gf, ga)) => {
            match_given_type(state, context, wf, gf)?
                .and_then(|| match_given_type(state, context, wa, ga))
        }

        (Type::OperatorApplication(wf, wi, wl, wr), Type::OperatorApplication(gf, gi, gl, gr))
            if wf == gf && wi == gi =>
        {
            match_given_type(state, context, wl, gl)?
                .and_then(|| match_given_type(state, context, wr, gr))
        }

        (Type::SynonymApplication(wsyn), Type::SynonymApplication(gsyn)) => {
            let wsyn = context.lookup_synonym(wsyn);
            let gsyn = context.lookup_synonym(gsyn);

            if wsyn.reference != gsyn.reference || wsyn.arguments.len() != gsyn.arguments.len() {
                return Ok(MatchType::Apart);
            }

            wsyn.arguments
                .iter()
                .zip(gsyn.arguments.iter())
                .try_fold(MatchType::Match, |result, (&wa, &ga)| {
                    result.and_then(|| match_given_type(state, context, wa, ga))
                })
        }

        _ => Ok(MatchType::Apart),
    }
}

/// Determines if two types [`CanUnify`].
///
/// This is used in [`match_type`], where if two different types bind to the
/// same rigid variable, we determine if the types can actually unify before
/// generating an equality. This is effectively a pure version of the
/// [`unification::unify`] function.
fn can_unify<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t1: TypeId,
    t2: TypeId,
) -> QueryResult<CanUnify>
where
    Q: ExternalQueries,
{
    let t1 = normalise::normalise(state, context, t1)?;
    let t2 = normalise::normalise(state, context, t2)?;

    if t1 == t2 {
        return Ok(CanUnify::Equal);
    }

    let t1_core = context.lookup_type(t1);
    let t2_core = context.lookup_type(t2);

    match (t1_core, t2_core) {
        (Type::Unification(_), _) | (_, Type::Unification(_)) => Ok(CanUnify::Unify),
        (Type::Unknown(_), _) | (_, Type::Unknown(_)) => Ok(CanUnify::Unify),
        (Type::Row(_), Type::Row(_)) => Ok(CanUnify::Unify),

        (Type::Application(f1, a1), Type::Application(f2, a2)) => {
            can_unify(state, context, f1, f2)?.and_then(|| can_unify(state, context, a1, a2))
        }
        (Type::Function(a1, r1), Type::Function(a2, r2)) => {
            can_unify(state, context, a1, a2)?.and_then(|| can_unify(state, context, r1, r2))
        }
        // Function(a, b) and Application(Application(f, a), b) can
        // unify when `f` resolves to the Function constructor.
        (Type::Function(..), Type::Application(..))
        | (Type::Application(..), Type::Function(..)) => Ok(CanUnify::Unify),
        (Type::KindApplication(f1, a1), Type::KindApplication(f2, a2)) => {
            can_unify(state, context, f1, f2)?.and_then(|| can_unify(state, context, a1, a2))
        }
        (Type::Kinded(i1, k1), Type::Kinded(i2, k2)) => {
            can_unify(state, context, i1, i2)?.and_then(|| can_unify(state, context, k1, k2))
        }
        (Type::Constrained(c1, i1), Type::Constrained(c2, i2)) => {
            can_unify(state, context, c1, c2)?.and_then(|| can_unify(state, context, i1, i2))
        }
        (Type::Rigid(n1, _, k1), Type::Rigid(n2, _, k2)) => {
            if n1 == n2 {
                can_unify(state, context, k1, k2)
            } else {
                Ok(CanUnify::Apart)
            }
        }
        (Type::Forall(b1, i1), Type::Forall(b2, i2)) => {
            let b1 = context.lookup_forall_binder(b1);
            let b2 = context.lookup_forall_binder(b2);
            can_unify(state, context, b1.kind, b2.kind)?
                .and_then(|| can_unify(state, context, i1, i2))
        }
        (Type::OperatorApplication(f1, i1, l1, r1), Type::OperatorApplication(f2, i2, l2, r2)) => {
            if f1 == f2 && i1 == i2 {
                can_unify(state, context, l1, l2)?.and_then(|| can_unify(state, context, r1, r2))
            } else {
                Ok(CanUnify::Apart)
            }
        }
        (Type::SynonymApplication(s1), Type::SynonymApplication(s2)) => {
            let s1 = context.lookup_synonym(s1);
            let s2 = context.lookup_synonym(s2);
            if s1.reference != s2.reference || s1.arguments.len() != s2.arguments.len() {
                return Ok(CanUnify::Apart);
            }

            s1.arguments
                .iter()
                .zip(s2.arguments.iter())
                .try_fold(CanUnify::Equal, |result, (&a1, &a2)| {
                    result.and_then(|| can_unify(state, context, a1, a2))
                })
        }
        _ => Ok(CanUnify::Apart),
    }
}

/// Validates that all rows in instance declaration arguments
/// do not have labels in non-determined positions.
///
/// In PureScript, instance declarations can only contain rows with labels
/// in positions that are determined by functional dependencies. In the
/// determiner position, only row variables such as `( | r )` are valid.
pub fn validate_instance_rows<Q>(
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
        walk::walk_type(state, context, id, &mut CollectFileReferences { files })
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
        walk::walk_type(state, context, id, &mut walker)?;
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
