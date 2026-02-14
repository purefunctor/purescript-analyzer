/// Implements compiler-solved instances.
mod compiler_solved;

/// Implements functional dependencies.
mod functional_dependency;

use compiler_solved::*;
use functional_dependency::{Fd, get_all_determined};

use std::collections::{HashSet, VecDeque};
use std::sync::Arc;
use std::{iter, mem};

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::algorithm::fold::{FoldAction, TypeFold, fold_type};
use crate::algorithm::state::implication::ImplicationId;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::visit::{
    CollectFileReferences, HasLabeledRole, TypeVisitor, VisitAction, visit_type,
};
use crate::algorithm::{substitute, toolkit, transfer, unification};
use crate::core::{self, Class, Instance, InstanceKind, Name, Variable};
use crate::{CheckedModule, ExternalQueries, Type, TypeId};

#[tracing::instrument(skip_all, name = "solve_constraints")]
pub fn solve_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: VecDeque<TypeId>,
    given: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    crate::debug_fields!(state, context, {
        ?wanted = wanted.len(),
        ?given = given.len(),
    });

    let given = elaborate_given(state, context, given)?;

    let mut work_queue = wanted;
    let mut residual = vec![];

    loop {
        let mut made_progress = false;

        'work: while let Some(wanted) = work_queue.pop_front() {
            crate::trace_fields!(state, context, { wanted = wanted }, "work");

            let Some(application) = constraint_application(state, wanted) else {
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
                Some(MatchInstance::Apart | MatchInstance::Stuck) | None => (),
            }

            match match_compiler_instances(state, context, &application, &given)? {
                Some(MatchInstance::Match { constraints, equalities }) => {
                    for (t1, t2) in equalities {
                        if unification::unify(state, context, t1, t2)? {
                            made_progress = true;
                        }
                    }
                    work_queue.extend(constraints);
                    continue 'work;
                }
                Some(MatchInstance::Stuck) => {
                    residual.push(wanted);
                    continue 'work;
                }
                Some(MatchInstance::Apart) | None => (),
            }

            let instance_chains = collect_instance_chains(state, context, &application)?;

            for chain in instance_chains {
                'chain: for instance in chain {
                    match match_instance(state, context, &application, instance)? {
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

    crate::debug_fields!(state, context, { ?residual = residual.len() });

    Ok(residual)
}

#[tracing::instrument(skip_all, name = "solve_implication")]
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

    let all_given = {
        let inherited = inherited.iter().copied();
        let given = given.iter().copied();
        inherited.chain(given).collect_vec()
    };

    crate::debug_fields!(state, context, {
        ?implication = implication,
        ?wanted = wanted.len(),
        ?given = given.len(),
        ?inherited = inherited.len(),
        ?children = children.len(),
    });

    // Solve this implication's children with all_given.
    for child in &children {
        let residual = solve_implication_id(state, context, *child, &all_given)?;

        crate::debug_fields!(state, context, {
            ?child = child,
            ?residual = residual.len(),
        });

        // TODO: partition_by_skolem_escape once skolems are introduced.
        state.implications[implication].wanted.extend(residual);
    }

    // Solve this implication's wanted constraints with all_given.
    let remaining = mem::take(&mut state.implications[implication].wanted);
    let wanted: VecDeque<TypeId> = wanted.into_iter().chain(remaining).collect();
    let residuals = solve_constraints(state, context, wanted, &all_given)?;

    let implication = &mut state.implications[implication];
    implication.given = given;
    implication.wanted = Vec::clone(&residuals).into();

    Ok(residuals)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct ConstraintApplication {
    pub(crate) file_id: FileId,
    pub(crate) item_id: TypeItemId,
    pub(crate) arguments: Vec<TypeId>,
}

pub(crate) fn constraint_application(
    state: &mut CheckState,
    id: TypeId,
) -> Option<ConstraintApplication> {
    let (constructor, arguments) = toolkit::extract_type_application(state, id);
    if let Type::Constructor(file_id, item_id) = state.storage[constructor] {
        Some(ConstraintApplication { file_id, item_id, arguments })
    } else {
        None
    }
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

    let applications =
        elaborated.into_iter().filter_map(|given| constraint_application(state, given));
    let mut applications = applications.collect_vec();

    let is_coercible = |file_id, item_id| {
        context.prim_coerce.file_id == file_id && context.prim_coerce.coercible == item_id
    };

    // For coercible applications, also elaborate into symmetric versions.
    let symmetric = applications.iter().filter_map(|application| {
        let is_coercible = is_coercible(application.file_id, application.item_id);
        let &[left, right] = application.arguments.as_slice() else { return None };
        is_coercible.then(|| ConstraintApplication {
            file_id: application.file_id,
            item_id: application.item_id,
            arguments: vec![right, left],
        })
    });

    let reversed = symmetric.collect_vec();
    applications.extend(reversed);

    Ok(applications)
}

/// Discovers superclass constraints for a given constraint.
pub(crate) fn elaborate_superclasses<Q>(
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
        let Some(ConstraintApplication { file_id, item_id, arguments }) =
            constraint_application(state, constraint)
        else {
            return Ok(());
        };

        if !seen.insert((file_id, item_id)) {
            return Ok(());
        }

        let Some(class_info) = lookup_file_class(state, context, file_id, item_id)? else {
            return Ok(());
        };

        if class_info.superclasses.is_empty() {
            return Ok(());
        }

        let mut bindings: substitute::NameToType = FxHashMap::default();
        for (name, &argument) in class_info.type_variable_names.iter().zip(arguments.iter()) {
            bindings.insert(name.clone(), argument);
        }

        for &(superclass, _) in class_info.superclasses.iter() {
            let substituted = ApplyBindings::on(state, &bindings, superclass);
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
) -> QueryResult<Vec<Vec<Instance>>>
where
    Q: ExternalQueries,
{
    let class_file = application.file_id;
    let class_id = application.item_id;

    let mut files_to_search = FxHashSet::default();
    files_to_search.insert(class_file);

    for &argument in &application.arguments {
        CollectFileReferences::on(state, argument, &mut files_to_search);
    }

    let mut all_instances = vec![];

    let mut copy_from = |checked: &CheckedModule| {
        let instances = checked.instances.values();
        let derived = checked.derived.values();

        let matching = iter::chain(instances, derived)
            .filter(|instance| instance.resolution == (class_file, class_id))
            .cloned();

        all_instances.extend(matching);
    };

    for &file_id in &files_to_search {
        if file_id == context.id {
            copy_from(&state.checked);
        } else {
            let checked = context.queries.checked(file_id)?;
            copy_from(&checked);
        }
    }

    let mut chain_map: FxHashMap<_, Vec<_>> = FxHashMap::default();
    let mut all_chains: Vec<Vec<_>> = vec![];

    for instance in all_instances {
        if let InstanceKind::Chain { id, .. } = instance.kind {
            chain_map.entry(id).or_default().push(instance)
        } else {
            all_chains.push(vec![instance])
        }
    }

    for (_, mut chain) in chain_map {
        chain.sort_by_key(|instance| match instance.kind {
            InstanceKind::Chain { position, .. } => position,
            InstanceKind::Derive => 0,
        });
        all_chains.push(chain);
    }

    Ok(all_chains)
}

fn localize_class<Q>(state: &mut CheckState, context: &CheckContext<Q>, class: &Class) -> Class
where
    Q: ExternalQueries,
{
    let superclasses = class.superclasses.iter().map(|&(t, k)| {
        let t = transfer::localize(state, context, t);
        let k = transfer::localize(state, context, k);
        (t, k)
    });

    let superclasses = superclasses.collect();

    let type_variable_kinds =
        class.type_variable_kinds.iter().map(|&kind| transfer::localize(state, context, kind));

    let type_variable_kinds = type_variable_kinds.collect();

    Class {
        superclasses,
        type_variable_kinds,
        type_variable_names: class.type_variable_names.clone(),
        quantified_variables: class.quantified_variables,
        kind_variables: class.kind_variables,
    }
}

fn lookup_local_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> Option<Class>
where
    Q: ExternalQueries,
{
    let class = state.checked.lookup_class(item_id)?;
    Some(localize_class(state, context, &class))
}

fn lookup_global_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    checked: &CheckedModule,
    item_id: TypeItemId,
) -> Option<Class>
where
    Q: ExternalQueries,
{
    let class = checked.classes.get(&item_id)?;
    Some(localize_class(state, context, class))
}

pub(crate) fn lookup_file_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<Option<Class>>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        Ok(lookup_local_class(state, context, item_id))
    } else {
        let checked = context.queries.checked(file_id)?;
        Ok(lookup_global_class(state, context, &checked, item_id))
    }
}

fn get_functional_dependencies<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<Vec<Fd>>
where
    Q: ExternalQueries,
{
    fn extract_fundeps(type_item: Option<&lowering::TypeItemIr>) -> Vec<Fd> {
        let Some(lowering::TypeItemIr::ClassGroup { class: Some(class), .. }) = type_item else {
            return vec![];
        };

        let class = class.functional_dependencies.iter().map(|functional_dependency| {
            Fd::new(
                functional_dependency.determiners.iter().map(|&x| x as usize),
                functional_dependency.determined.iter().map(|&x| x as usize),
            )
        });

        class.collect()
    }

    if file_id == context.id {
        Ok(extract_fundeps(context.lowered.info.get_type_item(item_id)))
    } else {
        let lowered = context.queries.lowered(file_id)?;
        Ok(extract_fundeps(lowered.info.get_type_item(item_id)))
    }
}

fn compute_match_closures(
    functional_dependencies: &[Fd],
    match_results: &[MatchType],
) -> HashSet<usize> {
    let initial = match_results.iter().enumerate().filter_map(|(index, result)| {
        if matches!(result, MatchType::Match) { Some(index) } else { None }
    });

    let initial: HashSet<usize> = initial.collect();
    functional_dependency::compute_closure(functional_dependencies, &initial)
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
    let determined_positions = compute_match_closures(&functional_dependencies, match_results);

    Ok(stuck_positions.iter().all(|index| determined_positions.contains(index)))
}

#[derive(Debug, Clone, Copy)]
enum MatchType {
    Match,
    Apart,
    Stuck,
}

impl MatchType {
    fn and_also(self, f: impl FnOnce() -> MatchType) -> MatchType {
        if let MatchType::Match = self { f() } else { self }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CanUnify {
    Equal,
    Apart,
    Unify,
}

impl CanUnify {
    fn and_also(self, f: impl FnOnce() -> CanUnify) -> CanUnify {
        if let CanUnify::Equal = self { f() } else { self }
    }

    fn is_apart(self) -> bool {
        matches!(self, CanUnify::Apart)
    }
}

#[derive(Debug, Clone)]
enum MatchInstance {
    Match { constraints: Vec<TypeId>, equalities: Vec<(TypeId, TypeId)> },
    Apart,
    Stuck,
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
) -> MatchType
where
    Q: ExternalQueries,
{
    let wanted = state.normalize_type(wanted);
    let given = state.normalize_type(given);

    if wanted == given {
        return MatchType::Match;
    }

    let wanted_core = &state.storage[wanted];
    let given_core = &state.storage[given];

    match (wanted_core, given_core) {
        (_, Type::Variable(Variable::Bound(name, _))) => {
            if let Some(&bound) = bindings.get(name) {
                match can_unify(state, wanted, bound) {
                    CanUnify::Equal => MatchType::Match,
                    CanUnify::Apart => MatchType::Apart,
                    CanUnify::Unify => {
                        equalities.push((wanted, bound));
                        MatchType::Match
                    }
                }
            } else {
                bindings.insert(name.clone(), wanted);
                MatchType::Match
            }
        }

        (Type::Unification(_), _) => MatchType::Stuck,

        (Type::Row(wanted_row), Type::Row(given_row)) => {
            let wanted_row = wanted_row.clone();
            let given_row = given_row.clone();
            match_row_type(state, context, bindings, equalities, wanted_row, given_row)
        }

        (
            &Type::Application(w_function, w_argument),
            &Type::Application(g_function, g_argument),
        ) => match_type(state, context, bindings, equalities, w_function, g_function)
            .and_also(|| match_type(state, context, bindings, equalities, w_argument, g_argument)),

        (&Type::Function(w_argument, w_result), &Type::Function(g_argument, g_result)) => {
            match_type(state, context, bindings, equalities, w_argument, g_argument)
                .and_also(|| match_type(state, context, bindings, equalities, w_result, g_result))
        }

        (&Type::Function(w_argument, w_result), &Type::Application(_, _)) => {
            let wanted = state.storage.intern(Type::Application(context.prim.function, w_argument));
            let wanted = state.storage.intern(Type::Application(wanted, w_result));
            match_type(state, context, bindings, equalities, wanted, given)
        }

        (&Type::Application(_, _), &Type::Function(g_argument, g_result)) => {
            let given = state.storage.intern(Type::Application(context.prim.function, g_argument));
            let given = state.storage.intern(Type::Application(given, g_result));
            match_type(state, context, bindings, equalities, wanted, given)
        }

        (
            &Type::KindApplication(w_function, w_argument),
            &Type::KindApplication(g_function, g_argument),
        ) => match_type(state, context, bindings, equalities, w_function, g_function)
            .and_also(|| match_type(state, context, bindings, equalities, w_argument, g_argument)),

        (
            &Type::OperatorApplication(f1, t1, l1, r1),
            &Type::OperatorApplication(f2, t2, l2, r2),
        ) => {
            if f1 == f2 && t1 == t2 {
                match_type(state, context, bindings, equalities, l1, l2)
                    .and_also(|| match_type(state, context, bindings, equalities, r1, r2))
            } else {
                MatchType::Apart
            }
        }

        (Type::SynonymApplication(_, f1, t1, a1), Type::SynonymApplication(_, f2, t2, a2)) => {
            if f1 == f2 && t1 == t2 && a1.len() == a2.len() {
                let a1 = Arc::clone(a1);
                let a2 = Arc::clone(a2);
                iter::zip(a1.iter(), a2.iter()).fold(MatchType::Match, |result, (&a1, &a2)| {
                    result.and_also(|| match_type(state, context, bindings, equalities, a1, a2))
                })
            } else {
                MatchType::Apart
            }
        }

        _ => MatchType::Apart,
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
    wanted_row: core::RowType,
    given_row: core::RowType,
) -> MatchType
where
    Q: ExternalQueries,
{
    let mut wanted_only = vec![];
    let mut given_only = vec![];
    let mut result = MatchType::Match;

    let wanted_fields = wanted_row.fields.iter();
    let given_fields = given_row.fields.iter();

    for field in itertools::merge_join_by(wanted_fields, given_fields, |wanted, given| {
        wanted.label.cmp(&given.label)
    }) {
        match field {
            itertools::EitherOrBoth::Both(wanted, given) => {
                result = result.and_also(|| {
                    match_type(state, context, bindings, equalities, wanted.id, given.id)
                });
                // Given an open wanted row, additional fields from the
                // given row can be absorbed into the wanted row's tail.
                if matches!(result, MatchType::Apart) && wanted_row.tail.is_none() {
                    return MatchType::Apart;
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
        fn new(only: &[&core::RowField], tail: Option<TypeId>) -> RowRest {
            if !only.is_empty() {
                RowRest::Additional
            } else if let Some(tail) = tail {
                RowRest::Open(tail)
            } else {
                RowRest::Closed
            }
        }
    }

    let given_rest = RowRest::new(&given_only, given_row.tail);
    let wanted_rest = RowRest::new(&wanted_only, wanted_row.tail);

    use RowRest::*;

    match given_rest {
        // If there are additional given fields
        Additional => match wanted_rest {
            // we cannot match it against a tail-less wanted,
            // nor against the additional wanted fields.
            Closed | Additional => MatchType::Apart,
            // we could potentially make progress by having the
            // wanted tail absorb the additional given fields
            Open(_) => MatchType::Stuck,
        },

        // If the given row has a tail, match it against the
        // additional fields and tail from the wanted row
        Open(given_tail) => {
            let fields = Arc::from_iter(wanted_only.into_iter().cloned());
            let row = core::RowType { fields, tail: wanted_row.tail };
            let row_id = state.storage.intern(Type::Row(row));
            result.and_also(|| match_type(state, context, bindings, equalities, row_id, given_tail))
        }

        // If we have a closed given row
        Closed => match wanted_rest {
            // we cannot match it against fields in the wanted row
            Additional => MatchType::Apart,
            // we could make progress with an open wanted row
            Open(_) => MatchType::Stuck,
            // we can match it directly with a closed wanted row
            Closed => result,
        },
    }
}

/// Matches an argument from a wanted constraint to one from a given constraint.
///
/// This function is specialised for matching given constraints, like those
/// found in value signatures rather than top-level instance declarations;
/// unlike [`match_type`], this function does not build bindings or equalities
/// for [`Variable::Bound`] or [`Variable::Implicit`] variables.
fn match_given_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: TypeId,
    given: TypeId,
) -> MatchType
where
    Q: ExternalQueries,
{
    let wanted = state.normalize_type(wanted);
    let given = state.normalize_type(given);

    if wanted == given {
        return MatchType::Match;
    }

    let wanted_core = &state.storage[wanted];
    let given_core = &state.storage[given];

    match (wanted_core, given_core) {
        (Type::Unification(_), _) => MatchType::Stuck,

        (
            Type::Variable(Variable::Bound(w_name, w_kind)),
            Type::Variable(Variable::Bound(g_name, g_kind)),
        ) => {
            if w_name == g_name {
                match_given_type(state, context, *w_kind, *g_kind)
            } else {
                MatchType::Apart
            }
        }

        (
            Type::Variable(Variable::Skolem(w_name, w_kind)),
            Type::Variable(Variable::Skolem(g_name, g_kind)),
        ) => {
            if w_name == g_name {
                match_given_type(state, context, *w_kind, *g_kind)
            } else {
                MatchType::Apart
            }
        }

        (
            &Type::Application(w_function, w_argument),
            &Type::Application(g_function, g_argument),
        ) => match_given_type(state, context, w_function, g_function)
            .and_also(|| match_given_type(state, context, w_argument, g_argument)),

        (&Type::Function(w_argument, w_result), &Type::Function(g_argument, g_result)) => {
            match_given_type(state, context, w_argument, g_argument)
                .and_also(|| match_given_type(state, context, w_result, g_result))
        }

        (&Type::Function(w_argument, w_result), &Type::Application(_, _)) => {
            let wanted = state.storage.intern(Type::Application(context.prim.function, w_argument));
            let wanted = state.storage.intern(Type::Application(wanted, w_result));
            match_given_type(state, context, wanted, given)
        }

        (&Type::Application(_, _), &Type::Function(g_argument, g_result)) => {
            let given = state.storage.intern(Type::Application(context.prim.function, g_argument));
            let given = state.storage.intern(Type::Application(given, g_result));
            match_given_type(state, context, wanted, given)
        }

        (Type::Row(wanted_row), Type::Row(given_row)) => {
            if wanted_row.fields.len() != given_row.fields.len() {
                return MatchType::Apart;
            }

            let wanted_fields = Arc::clone(&wanted_row.fields);
            let given_fields = Arc::clone(&given_row.fields);

            let wanted_tail = wanted_row.tail;
            let given_tail = given_row.tail;

            let mut result = MatchType::Match;
            for (wanted_field, given_field) in iter::zip(wanted_fields.iter(), given_fields.iter())
            {
                if wanted_field.label != given_field.label {
                    return MatchType::Apart;
                }
                result = result
                    .and_also(|| match_given_type(state, context, wanted_field.id, given_field.id));
            }

            match (wanted_tail, given_tail) {
                (Some(wanted_tail), Some(given_tail)) => {
                    result.and_also(|| match_given_type(state, context, wanted_tail, given_tail))
                }
                (Some(wanted_tail), None) => {
                    let wanted_tail = state.normalize_type(wanted_tail);
                    if matches!(state.storage[wanted_tail], Type::Unification(_)) {
                        result.and_also(|| MatchType::Stuck)
                    } else {
                        MatchType::Apart
                    }
                }
                (None, Some(given_tail)) => {
                    let given_tail = state.normalize_type(given_tail);
                    if matches!(state.storage[given_tail], Type::Unification(_)) {
                        result.and_also(|| MatchType::Stuck)
                    } else {
                        MatchType::Apart
                    }
                }
                (None, None) => result,
            }
        }

        (
            &Type::KindApplication(w_function, w_argument),
            &Type::KindApplication(g_function, g_argument),
        ) => match_given_type(state, context, w_function, g_function)
            .and_also(|| match_given_type(state, context, w_argument, g_argument)),

        (
            &Type::OperatorApplication(f1, t1, l1, r1),
            &Type::OperatorApplication(f2, t2, l2, r2),
        ) => {
            if f1 == f2 && t1 == t2 {
                match_given_type(state, context, l1, l2)
                    .and_also(|| match_given_type(state, context, r1, r2))
            } else {
                MatchType::Apart
            }
        }

        (Type::SynonymApplication(_, f1, t1, a1), Type::SynonymApplication(_, f2, t2, a2)) => {
            if f1 == f2 && t1 == t2 && a1.len() == a2.len() {
                let a1 = Arc::clone(a1);
                let a2 = Arc::clone(a2);
                iter::zip(a1.iter(), a2.iter()).fold(MatchType::Match, |result, (&a1, &a2)| {
                    result.and_also(|| match_given_type(state, context, a1, a2))
                })
            } else {
                MatchType::Apart
            }
        }

        _ => MatchType::Apart,
    }
}

/// Determines if two types [`CanUnify`].
///
/// This is used in [`match_type`], where if two different types bind to the
/// same [`Variable::Bound`] variable, we determine if the types can actually
/// unify before generating an equality. This is effectively a pure version
/// of the [`unify`] function.
///
/// [`unify`]: crate::algorithm::unification::unify
fn can_unify(state: &mut CheckState, t1: TypeId, t2: TypeId) -> CanUnify {
    use CanUnify::*;

    let t1 = state.normalize_type(t1);
    let t2 = state.normalize_type(t2);

    if t1 == t2 {
        return Equal;
    }

    let t1_core = &state.storage[t1];
    let t2_core = &state.storage[t2];

    match (t1_core, t2_core) {
        (Type::Unification(_), _) | (_, Type::Unification(_)) => Unify,

        (Type::Unknown, _) | (_, Type::Unknown) => Unify,

        (Type::Row(_), Type::Row(_)) => Unify,

        (
            &Type::Application(t1_function, t1_argument),
            &Type::Application(t2_function, t2_argument),
        ) => can_unify(state, t1_function, t2_function)
            .and_also(|| can_unify(state, t1_argument, t2_argument)),

        (&Type::Function(t1_argument, t1_result), &Type::Function(t2_argument, t2_result)) => {
            can_unify(state, t1_argument, t2_argument)
                .and_also(|| can_unify(state, t1_result, t2_result))
        }

        // Function(a, b) and Application(Application(f, a), b) can
        // unify when `f` resolves to the Function constructor.
        (&Type::Function(..), &Type::Application(..))
        | (&Type::Application(..), &Type::Function(..)) => Unify,

        (
            &Type::KindApplication(t1_function, t1_argument),
            &Type::KindApplication(t2_function, t2_argument),
        ) => can_unify(state, t1_function, t2_function)
            .and_also(|| can_unify(state, t1_argument, t2_argument)),

        (&Type::Kinded(t1_type, t1_kind), &Type::Kinded(t2_type, t2_kind)) => {
            can_unify(state, t1_type, t2_type).and_also(|| can_unify(state, t1_kind, t2_kind))
        }

        (
            &Type::Constrained(t1_constraint, t1_body),
            &Type::Constrained(t2_constraint, t2_body),
        ) => can_unify(state, t1_constraint, t2_constraint)
            .and_also(|| can_unify(state, t1_body, t2_body)),

        (&Type::Forall(ref t1_binder, t1_body), &Type::Forall(ref t2_binder, t2_body)) => {
            can_unify(state, t1_binder.kind, t2_binder.kind)
                .and_also(|| can_unify(state, t1_body, t2_body))
        }

        (
            &Type::OperatorApplication(t1_file, t1_item, t1_left, t1_right),
            &Type::OperatorApplication(t2_file, t2_item, t2_left, t2_right),
        ) => {
            if t1_file == t2_file && t1_item == t2_item {
                can_unify(state, t1_left, t2_left).and_also(|| can_unify(state, t1_right, t2_right))
            } else {
                Apart
            }
        }

        (
            Type::SynonymApplication(_, t1_file, t1_item, t1_arguments),
            Type::SynonymApplication(_, t2_file, t2_item, t2_arguments),
        ) => {
            if t1_file == t2_file && t1_item == t2_item && t1_arguments.len() == t2_arguments.len()
            {
                let t1_arguments = Arc::clone(t1_arguments);
                let t2_arguments = Arc::clone(t2_arguments);
                iter::zip(t1_arguments.iter(), t2_arguments.iter()).fold(
                    Equal,
                    |result, (&t1_argument, &t2_argument)| {
                        result.and_also(|| can_unify(state, t1_argument, t2_argument))
                    },
                )
            } else {
                Apart
            }
        }

        (
            &Type::Variable(Variable::Bound(ref t1_name, t1_kind)),
            &Type::Variable(Variable::Bound(ref t2_name, t2_kind)),
        ) => {
            if t1_name == t2_name {
                can_unify(state, t1_kind, t2_kind)
            } else {
                Apart
            }
        }

        (
            &Type::Variable(Variable::Skolem(ref t1_name, t1_kind)),
            &Type::Variable(Variable::Skolem(ref t2_name, t2_kind)),
        ) => {
            if t1_name == t2_name {
                can_unify(state, t1_kind, t2_kind)
            } else {
                Apart
            }
        }

        _ => Apart,
    }
}

/// Matches a wanted constraint to an instance.
#[tracing::instrument(skip_all, name = "match_instance")]
fn match_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    instance: Instance,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    let ConstraintApplication { file_id, item_id, ref arguments } = *wanted;

    let mut bindings = FxHashMap::default();
    let mut equalities = vec![];
    let mut match_results = vec![];
    let mut stuck_positions = vec![];

    for (index, (wanted, (given, _))) in arguments.iter().zip(&instance.arguments).enumerate() {
        let given = transfer::localize(state, context, *given);
        let match_result =
            match_type(state, context, &mut bindings, &mut equalities, *wanted, given);

        if matches!(match_result, MatchType::Apart) {
            crate::trace_fields!(state, context, { ?wanted = wanted, ?given = given }, "apart");
            return Ok(MatchInstance::Apart);
        }

        if matches!(match_result, MatchType::Stuck) {
            stuck_positions.push(index);
        }

        match_results.push(match_result);
    }

    if !stuck_positions.is_empty() {
        if !can_determine_stuck(context, file_id, item_id, &match_results, &stuck_positions)? {
            crate::trace_fields!(state, context, { ?wanted = wanted }, "stuck");
            return Ok(MatchInstance::Stuck);
        }

        for &index in &stuck_positions {
            let (instance_type, _) = &instance.arguments[index];
            let instance_type = transfer::localize(state, context, *instance_type);
            let substituted = ApplyBindings::on(state, &bindings, instance_type);
            equalities.push((arguments[index], substituted));
        }
    }

    let mut argument_names = FxHashSet::default();
    for &(argument, _) in &instance.arguments {
        let localized = transfer::localize(state, context, argument);
        CollectBoundNames::on(state, localized, &mut argument_names);
    }

    let mut constraint_variables = FxHashMap::default();
    for &(constraint, _) in &instance.constraints {
        let localized = transfer::localize(state, context, constraint);
        CollectBoundVariables::on(state, localized, &mut constraint_variables);
    }

    for (name, kind) in constraint_variables {
        if !argument_names.contains(&name) && !bindings.contains_key(&name) {
            let unification = state.fresh_unification_kinded(kind);
            bindings.insert(name, unification);
        }
    }

    let constraints: Vec<_> = instance
        .constraints
        .iter()
        .map(|(constraint, _)| {
            let localized = transfer::localize(state, context, *constraint);
            ApplyBindings::on(state, &bindings, localized)
        })
        .collect();

    crate::trace_fields!(state, context, {
        ?constraints = constraints.len(),
        ?equalities = equalities.len()
    }, "match");

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
            wanted.arguments.iter().zip(&given.arguments).enumerate()
        {
            let match_result = match_given_type(state, context, wanted_argument, given_argument);

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
        let equalities = stuck_positions.iter().map(|&index| {
            let wanted = wanted.arguments[index];
            let given = given.arguments[index];
            (wanted, given)
        });

        let constraints = vec![];
        let equalities = equalities.collect_vec();

        return Ok(Some(MatchInstance::Match { constraints, equalities }));
    }

    Ok(None)
}

/// Matches a wanted constraint to compiler-solved instances.
fn match_compiler_instances<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    given: &[ConstraintApplication],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let ConstraintApplication { file_id, item_id, ref arguments } = *wanted;

    macro_rules! dispatch {
        (
            $($prim_ctx:expr => {
                $($item:ident => $handler:expr),* $(,)?
            }),*
            $(, ? $optional:expr => $opt_handler:expr)* $(,)?
        ) => {
            $(
                if file_id == $prim_ctx.file_id {
                    $(if item_id == $prim_ctx.$item { $handler } else)*
                    { None }
                } else
            )*
            $(if $optional == Some((file_id, item_id)) { $opt_handler } else)*
            { None }
        };
    }

    let match_instance = dispatch! {
        context.prim_int => {
            add => prim_int_add(state, arguments),
            mul => prim_int_mul(state, arguments),
            compare => prim_int_compare(state, context, arguments, given),
            to_string => prim_int_to_string(state, arguments),
        },
        context.prim_symbol => {
            append => prim_symbol_append(state, arguments),
            compare => prim_symbol_compare(state, context, arguments),
            cons => prim_symbol_cons(state, arguments),
        },
        context.prim_row => {
            union => prim_row_union(state, context, arguments)?,
            cons => prim_row_cons(state, context, arguments)?,
            lacks => prim_row_lacks(state, context, arguments)?,
            nub => prim_row_nub(state, context, arguments)?,
        },
        context.prim_row_list => {
            row_to_list => prim_rowlist_row_to_list(state, context, arguments),
        },
        context.prim_coerce => {
            coercible => prim_coercible(state, context, arguments)?,
        },
        context.prim_type_error => {
            warn => prim_warn(state, context, arguments),
            fail => prim_fail(state, context, arguments),
        },
        // These classes are defined in `prelude` and may be optional.
        ? context.known_reflectable.is_symbol => prim_is_symbol(state, arguments),
        ? context.known_reflectable.reflectable => prim_reflectable(state, context, arguments),
    };

    Ok(match_instance)
}

struct ApplyBindings<'a> {
    bindings: &'a FxHashMap<Name, TypeId>,
}

impl<'a> ApplyBindings<'a> {
    fn on(
        state: &mut CheckState,
        bindings: &'a FxHashMap<Name, TypeId>,
        type_id: TypeId,
    ) -> TypeId {
        fold_type(state, type_id, &mut ApplyBindings { bindings })
    }
}

impl TypeFold for ApplyBindings<'_> {
    fn transform(&mut self, _state: &mut CheckState, id: TypeId, t: &Type) -> FoldAction {
        match t {
            Type::Variable(Variable::Bound(name, _)) => {
                let id = self.bindings.get(name).copied().unwrap_or(id);
                FoldAction::Replace(id)
            }
            _ => FoldAction::Continue,
        }
    }
}

/// Collects all bound variable names from a type.
struct CollectBoundNames<'a> {
    names: &'a mut FxHashSet<Name>,
}

impl<'a> CollectBoundNames<'a> {
    fn on(state: &mut CheckState, type_id: TypeId, names: &'a mut FxHashSet<Name>) {
        visit_type(state, type_id, &mut CollectBoundNames { names });
    }
}

impl TypeVisitor for CollectBoundNames<'_> {
    fn visit(&mut self, _state: &mut CheckState, _id: TypeId, t: &Type) -> VisitAction {
        if let Type::Variable(Variable::Bound(name, _)) = t {
            self.names.insert(name.clone());
        }
        VisitAction::Continue
    }
}

/// Collects all bound variables with their kinds from a type.
struct CollectBoundVariables<'a> {
    variables: &'a mut FxHashMap<Name, TypeId>,
}

impl<'a> CollectBoundVariables<'a> {
    fn on(state: &mut CheckState, type_id: TypeId, variables: &'a mut FxHashMap<Name, TypeId>) {
        visit_type(state, type_id, &mut CollectBoundVariables { variables });
    }
}

impl TypeVisitor for CollectBoundVariables<'_> {
    fn visit(&mut self, _state: &mut CheckState, _id: TypeId, t: &Type) -> VisitAction {
        if let Type::Variable(Variable::Bound(name, kind)) = t {
            self.variables.insert(name.clone(), *kind);
        }
        VisitAction::Continue
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
    arguments: &[(TypeId, TypeId)],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let functional_dependencies = get_functional_dependencies(context, class_file, class_item)?;
    let all_determined = get_all_determined(&functional_dependencies);

    for (position, &(argument_type, _)) in arguments.iter().enumerate() {
        if all_determined.contains(&position) {
            continue;
        }
        if HasLabeledRole::on(state, argument_type) {
            let type_message = state.render_local_type(context, argument_type);
            state.insert_error(crate::error::ErrorKind::InstanceHeadLabeledRow {
                class_file,
                class_item,
                position,
                type_message,
            });
        }
    }

    Ok(())
}
