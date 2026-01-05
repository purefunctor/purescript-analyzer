/// Implements compiler-solved instances.
mod compiler_solved;

/// Implements functional dependencies.
mod functional_dependency;

use compiler_solved::*;
use functional_dependency::Fd;

use std::collections::{HashSet, VecDeque};
use std::iter;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::algorithm::fold::{FoldAction, TypeFold, fold_type};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{transfer, unification};
use crate::core::{Class, Instance, Variable, debruijn};
use crate::{CheckedModule, ExternalQueries, Type, TypeId};

pub fn solve_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: VecDeque<TypeId>,
    given: Vec<TypeId>,
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

            if let Some(result) = match_compiler_instances(state, context, &application) {
                match result {
                    MatchInstance::Match { constraints, equalities } => {
                        for (t1, t2) in equalities {
                            if unification::unify(state, context, t1, t2)? {
                                made_progress = true;
                            }
                        }
                        work_queue.extend(constraints);
                        continue 'work;
                    }
                    MatchInstance::Apart => (),
                    MatchInstance::Stuck => {
                        residual.push(wanted);
                        continue 'work;
                    }
                }
            }

            let instance_chains =
                collect_instance_chains(state, context, application.file_id, application.item_id)?;

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

    Ok(residual)
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct ConstraintApplication {
    pub(crate) file_id: FileId,
    pub(crate) item_id: TypeItemId,
    pub(crate) arguments: Vec<TypeId>,
}

pub(crate) fn constraint_application(
    state: &mut CheckState,
    id: TypeId,
) -> Option<ConstraintApplication> {
    let mut arguments = vec![];
    let mut current_id = id;
    loop {
        match state.storage[current_id] {
            Type::Application(function, argument) => {
                arguments.push(argument);
                current_id = state.normalize_type(function);
            }
            Type::KindApplication(function, _) => {
                current_id = state.normalize_type(function);
            }
            Type::Constructor(file_id, item_id) => {
                arguments.reverse();
                return Some(ConstraintApplication { file_id, item_id, arguments });
            }
            _ => {
                return None;
            }
        }
    }
}

/// Discovers implied constraints from given constraints.
fn elaborate_given<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: Vec<TypeId>,
) -> QueryResult<Vec<ConstraintApplication>>
where
    Q: ExternalQueries,
{
    let mut elaborated = vec![];

    for constraint in given {
        elaborated.push(constraint);
        elaborate_superclasses(state, context, constraint, &mut elaborated)?;
    }

    Ok(elaborated.into_iter().filter_map(|given| constraint_application(state, given)).collect())
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

        let initial_level = class_info.quantified_variables.0 + class_info.kind_variables.0;
        let mut bindings = FxHashMap::default();
        for (index, &argument) in arguments.iter().enumerate() {
            let level = debruijn::Level(initial_level + index as u32);
            bindings.insert(level, argument);
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

fn collect_instance_chains<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<Vec<Vec<Instance>>>
where
    Q: ExternalQueries,
{
    let instances = if file_id == context.id {
        state
            .checked
            .instances
            .values()
            .filter(|instance| instance.resolution.1 == item_id)
            .cloned()
            .collect_vec()
    } else {
        let checked = context.queries.checked(file_id)?;
        checked
            .instances
            .values()
            .filter(|instance| instance.resolution.1 == item_id)
            .cloned()
            .collect_vec()
    };

    let mut grouped: FxHashMap<_, Vec<_>> = FxHashMap::default();
    for instance in instances {
        grouped.entry(instance.chain_id).or_default().push(instance);
    }

    let mut result: Vec<Vec<_>> = grouped.into_values().collect();
    for chain in &mut result {
        chain.sort_by_key(|instance| instance.chain_position);
    }

    Ok(result)
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
    Class {
        superclasses: superclasses.collect(),
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
    if let Some(class) = state.binding_group.lookup_class(item_id) {
        Some(class)
    } else {
        let class = state.checked.lookup_class(item_id)?;
        Some(localize_class(state, context, &class))
    }
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
fn match_type(
    state: &mut CheckState,
    bindings: &mut FxHashMap<debruijn::Level, TypeId>,
    equalities: &mut Vec<(TypeId, TypeId)>,
    wanted: TypeId,
    given: TypeId,
) -> MatchType {
    let wanted = state.normalize_type(wanted);
    let given = state.normalize_type(given);

    if wanted == given {
        return MatchType::Match;
    }

    let wanted_core = &state.storage[wanted];
    let given_core = &state.storage[given];

    match (wanted_core, given_core) {
        (_, Type::Variable(variable)) => {
            if let Variable::Implicit(level) | Variable::Bound(level) = variable {
                if let Some(&bound) = bindings.get(level) {
                    match can_unify(state, wanted, bound) {
                        CanUnify::Equal => MatchType::Match,
                        CanUnify::Apart => MatchType::Apart,
                        CanUnify::Unify => {
                            equalities.push((wanted, bound));
                            MatchType::Match
                        }
                    }
                } else {
                    bindings.insert(*level, wanted);
                    MatchType::Match
                }
            } else {
                MatchType::Apart
            }
        }

        (Type::Unification(_), _) => MatchType::Stuck,

        (
            &Type::Application(w_function, w_argument),
            &Type::Application(g_function, g_argument),
        ) => match_type(state, bindings, equalities, w_function, g_function)
            .and_also(|| match_type(state, bindings, equalities, w_argument, g_argument)),

        (&Type::Function(w_argument, w_result), &Type::Function(g_argument, g_result)) => {
            match_type(state, bindings, equalities, w_argument, g_argument)
                .and_also(|| match_type(state, bindings, equalities, w_result, g_result))
        }

        (
            &Type::KindApplication(w_function, w_argument),
            &Type::KindApplication(g_function, g_argument),
        ) => match_type(state, bindings, equalities, w_function, g_function)
            .and_also(|| match_type(state, bindings, equalities, w_argument, g_argument)),

        (
            &Type::OperatorApplication(f1, t1, l1, r1),
            &Type::OperatorApplication(f2, t2, l2, r2),
        ) => {
            if f1 == f2 && t1 == t2 {
                match_type(state, bindings, equalities, l1, l2)
                    .and_also(|| match_type(state, bindings, equalities, r1, r2))
            } else {
                MatchType::Apart
            }
        }

        (Type::SynonymApplication(_, f1, t1, a1), Type::SynonymApplication(_, f2, t2, a2)) => {
            if f1 == f2 && t1 == t2 && a1.len() == a2.len() {
                let a1 = Arc::clone(a1);
                let a2 = Arc::clone(a2);
                iter::zip(a1.iter(), a2.iter()).fold(MatchType::Match, |result, (&a1, &a2)| {
                    result.and_also(|| match_type(state, bindings, equalities, a1, a2))
                })
            } else {
                MatchType::Apart
            }
        }

        _ => MatchType::Apart,
    }
}

/// Matches an argument from a wanted constraint to one from a given constraint.
///
/// This function is specialised for matching given constraints, like those
/// found in value signatures rather than top-level instance declarations;
/// unlike [`match_type`], this function does not build bindings or equalities
/// for [`Variable::Bound`] or [`Variable::Implicit`] variables.
fn match_given_type(state: &mut CheckState, wanted: TypeId, given: TypeId) -> MatchType {
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
            &Type::Application(w_function, w_argument),
            &Type::Application(g_function, g_argument),
        ) => match_given_type(state, w_function, g_function)
            .and_also(|| match_given_type(state, w_argument, g_argument)),

        (&Type::Function(w_argument, w_result), &Type::Function(g_argument, g_result)) => {
            match_given_type(state, w_argument, g_argument)
                .and_also(|| match_given_type(state, w_result, g_result))
        }

        (
            &Type::KindApplication(w_function, w_argument),
            &Type::KindApplication(g_function, g_argument),
        ) => match_given_type(state, w_function, g_function)
            .and_also(|| match_given_type(state, w_argument, g_argument)),

        (
            &Type::OperatorApplication(f1, t1, l1, r1),
            &Type::OperatorApplication(f2, t2, l2, r2),
        ) => {
            if f1 == f2 && t1 == t2 {
                match_given_type(state, l1, l2).and_also(|| match_given_type(state, r1, r2))
            } else {
                MatchType::Apart
            }
        }

        (Type::SynonymApplication(_, f1, t1, a1), Type::SynonymApplication(_, f2, t2, a2)) => {
            if f1 == f2 && t1 == t2 && a1.len() == a2.len() {
                let a1 = Arc::clone(a1);
                let a2 = Arc::clone(a2);
                iter::zip(a1.iter(), a2.iter()).fold(MatchType::Match, |result, (&a1, &a2)| {
                    result.and_also(|| match_given_type(state, a1, a2))
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
/// same [`Variable::Implicit`] or [`Variable::Bound`] variable, we determine
/// if the types can actually unify before generating an equality. This is
/// effectively a pure version of the [`unify`] function.
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

        (&Type::Application(f1, a1), &Type::Application(f2, a2)) => {
            can_unify(state, f1, f2).and_also(|| can_unify(state, a1, a2))
        }

        (&Type::Function(a1, r1), &Type::Function(a2, r2)) => {
            can_unify(state, a1, a2).and_also(|| can_unify(state, r1, r2))
        }

        (&Type::KindApplication(f1, a1), &Type::KindApplication(f2, a2)) => {
            can_unify(state, f1, f2).and_also(|| can_unify(state, a1, a2))
        }

        (&Type::Kinded(t1, k1), &Type::Kinded(t2, k2)) => {
            can_unify(state, t1, t2).and_also(|| can_unify(state, k1, k2))
        }

        (&Type::Constrained(c1, b1), &Type::Constrained(c2, b2)) => {
            can_unify(state, c1, c2).and_also(|| can_unify(state, b1, b2))
        }

        (&Type::Forall(_, b1), &Type::Forall(_, b2)) => can_unify(state, b1, b2),

        (
            &Type::OperatorApplication(f1, t1, l1, r1),
            &Type::OperatorApplication(f2, t2, l2, r2),
        ) => {
            if f1 == f2 && t1 == t2 {
                can_unify(state, l1, l2).and_also(|| can_unify(state, r1, r2))
            } else {
                Apart
            }
        }

        (Type::SynonymApplication(_, f1, t1, a1), Type::SynonymApplication(_, f2, t2, a2)) => {
            if f1 == f2 && t1 == t2 && a1.len() == a2.len() {
                let a1 = Arc::clone(a1);
                let a2 = Arc::clone(a2);
                iter::zip(a1.iter(), a2.iter())
                    .fold(Equal, |result, (&a1, &a2)| result.and_also(|| can_unify(state, a1, a2)))
            } else {
                Apart
            }
        }

        _ => Apart,
    }
}

/// Matches a wanted constraint to an instance.
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
        let match_result = match_type(state, &mut bindings, &mut equalities, *wanted, given);

        if matches!(match_result, MatchType::Apart) {
            return Ok(MatchInstance::Apart);
        }

        if matches!(match_result, MatchType::Stuck) {
            stuck_positions.push(index);
        }

        match_results.push(match_result);
    }

    if !stuck_positions.is_empty() {
        if !can_determine_stuck(context, file_id, item_id, &match_results, &stuck_positions)? {
            return Ok(MatchInstance::Stuck);
        }

        for &index in &stuck_positions {
            let (instance_type, _) = &instance.arguments[index];
            let instance_type = transfer::localize(state, context, *instance_type);
            let substituted = ApplyBindings::on(state, &bindings, instance_type);
            equalities.push((arguments[index], substituted));
        }
    }

    let constraints = instance
        .constraints
        .iter()
        .map(|(constraint, _)| transfer::localize(state, context, *constraint))
        .collect();

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

        let mut match_results = Vec::with_capacity(wanted.arguments.len());
        let mut stuck_positions = vec![];

        for (index, (&wanted_argument, &given_argument)) in
            wanted.arguments.iter().zip(&given.arguments).enumerate()
        {
            let match_result = match_given_type(state, wanted_argument, given_argument);

            if matches!(match_result, MatchType::Apart) {
                continue 'given;
            }

            if matches!(match_result, MatchType::Stuck) {
                stuck_positions.push(index);
            }

            match_results.push(match_result);
        }

        if stuck_positions.is_empty() {
            return Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![] }));
        }

        if !can_determine_stuck(
            context,
            wanted.file_id,
            wanted.item_id,
            &match_results,
            &stuck_positions,
        )? {
            continue 'given;
        }

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
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let ConstraintApplication { file_id, item_id, ref arguments } = *wanted;

    if file_id == context.prim_int.file_id {
        if item_id == context.prim_int.add {
            prim_int_add(state, arguments)
        } else if item_id == context.prim_int.mul {
            prim_int_mul(state, arguments)
        } else if item_id == context.prim_int.compare {
            prim_int_compare(state, context, arguments)
        } else if item_id == context.prim_int.to_string {
            prim_int_to_string(state, arguments)
        } else {
            None
        }
    } else if file_id == context.prim_symbol.file_id {
        if item_id == context.prim_symbol.append {
            prim_symbol_append(state, arguments)
        } else if item_id == context.prim_symbol.compare {
            prim_symbol_compare(state, context, arguments)
        } else if item_id == context.prim_symbol.cons {
            prim_symbol_cons(state, arguments)
        } else {
            None
        }
    } else if file_id == context.prim_row.file_id {
        if item_id == context.prim_row.union {
            prim_row_union(state, arguments)
        } else if item_id == context.prim_row.cons {
            prim_row_cons(state, arguments)
        } else if item_id == context.prim_row.lacks {
            prim_row_lacks(state, arguments)
        } else if item_id == context.prim_row.nub {
            prim_row_nub(state, arguments)
        } else {
            None
        }
    } else if file_id == context.prim_row_list.file_id {
        if item_id == context.prim_row_list.row_to_list {
            prim_rowlist_row_to_list(state, context, arguments)
        } else {
            None
        }
    } else {
        None
    }
}

struct ApplyBindings<'a> {
    bindings: &'a FxHashMap<debruijn::Level, TypeId>,
}

impl<'a> ApplyBindings<'a> {
    fn on(
        state: &mut CheckState,
        bindings: &'a FxHashMap<debruijn::Level, TypeId>,
        type_id: TypeId,
    ) -> TypeId {
        fold_type(state, type_id, &mut ApplyBindings { bindings })
    }
}

impl TypeFold for ApplyBindings<'_> {
    fn transform(&mut self, _state: &mut CheckState, id: TypeId, t: &Type) -> FoldAction {
        match t {
            Type::Variable(Variable::Implicit(level) | Variable::Bound(level)) => {
                let id = self.bindings.get(level).copied().unwrap_or(id);
                FoldAction::Replace(id)
            }
            _ => FoldAction::Continue,
        }
    }
}
