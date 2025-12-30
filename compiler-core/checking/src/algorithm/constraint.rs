mod functional_dependency;
use functional_dependency::Fd;

use std::collections::{HashSet, VecDeque};
use std::{iter, mem};

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::algorithm::fold::{FoldAction, TypeFold, fold_type};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{transfer, unification};
use crate::core::{ClassInfo, Instance, Variable, debruijn};
use crate::{ExternalQueries, Type, TypeId};

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
}

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

        (&Type::Constructor(f1, t1), &Type::Constructor(f2, t2)) => {
            if f1 == f2 && t1 == t2 {
                Equal
            } else {
                Apart
            }
        }

        (&Type::Operator(f1, t1), &Type::Operator(f2, t2)) => {
            if f1 == f2 && t1 == t2 {
                Equal
            } else {
                Apart
            }
        }

        (Type::Integer(n1), Type::Integer(n2)) => {
            if n1 == n2 {
                Equal
            } else {
                Apart
            }
        }

        (Type::String(k1, s1), Type::String(k2, s2)) => {
            if k1 == k2 && s1 == s2 {
                Equal
            } else {
                Apart
            }
        }

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

        (
            Type::SynonymApplication(_, f1, t1, args1),
            Type::SynonymApplication(_, f2, t2, args2),
        ) => {
            if f1 == f2 && t1 == t2 && args1.len() == args2.len() {
                let args1 = args1.clone();
                let args2 = args2.clone();
                iter::zip(args1.iter(), args2.iter())
                    .fold(Equal, |result, (&a1, &a2)| result.and_also(|| can_unify(state, a1, a2)))
            } else {
                Apart
            }
        }

        (Type::Row(_), Type::Row(_)) => Unify,

        (Type::Variable(v1), Type::Variable(v2)) => {
            if v1 == v2 {
                Equal
            } else {
                Apart
            }
        }

        _ => Apart,
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
            match match_given(state, context, wanted, &given)? {
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

            let Some(ConstraintApplication { file_id, item_id, arguments }) =
                constraint_application(state, wanted)
            else {
                residual.push(wanted);
                continue;
            };

            let instance_chains = collect_instances(state, context, file_id, item_id)?;

            for chain in instance_chains {
                'chain: for instance in chain {
                    match match_instance(state, context, file_id, item_id, &arguments, instance)? {
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

fn elaborate_given<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut given: Vec<TypeId>,
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let initial_given = mem::take(&mut given);

    let mut given = vec![];
    for constraint in initial_given {
        given.push(constraint);
        elaborate_superclasses(state, context, constraint, &mut given)?;
    }

    Ok(given)
}

fn collect_instances<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<Vec<Vec<Instance>>>
where
    Q: ExternalQueries,
{
    let instances: Vec<Instance> = if file_id == context.id {
        state
            .checked
            .instances
            .iter()
            .filter(|instance| instance.resolution.1 == item_id)
            .cloned()
            .collect_vec()
    } else {
        let checked = context.queries.checked(file_id)?;
        checked
            .instances
            .iter()
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

struct ConstraintApplication {
    file_id: FileId,
    item_id: TypeItemId,
    arguments: Vec<TypeId>,
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

fn get_class_info<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<Option<ClassInfo>>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        Ok(state.checked.classes.get(&item_id).cloned())
    } else {
        let checked = context.queries.checked(file_id)?;
        Ok(checked.classes.get(&item_id).cloned())
    }
}

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
        let Some(ConstraintApplication { file_id, item_id, arguments }) =
            constraint_application(state, constraint)
        else {
            return Ok(());
        };

        if !seen.insert((file_id, item_id)) {
            return Ok(());
        }

        let Some(class_info) = get_class_info(state, context, file_id, item_id)? else {
            return Ok(());
        };

        if class_info.superclasses.is_empty() {
            return Ok(());
        }

        let mut bindings = FxHashMap::default();
        for (&level, &argument) in class_info.variable_levels.iter().zip(&arguments) {
            bindings.insert(level, argument);
        }

        for &(superclass, _kind) in &class_info.superclasses {
            let localized = transfer::localize(state, context, superclass);
            let substituted = ApplyBindings::on(state, &bindings, localized);
            constraints.push(substituted);
            aux(state, context, substituted, constraints, seen)?;
        }

        Ok(())
    }

    let mut seen = HashSet::new();
    aux(state, context, constraint, constraints, &mut seen)
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

fn unstuck<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
    match_results: &[MatchType],
    stuck_positions: &[usize],
) -> QueryResult<Option<HashSet<usize>>>
where
    Q: ExternalQueries,
{
    if stuck_positions.is_empty() {
        return Ok(Some(HashSet::new()));
    }

    let functional_dependencies = get_functional_dependencies(context, file_id, item_id)?;
    let determined_positions = compute_match_closures(&functional_dependencies, match_results);

    let mut resolved = HashSet::new();
    for &stuck_index in stuck_positions {
        if !determined_positions.contains(&stuck_index) {
            return Ok(None);
        }
        resolved.insert(stuck_index);
    }

    Ok(Some(resolved))
}

fn match_given<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: TypeId,
    given: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let Some(wanted_application) = constraint_application(state, wanted) else {
        return Ok(None);
    };

    'given: for &given in given {
        let Some(given_application) = constraint_application(state, given) else {
            continue;
        };

        if wanted_application.file_id != given_application.file_id
            || wanted_application.item_id != given_application.item_id
        {
            continue;
        }

        if wanted_application.arguments.len() != given_application.arguments.len() {
            continue;
        }

        let mut match_results = Vec::with_capacity(wanted_application.arguments.len());
        let mut stuck_positions = vec![];

        for (index, (&wanted_argument, &given_argument)) in
            wanted_application.arguments.iter().zip(&given_application.arguments).enumerate()
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

        let Some(unstuck) = unstuck(
            context,
            wanted_application.file_id,
            wanted_application.item_id,
            &match_results,
            &stuck_positions,
        )?
        else {
            continue 'given;
        };

        if unstuck.len() < stuck_positions.len() {
            continue 'given;
        }

        let equalities = stuck_positions.iter().map(|&index| {
            let wanted = wanted_application.arguments[index];
            let given = given_application.arguments[index];
            (wanted, given)
        });

        let constraints = vec![];
        let equalities = equalities.collect_vec();

        return Ok(Some(MatchInstance::Match { constraints, equalities }));
    }

    Ok(None)
}

fn match_given_type(state: &mut CheckState, wanted: TypeId, given: TypeId) -> MatchType {
    let wanted = state.normalize_type(wanted);
    let given = state.normalize_type(given);

    let wanted_core = &state.storage[wanted];
    let given_core = &state.storage[given];

    match (wanted_core, given_core) {
        (Type::Variable(Variable::Bound(w_level)), Type::Variable(Variable::Bound(g_level)))
        | (
            Type::Variable(Variable::Skolem(w_level, _)),
            Type::Variable(Variable::Skolem(g_level, _)),
        ) => {
            if w_level == g_level {
                MatchType::Match
            } else {
                MatchType::Apart
            }
        }

        (Type::Unification(_), _) => MatchType::Stuck,

        (&Type::Constructor(w_file_id, w_type_id), &Type::Constructor(g_file_id, g_type_id)) => {
            if w_file_id == g_file_id && w_type_id == g_type_id {
                MatchType::Match
            } else {
                MatchType::Apart
            }
        }

        (
            &Type::Application(w_function, w_argument),
            &Type::Application(g_function, g_argument),
        ) => match_given_type(state, w_function, g_function)
            .and_also(|| match_given_type(state, w_argument, g_argument)),

        (&Type::Function(w_argument, w_result), &Type::Function(g_argument, g_result)) => {
            match_given_type(state, w_argument, g_argument)
                .and_also(|| match_given_type(state, w_result, g_result))
        }

        _ => MatchType::Apart,
    }
}

fn constraint_application(state: &mut CheckState, id: TypeId) -> Option<ConstraintApplication> {
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

fn match_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
    arguments: &[TypeId],
    instance: Instance,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
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
        let Some(unstuck) = unstuck(context, file_id, item_id, &match_results, &stuck_positions)?
        else {
            return Ok(MatchInstance::Stuck);
        };

        if unstuck.len() < stuck_positions.len() {
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

#[derive(Debug, Clone, Copy)]
enum MatchType {
    Match,
    Apart,
    Stuck,
}

impl MatchType {
    fn and_also(self, f: impl FnOnce() -> MatchType) -> MatchType {
        match self {
            MatchType::Match => f(),
            other => other,
        }
    }
}

#[derive(Debug, Clone)]
pub enum MatchInstance {
    Match { constraints: Vec<TypeId>, equalities: Vec<(TypeId, TypeId)> },
    Apart,
    Stuck,
}

fn match_type(
    state: &mut CheckState,
    bindings: &mut FxHashMap<debruijn::Level, TypeId>,
    equalities: &mut Vec<(TypeId, TypeId)>,
    wanted: TypeId,
    given: TypeId,
) -> MatchType {
    let wanted = state.normalize_type(wanted);
    let given = state.normalize_type(given);

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

        (&Type::Constructor(w_file_id, w_type_id), &Type::Constructor(g_file_id, g_type_id)) => {
            if w_file_id == g_file_id && w_type_id == g_type_id {
                MatchType::Match
            } else {
                MatchType::Apart
            }
        }

        (
            &Type::Application(w_function, w_argument),
            &Type::Application(g_function, g_argument),
        ) => match_type(state, bindings, equalities, w_function, g_function)
            .and_also(|| match_type(state, bindings, equalities, w_argument, g_argument)),

        (&Type::Function(w_argument, w_result), &Type::Function(g_argument, g_result)) => {
            match_type(state, bindings, equalities, w_argument, g_argument)
                .and_also(|| match_type(state, bindings, equalities, w_result, g_result))
        }

        _ => MatchType::Apart,
    }
}
