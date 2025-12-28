mod functional_dependency;
use functional_dependency::Fd;

use std::collections::{HashSet, VecDeque};
use std::mem;

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

    'work: while let Some(wanted) = work_queue.pop_front() {
        if match_given(state, wanted, &given).is_some() {
            continue;
        }

        let Some(ConstraintApplication { file_id, item_id, arguments }) =
            constraint_application(state, wanted)
        else {
            residual.push(wanted);
            continue;
        };

        let instances = collect_instances(state, context, file_id, item_id)?;

        for instance in instances {
            match match_instance(state, context, file_id, item_id, &arguments, instance)? {
                MatchInstance::Match { constraints } => {
                    work_queue.extend(constraints);
                    continue 'work;
                }
                MatchInstance::Improve { improvements } => {
                    for (wanted_type, bound_type) in improvements {
                        unification::unify(state, context, wanted_type, bound_type)?;
                    }
                    work_queue.push_back(wanted);
                    continue 'work;
                }
                MatchInstance::Apart | MatchInstance::Stuck => continue,
            }
        }

        residual.push(wanted);
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
) -> QueryResult<Vec<Instance>>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        let checked = &state.checked;
        let instances =
            checked.instances.iter().filter(|instance| instance.resolution.1 == item_id);
        Ok(instances.cloned().collect_vec())
    } else {
        let checked = context.queries.checked(file_id)?;
        let instances =
            checked.instances.iter().filter(|instance| instance.resolution.1 == item_id);
        Ok(instances.cloned().collect_vec())
    }
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

        // Build bindings: stored level -> provided argument
        let mut bindings = FxHashMap::default();
        for (&level, &argument) in class_info.variable_levels.iter().zip(&arguments) {
            bindings.insert(level, argument);
        }

        // Localize and apply bindings to each superclass
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
        if matches!(result, MatchType::Match | MatchType::Improve) { Some(index) } else { None }
    });

    let initial: HashSet<usize> = initial.collect();
    functional_dependency::compute_closure(functional_dependencies, &initial)
}

fn match_given(state: &mut CheckState, wanted: TypeId, given: &[TypeId]) -> Option<MatchInstance> {
    let wanted_application = constraint_application(state, wanted)?;

    for &given in given {
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

        let all_match = wanted_application.arguments.iter().zip(&given_application.arguments).all(
            |(&wanted, &given)| matches!(match_given_type(state, wanted, given), MatchType::Match),
        );

        if all_match {
            return Some(MatchInstance::Match { constraints: vec![] });
        }
    }

    None
}

fn match_given_type(state: &mut CheckState, wanted: TypeId, given: TypeId) -> MatchType {
    let wanted = state.normalize_type(wanted);
    let given = state.normalize_type(given);

    let wanted_core = &state.storage[wanted];
    let given_core = &state.storage[given];

    match (wanted_core, given_core) {
        (Type::Variable(Variable::Bound(w_level)), Type::Variable(Variable::Bound(g_level))) => {
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
    let mut improvements = vec![];
    let mut match_results = Vec::with_capacity(arguments.len());
    let mut stuck_positions = vec![];

    for (index, (wanted, (given, _))) in arguments.iter().zip(&instance.arguments).enumerate() {
        let given = transfer::localize(state, context, *given);
        let match_result = match_type(state, &mut bindings, &mut improvements, *wanted, given);

        if matches!(match_result, MatchType::Apart) {
            return Ok(MatchInstance::Apart);
        }

        if matches!(match_result, MatchType::Stuck) {
            stuck_positions.push(index);
        }

        match_results.push(match_result);
    }

    if !stuck_positions.is_empty() {
        let functional_dependencies = get_functional_dependencies(context, file_id, item_id)?;
        let determined_positions = compute_match_closures(&functional_dependencies, &match_results);

        for &stuck_index in &stuck_positions {
            if !determined_positions.contains(&stuck_index) {
                return Ok(MatchInstance::Stuck);
            }
            // Generate improvement for this fundep-determined stuck position
            let (instance_type, _) = &instance.arguments[stuck_index];
            let instance_type = transfer::localize(state, context, *instance_type);
            let substituted = ApplyBindings::on(state, &bindings, instance_type);
            improvements.push((arguments[stuck_index], substituted));
        }
    }

    if improvements.is_empty() {
        let constraints = instance
            .constraints
            .iter()
            .map(|(constraint, _)| transfer::localize(state, context, *constraint))
            .collect();
        Ok(MatchInstance::Match { constraints })
    } else {
        Ok(MatchInstance::Improve { improvements })
    }
}

#[derive(Debug, Clone, Copy)]
enum MatchType {
    /// Types are structurally equal
    Match,
    /// Instance variable was seen before, generates an improvement
    Improve,
    /// Types are structurally incompatible
    Apart,
    /// Wanted type is a unification variable
    Stuck,
}

impl MatchType {
    fn and_also(self, f: impl FnOnce() -> MatchType) -> MatchType {
        if matches!(self, MatchType::Match) { f() } else { self }
    }
}

#[derive(Debug, Clone)]
pub enum MatchInstance {
    /// Instance matches - return its constraints as new wanted
    Match { constraints: Vec<TypeId> },
    /// Needs unification first, then re-queue the original constraint
    Improve { improvements: Vec<(TypeId, TypeId)> },
    /// Instance definitely doesn't match
    Apart,
    /// Has unification variable not determinable via fundeps
    Stuck,
}

fn match_type(
    state: &mut CheckState,
    bindings: &mut FxHashMap<debruijn::Level, TypeId>,
    improvements: &mut Vec<(TypeId, TypeId)>,
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
                    // Same instance variable seen before
                    if wanted != bound {
                        // Need to improve - wanted differs from bound type
                        improvements.push((wanted, bound));
                        MatchType::Improve
                    } else {
                        // Same type - trivial match
                        MatchType::Match
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
        ) => match_type(state, bindings, improvements, w_function, g_function)
            .and_also(|| match_type(state, bindings, improvements, w_argument, g_argument)),

        (&Type::Function(w_argument, w_result), &Type::Function(g_argument, g_result)) => {
            match_type(state, bindings, improvements, w_argument, g_argument)
                .and_also(|| match_type(state, bindings, improvements, w_result, g_result))
        }

        _ => MatchType::Apart,
    }
}
