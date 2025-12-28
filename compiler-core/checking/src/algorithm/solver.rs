use std::collections::{HashSet, VecDeque};

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::algorithm::fd::{self, FunDep};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::transfer;
use crate::algorithm::unification;
use crate::core::{Instance, Variable, debruijn};
use crate::error::ErrorKind;
use crate::{ExternalQueries, Type, TypeId};

fn apply_bindings(
    state: &mut CheckState,
    bindings: &FxHashMap<debruijn::Level, TypeId>,
    type_id: TypeId,
) -> TypeId {
    let type_id = state.normalize_type(type_id);
    match &state.storage[type_id] {
        Type::Variable(Variable::Implicit(level) | Variable::Bound(level)) => {
            bindings.get(level).copied().unwrap_or(type_id)
        }
        &Type::Application(function, argument) => {
            let function = apply_bindings(state, bindings, function);
            let argument = apply_bindings(state, bindings, argument);
            state.storage.intern(Type::Application(function, argument))
        }
        &Type::Function(argument, result) => {
            let argument = apply_bindings(state, bindings, argument);
            let result = apply_bindings(state, bindings, result);
            state.storage.intern(Type::Function(argument, result))
        }
        _ => type_id,
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
    // Constraint solving todo:
    //
    // 1. Take wanted -> (TypeId, Vec<TypeId>)
    // 2. TypeId -> Constructor(FileId, TypeItemId)
    // 3. Search arguments for all constructors, these files are to look for instances in
    //    We can do this some time later, let's just focus on constraint solving within
    //    this file for now.
    //
    // From the constructor, we can look up instances in the local file that we've saved
    // earlier in the CheckedModule::instances field, simply filter by the `resolution`
    //
    // Time to match instance heads! Matching is like unification that doesn't generate
    // unification constraints, but instead substitutions for type variables. If the
    // instance head has a type variable, it matches with anything. Further uses of
    // that type variable will be replaced with the type it originally matched with.
    // If the wanted head has a unification variable that ends up matching with another
    // type through this mechanism, we make sure to generate an 'improvement' i.e. please
    // unify these types for me so the next solving turn there's no more constraints kthxbye
    //
    // When matching an instance head, consider that we also need to apply functional dependency
    // closure checking, especially to determine if a unification variable completely stalls
    // matching for that constraint or if it can be improved.

    let mut work_queue = wanted;

    'outer: while let Some(wanted) = work_queue.pop_front() {
        if match_given(state, wanted, &given).is_some() {
            continue;
        }

        // Fall back to instance matching
        let Some(ConstraintApplication { file_id, item_id, arguments }) =
            constraint_application(state, wanted)
        else {
            let constraint = transfer::globalize(state, context, wanted);
            state.insert_error(ErrorKind::NoInstanceFound { constraint });
            continue;
        };

        let instances = collect_instances(state, context, file_id, item_id)?;

        for instance in instances {
            match match_instance(state, context, file_id, item_id, &arguments, instance)? {
                MatchInstance::Match { constraints } => {
                    work_queue.extend(constraints);
                    continue 'outer;
                }
                MatchInstance::Improve { improvements } => {
                    for (wanted_type, bound_type) in improvements {
                        unification::unify(state, context, wanted_type, bound_type)?;
                    }
                    work_queue.push_back(wanted);
                    continue 'outer;
                }
                MatchInstance::Apart | MatchInstance::Stuck => continue,
            }
        }

        // No instance matched
        let constraint = transfer::globalize(state, context, wanted);
        state.insert_error(ErrorKind::NoInstanceFound { constraint });
    }

    Ok(vec![])
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
) -> QueryResult<Vec<FunDep>>
where
    Q: ExternalQueries,
{
    fn extract_fundeps(type_item: Option<&lowering::TypeItemIr>) -> Vec<FunDep> {
        let Some(lowering::TypeItemIr::ClassGroup { class: Some(class), .. }) = type_item else {
            return vec![];
        };

        let class = class.functional_dependencies.iter().map(|functional_dependency| {
            FunDep::new(
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

fn compute_match_closures(fundeps: &[FunDep], match_results: &[MatchType]) -> HashSet<usize> {
    let initial: HashSet<usize> = match_results
        .iter()
        .enumerate()
        .filter(|(_, result)| matches!(result, MatchType::Match | MatchType::Improve))
        .map(|(index, _)| index)
        .collect();
    fd::compute_closure(fundeps, &initial)
}

fn match_given(state: &mut CheckState, wanted: TypeId, given: &[TypeId]) -> Option<MatchInstance> {
    let Some(wanted_application) = constraint_application(state, wanted) else {
        return None;
    };

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
            let substituted = apply_bindings(state, &bindings, instance_type);
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
