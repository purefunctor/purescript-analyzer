use std::collections::{HashSet, VecDeque};

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::algorithm::fd::{self, FunDep};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::transfer;
use crate::core::{Instance, Variable, debruijn, pretty};
use crate::{ExternalQueries, Type, TypeId};

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

    println!("=== Wanted ===");
    for wanted in wanted {
        let Some(ConstraintApplication { file_id, item_id, arguments }) =
            constraint_application(state, wanted)
        else {
            continue;
        };
        for instance in collect_instances(state, context, file_id, item_id)? {
            match_instance(state, context, file_id, item_id, &arguments, instance)?;
        }
    }

    println!("=== Given ===");
    for given in given {
        let given = pretty::print_local(state, context, given);
        println!("{given}");
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

fn compute_match_closures(fundeps: &[FunDep], match_results: &[MatchResult]) -> HashSet<usize> {
    let initial: HashSet<usize> = match_results
        .iter()
        .enumerate()
        .filter(|(_, result)| matches!(result, MatchResult::Match | MatchResult::Improve))
        .map(|(index, _)| index)
        .collect();
    fd::compute_closure(fundeps, &initial)
}

fn adjust_match_by_closure(
    match_results: &[MatchResult],
    determined_positions: &HashSet<usize>,
) -> Vec<MatchResult> {
    match_results
        .iter()
        .enumerate()
        .map(|(idx, result)| {
            if matches!(result, MatchResult::Stuck) && determined_positions.contains(&idx) {
                MatchResult::Match
            } else {
                *result
            }
        })
        .collect()
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
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let mut bindings = FxHashMap::default();
    let mut improvement = vec![];

    let match_results = arguments.iter().zip(instance.arguments).map(|(wanted, (given, _))| {
        let given = transfer::localize(state, context, given);
        match_type(state, &mut bindings, &mut improvement, *wanted, given)
    });

    let match_results = match_results.collect_vec();
    let functional_dependencies = get_functional_dependencies(context, file_id, item_id)?;

    let determined_positions = compute_match_closures(&functional_dependencies, &match_results);
    let adjusted_results = adjust_match_by_closure(&match_results, &determined_positions);

    println!("Match results: {match_results:?}");
    println!("Determined positions: {determined_positions:?}");
    println!("Adjusted results: {adjusted_results:?}");

    Ok(())
}

#[derive(Debug, Clone, Copy)]
enum MatchResult {
    Match,
    Improve,
    Apart,
    Stuck,
}

impl MatchResult {
    fn and_also(self, f: impl FnOnce() -> MatchResult) -> MatchResult {
        if matches!(self, MatchResult::Match) { f() } else { self }
    }
}

fn match_type(
    state: &mut CheckState,
    bindings: &mut FxHashMap<debruijn::Level, TypeId>,
    improvements: &mut Vec<(TypeId, TypeId)>,
    wanted: TypeId,
    given: TypeId,
) -> MatchResult {
    let wanted = state.normalize_type(wanted);
    let given = state.normalize_type(given);

    let wanted_core = &state.storage[wanted];
    let given_core = &state.storage[given];

    match (wanted_core, given_core) {
        (_, Type::Variable(variable)) => {
            if let Variable::Implicit(level) = variable {
                bindings.insert(*level, wanted);
                MatchResult::Match
            } else if let Variable::Bound(level) = variable
                && let Some(bound) = bindings.get(level)
            {
                improvements.push((wanted, *bound));
                MatchResult::Improve
            } else {
                MatchResult::Apart
            }
        }

        (Type::Unification(_), _) => MatchResult::Stuck,

        (&Type::Constructor(w_file_id, w_type_id), &Type::Constructor(g_file_id, g_type_id)) => {
            if w_file_id == g_file_id && w_type_id == g_type_id {
                MatchResult::Match
            } else {
                MatchResult::Apart
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

        _ => MatchResult::Apart,
    }
}
