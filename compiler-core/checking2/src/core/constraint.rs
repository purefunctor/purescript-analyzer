pub mod compiler;
pub mod fd;
pub mod given;
pub mod instances;

use std::collections::{HashSet, VecDeque};
use std::mem;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::{Type, TypeId, normalise, toolkit, unification};
use crate::implication::ImplicationId;
use crate::state::CheckState;

use compiler::match_compiler_instances;
use given::{elaborate_given, match_given_instances};
use instances::{collect_instance_chains, match_instance};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConstraintApplication {
    pub file_id: FileId,
    pub item_id: TypeItemId,
    pub arguments: Vec<TypeId>,
}

#[derive(Debug, Clone)]
pub enum MatchInstance {
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
                Some(MatchInstance::Apart) | None => {}
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
