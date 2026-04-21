pub mod compiler;
pub mod elaborate;
pub mod given;
pub mod instances;

use std::collections::VecDeque;
use std::mem;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{KindOrType, Type, TypeId, normalise, toolkit, unification};
use crate::error::ErrorKind;
use crate::implication::ImplicationId;
use crate::state::CheckState;

use compiler::match_compiler_instances;
use elaborate::{elaborate_given, substitute_constraint, substitute_constraints};
use given::match_given_instances;
use instances::{collect_instance_chains, match_instance};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConstraintApplication {
    pub file_id: FileId,
    pub item_id: TypeItemId,
    pub arguments: Vec<KindOrType>,
}

impl ConstraintApplication {
    pub fn expect_type_arguments<const N: usize>(&self) -> Option<[TypeId; N]> {
        self.arguments
            .iter()
            .filter_map(|argument| match argument {
                KindOrType::Type(argument) => Some(*argument),
                KindOrType::Kind(_) => None,
            })
            .collect_array()
    }
}

#[derive(Debug, Clone)]
pub enum MatchInstance {
    Match { constraints: Vec<TypeId>, equalities: Vec<(TypeId, TypeId)> },
    Apart,
    Stuck,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchType {
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
    let (wanted, given, patterns, children) = {
        let node = &mut state.implications[implication];
        (
            mem::take(&mut node.wanted),
            mem::take(&mut node.given),
            mem::take(&mut node.patterns),
            node.children.clone(),
        )
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

    let has_given_partial = all_given.contains(&context.prim.partial);
    if !has_given_partial {
        for deferred in patterns {
            state.checked.errors.push(crate::error::CheckError {
                kind: ErrorKind::MissingPatterns { patterns: deferred.patterns },
                crumbs: deferred.crumbs,
            });
        }
    }

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
    let elaborate::ElaboratedGiven { given, substitution } =
        elaborate_given(state, context, given)?;

    let work_queue = wanted
        .into_iter()
        .map(|constraint| substitute_constraint(state, context, &substitution, constraint))
        .collect::<QueryResult<VecDeque<_>>>()?;

    let mut work_queue = work_queue;
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
                Some(MatchInstance::Stuck | MatchInstance::Apart) | None => {}
            }

            match match_compiler_instances(state, context, &application, &given)? {
                Some(MatchInstance::Match { constraints, equalities }) => {
                    for (t1, t2) in equalities {
                        if unification::unify(state, context, t1, t2)? {
                            made_progress = true;
                        }
                    }
                    let constraints =
                        substitute_constraints(state, context, &substitution, constraints)?;
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
                            let constraints =
                                substitute_constraints(state, context, &substitution, constraints)?;
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
    let (constructor, arguments) = toolkit::extract_all_applications(state, context, id)?;
    let constructor = normalise::expand(state, context, constructor)?;
    Ok(match context.lookup_type(constructor) {
        Type::Constructor(file_id, item_id) => {
            Some(ConstraintApplication { file_id, item_id, arguments })
        }
        _ => None,
    })
}
