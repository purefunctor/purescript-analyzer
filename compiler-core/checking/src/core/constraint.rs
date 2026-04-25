//! Implements the constraint solver for PureScript.
//!
//! The [`solve_implication`] function is the entrypoint for solving the root
//! [`Implication`]; the [`solve_implication_id`] function solves a single
//! [`Implication`] with respect to inheritance; and the [`solve_constraints`]
//! function implements the equality-driven constraint solver.
//!
//! [`Implication`]: crate::implication::Implication

pub mod canonical;
pub mod compiler;
pub mod elaborate;
pub mod instances;
pub mod matching;

pub use canonical::{CanonicalConstraint, CanonicalConstraintId, Canonicals};

use matching::{InstanceMatch, MatchInstance, match_given_instance, match_instance_chain};

use std::collections::VecDeque;
use std::{iter, mem};

use building_types::QueryResult;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, unification};
use crate::error::ErrorKind;
use crate::implication::{ImplicationId, Patterns};
use crate::state::{CheckState, UnificationState};

pub fn solve_implication<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let implication = state.implications.current();
    solve_implication_id(state, context, implication, &[])
}

pub fn solve_implication_id<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    implication: ImplicationId,
    inherited: &[TypeId],
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let (wanted, given, patterns, children) = {
        let node = &mut state.implications[implication];
        (
            mem::take(&mut node.wanted),
            mem::take(&mut node.given),
            mem::take(&mut node.patterns),
            mem::take(&mut node.children),
        )
    };

    let inherited_given = {
        let inherited = inherited.iter();
        let given = given.iter();
        iter::chain(inherited, given).copied().collect_vec()
    };

    let mut wanted = wanted
        .iter()
        .filter_map(|id| canonical::canonicalise(state, context, *id).transpose())
        .collect::<QueryResult<VecDeque<_>>>()?;

    for child in &children {
        let residual = solve_implication_id(state, context, *child, &inherited_given)?;
        wanted.extend(residual);
    }

    let residual = solve_constraints(state, context, wanted, &inherited_given)?;
    let elide_missing_patterns = inherited_given.contains(&context.prim.partial);

    if !elide_missing_patterns {
        for Patterns { patterns, crumbs } in patterns {
            state.checked.errors.push(crate::error::CheckError {
                kind: ErrorKind::MissingPatterns { patterns },
                crumbs,
            });
        }
    }

    Ok(residual)
}

/// A unit of work in the constraint solver.
pub enum WorkItem {
    /// A constraint to be solved.
    Constraint(CanonicalConstraintId),
    /// A unification to be performed.
    Unify(TypeId, TypeId),
}

type Stuck = FxHashMap<u32, Vec<CanonicalConstraintId>>;

pub fn solve_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: VecDeque<CanonicalConstraintId>,
    given: &[TypeId],
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let given = given
        .iter()
        .filter_map(|id| canonical::canonicalise(state, context, *id).transpose())
        .collect::<QueryResult<Vec<_>>>()?;

    let elaborate::ElaboratedGiven { given, substitution } =
        elaborate::elaborate_given(state, context, &given)?;

    let wanted = wanted
        .into_iter()
        .map(|wanted| canonical::substitute_canonical(state, context, &substitution, wanted))
        .collect::<QueryResult<VecDeque<_>>>()?;

    let mut work = wanted.into_iter().map(WorkItem::Constraint).collect::<VecDeque<_>>();

    let mut stuck = Stuck::default();
    let mut residuals = vec![];

    'work: while let Some(item) = work.pop_back() {
        match item {
            WorkItem::Constraint(wanted) => {
                let mut blocked = FxHashSet::default();

                match match_given_instance(state, context, wanted, &given)? {
                    MatchInstance::Match(InstanceMatch { goals }) => {
                        work.extend(goals);
                        continue 'work;
                    }
                    MatchInstance::Stuck(id) => {
                        blocked.extend(id);
                    }
                    MatchInstance::Apart => (),
                }

                match compiler::match_compiler_instance(state, context, wanted, &given)? {
                    Some(MatchInstance::Match(InstanceMatch { goals })) => {
                        work.extend(goals);
                        continue 'work;
                    }
                    Some(MatchInstance::Stuck(id)) => {
                        blocked.extend(id);
                    }
                    Some(MatchInstance::Apart) | None => (),
                }

                let chains = instances::collect_instance_chains(state, context, wanted)?;
                'chain: for chain in chains {
                    match match_instance_chain(state, context, wanted, &chain)? {
                        MatchInstance::Match(InstanceMatch { goals }) => {
                            work.extend(goals);
                            continue 'work;
                        }
                        MatchInstance::Stuck(id) => {
                            blocked.extend(id);
                            continue 'chain;
                        }
                        MatchInstance::Apart => {
                            continue 'chain;
                        }
                    }
                }

                if blocked.is_empty() {
                    residuals.push(wanted);
                } else {
                    for id in blocked {
                        stuck.entry(id).or_default().push(wanted);
                    }
                }
            }
            WorkItem::Unify(t1, t2) => {
                if unification::unify(state, context, t1, t2)? {
                    let mut awake = FxHashSet::default();
                    stuck.retain(|&id, constraints| {
                        if let UnificationState::Solved(_) = state.unifications.get(id).state {
                            let constraints = constraints.iter().copied();
                            awake.extend(constraints);
                            false
                        } else {
                            true
                        }
                    });
                    for constraint in awake {
                        work.push_back(WorkItem::Constraint(constraint));
                    }
                }
            }
        }
    }

    for (_, constraints) in stuck {
        residuals.extend(constraints);
    }

    Ok(residuals)
}
