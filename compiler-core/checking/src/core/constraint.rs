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
pub mod matching2;

pub use canonical::{CanonicalConstraint, CanonicalConstraintId, Canonicals};

use matching::{InstanceMatch, MatchInstance};

use std::collections::VecDeque;
use std::{iter, mem};

use building_types::QueryResult;
use indexmap::IndexSet;
use itertools::Itertools;
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

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

pub struct WorkList {
    unifications: Vec<(TypeId, TypeId)>,
    constraints: VecDeque<CanonicalConstraintId>,
}

impl WorkList {
    pub fn extend_from_match(&mut self, instance: InstanceMatch) {
        self.unifications.extend(instance.unifications);
        self.constraints.extend(instance.constraints);
    }

    pub fn extend_from_parts(
        &mut self,
        unifications: Vec<(TypeId, TypeId)>,
        constraints: Vec<CanonicalConstraintId>,
    ) {
        self.unifications.extend(unifications);
        self.constraints.extend(constraints);
    }
}

type Stuck = FxHashMap<u32, Vec<CanonicalConstraintId>>;
type Awake = IndexSet<CanonicalConstraintId, FxBuildHasher>;
type Skolem = Vec<CanonicalConstraintId>;

fn wake_constraints(work: &mut WorkList, stuck: &mut Stuck, state: &CheckState) {
    let mut awake = Awake::default();

    stuck.retain(|&id, constraints| {
        if let UnificationState::Solved(_) = state.unifications.get(id).state {
            for &constraint in constraints.iter() {
                awake.insert(constraint);
            }
            false
        } else {
            true
        }
    });

    if awake.is_empty() {
        return;
    }

    // For each constraint in the constraint set;
    for constraints in stuck.values_mut() {
        // keep only the constraints that are not awake;
        constraints.retain(|constraint| !awake.contains(constraint));
    }

    // and keep only the non-empty constraint sets.
    stuck.retain(|_, constraints| !constraints.is_empty());

    work.constraints.extend(awake);
}

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

    let mut work = WorkList { unifications: vec![], constraints: wanted };
    let mut stuck = Stuck::default();
    let mut skolem = Skolem::default();
    let mut residuals = vec![];

    'work: loop {
        let mut has_unification = false;
        for (t1, t2) in mem::take(&mut work.unifications) {
            has_unification |= unification::unify(state, context, t1, t2)?;
        }

        if has_unification {
            wake_constraints(&mut work, &mut stuck, state);
            work.constraints.extend(mem::take(&mut skolem));
        }

        let Some(wanted) = work.constraints.pop_front() else {
            break 'work;
        };

        let mut blocked = FxHashSet::default();
        let mut blocked_on_skolem = false;

        for &provided in &given {
            match matching2::match_provided(state, context, wanted, provided)? {
                matching2::MatchInstance::Match { unifications, constraints } => {
                    work.extend_from_parts(unifications, constraints);
                    continue 'work;
                }
                matching2::MatchInstance::Stuck { stuck } => {
                    blocked.extend(stuck);
                }
                matching2::MatchInstance::Skolem => {
                    blocked_on_skolem = true;
                }
                matching2::MatchInstance::Apart => (),
            }
        }

        match compiler::match_compiler_instance(state, context, wanted, &given)? {
            Some(MatchInstance::Match(instance)) => {
                work.extend_from_match(instance);
                continue 'work;
            }
            Some(MatchInstance::Stuck(id)) => {
                blocked.extend(id);
            }
            Some(MatchInstance::Apart) | None => (),
        }

        let search = instances::collect_instance_chains(state, context, wanted)?;
        'chain: for chain in search.chains {
            for candidate in chain {
                match matching2::match_declared(state, context, wanted, candidate)? {
                    matching2::MatchInstance::Match { unifications, constraints } => {
                        work.extend_from_parts(unifications, constraints);
                        continue 'work;
                    }
                    matching2::MatchInstance::Stuck { stuck } => {
                        blocked.extend(stuck);
                        continue 'chain;
                    }
                    matching2::MatchInstance::Skolem => {
                        blocked_on_skolem = true;
                        continue 'chain;
                    }
                    matching2::MatchInstance::Apart => (),
                }
            }
        }

        // If no candidate matched, the candidate search itself may also be incomplete
        // due to unsolved unification variables; we will wait for them to be solved.
        blocked.extend(search.blocking);

        if blocked.is_empty() {
            if blocked_on_skolem {
                skolem.push(wanted);
            } else {
                residuals.push(wanted);
            }
        } else {
            for id in blocked {
                stuck.entry(id).or_default().push(wanted);
            }
        }
    }

    let mut unique_residuals = Awake::default();
    unique_residuals.extend(residuals);

    for (_, constraints) in stuck {
        unique_residuals.extend(constraints);
    }
    unique_residuals.extend(skolem);

    Ok(unique_residuals.into_iter().collect())
}

pub fn is_type_error<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraint: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let Some(canonical) = canonical::canonicalise(state, context, constraint)? else {
        return Ok(false);
    };

    let canonical = &state.canonicals[canonical];
    Ok(canonical.file_id == context.prim_type_error.file_id
        && (canonical.type_id == context.prim_type_error.warn
            || canonical.type_id == context.prim_type_error.fail))
}
