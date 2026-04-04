//! Implements the constraint solver for PureScript.
//!
//! The [`solve_implication`] function is the entrypoint for solving the root
//! [`Implication`]; the [`solve_implication_id`] function solves a single
//! [`Implication`] with respect to inheritance; and the [`solve_constraints`]
//! function implements the equality-driven constraint solver.
//!
//! [`Implication`]: crate::implication::Implication

pub mod canonical;
pub mod elaborate;
pub mod matching;

pub use crate::core::constraint::fd;
use crate::core::constraint2::matching::{InstanceMatch, MatchInstance, match_given_instance};

pub use canonical::{CanonicalConstraint, CanonicalConstraintId, Canonicals};

use std::collections::VecDeque;
use std::{iter, mem};

use building_types::QueryResult;
use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::TypeId;
use crate::error::ErrorKind;
use crate::implication::{ImplicationId, Patterns};
use crate::state::CheckState;

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

pub fn solve_implication_id<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    implication: ImplicationId,
    inherited: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let (mut wanted, given, patterns, children) = {
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
    wanted: VecDeque<TypeId>,
    given: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let mut work = wanted
        .iter()
        .filter_map(|id| canonical::canonicalise(state, context, *id).transpose())
        .map(|id| id.map(WorkItem::Constraint))
        .collect::<QueryResult<VecDeque<_>>>()?;

    let given = given
        .iter()
        .filter_map(|id| canonical::canonicalise(state, context, *id).transpose())
        .collect::<QueryResult<Vec<_>>>()?;

    let given = elaborate::elaborate_given(state, context, &given)?;

    let mut stuck = Stuck::default();

    'work: while let Some(item) = work.pop_back() {
        match item {
            WorkItem::Constraint(wanted) => {
                match match_given_instance(state, context, wanted, &given)? {
                    MatchInstance::Match(InstanceMatch { goals }) => {
                        work.extend(goals);
                        continue 'work;
                    }
                    MatchInstance::Stuck(id) => {
                        for id in id {
                            stuck.entry(id).or_default().push(wanted);
                        }
                        continue 'work;
                    }
                    MatchInstance::Apart => (),
                }
            }
            WorkItem::Unify(_, _) => (),
        }
    }

    Ok(vec![])
}
