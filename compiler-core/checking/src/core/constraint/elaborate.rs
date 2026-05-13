//! Implements elabortion for given constraints.

pub mod improvements;

use std::collections::VecDeque;
use std::sync::Arc;

use building_types::QueryResult;
use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::context::CheckContext;
use crate::core::constraint::canonical::CanonicalConstraint;
use crate::core::constraint::matching::MatchInstance;
use crate::core::constraint::{CanonicalConstraintId, canonical, compiler};
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::walk::{TypeWalker, WalkAction, walk_type};
use crate::core::{CheckedClass, KindOrType, Name, Type, TypeId, normalise, toolkit, zonk};
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

pub struct ElaboratedGiven {
    pub given: Vec<CanonicalConstraintId>,
    pub substitution: NameToType,
}

pub fn elaborate_given_substitution<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[CanonicalConstraintId],
) -> QueryResult<NameToType>
where
    Q: ExternalQueries,
{
    let mut substitution = elaborate_given(state, context, given)?.substitution;
    let mut conflicts = FxHashSet::default();

    for &constraint_id in given {
        let constraint = state.canonicals[constraint_id].clone();
        let Some(class) =
            toolkit::lookup_file_class(state, context, constraint.file_id, constraint.type_id)?
        else {
            continue;
        };

        for dependency in class.functional_dependencies.iter() {
            let mut arguments = constraint.arguments.to_vec();
            let mut replacements = vec![];

            for &position in dependency.determined.iter() {
                let position = position as usize;
                let Some(KindOrType::Type(argument)) = arguments.get_mut(position) else {
                    continue;
                };

                let expanded = normalise::expand(state, context, *argument)?;
                let Type::Rigid(name, _, kind) = context.lookup_type(expanded) else {
                    continue;
                };

                let fresh = state.fresh_unification(context.queries, kind);
                *argument = fresh;
                replacements.push((name, fresh));
            }

            if replacements.is_empty() {
                continue;
            }

            let wanted = state.canonicals.intern(CanonicalConstraint {
                arguments: Arc::from(arguments),
                ..constraint.clone()
            });
            let wanted = VecDeque::from([wanted]);
            let error_count = state.checked.errors.len();
            super::solve_constraints(state, context, wanted, &[])?;
            state.checked.errors.truncate(error_count);

            for (name, fresh) in replacements {
                let replacement = zonk::zonk(state, context, fresh)?;
                register_improvement(
                    state,
                    context,
                    &mut substitution,
                    &mut conflicts,
                    name,
                    replacement,
                )?;
            }
        }
    }

    Ok(substitution)
}

/// Entrypoint for elaborating given [`CanonicalConstraint`].
pub fn elaborate_given<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[CanonicalConstraintId],
) -> QueryResult<ElaboratedGiven>
where
    Q: ExternalQueries,
{
    let given = elaborate_superclasses(state, context, given)?;
    let given = elaborate_coercible(state, context, given);
    let (given, substitution) = extract_compiler_solved(state, context, given)?;
    Ok(ElaboratedGiven { given, substitution })
}

/// Elaborates superclasses from a given [`CanonicalConstraint`].
pub fn elaborate_superclasses<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[CanonicalConstraintId],
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let mut elaborated = Vec::with_capacity(given.len());
    let mut pending = VecDeque::with_capacity(given.len());
    let mut seen = FxHashSet::default();

    for &given in given {
        if seen.insert(given) {
            elaborated.push(given);
            pending.push_back(given);
        }
    }

    while let Some(constraint) = pending.pop_front() {
        elaborate_via_superclass(
            state,
            context,
            constraint,
            &mut elaborated,
            &mut pending,
            &mut seen,
        )?;
    }

    Ok(elaborated)
}

fn elaborate_via_superclass<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraint: CanonicalConstraintId,
    constraints: &mut Vec<CanonicalConstraintId>,
    pending: &mut VecDeque<CanonicalConstraintId>,
    seen: &mut FxHashSet<CanonicalConstraintId>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CanonicalConstraint { file_id, type_id, .. } = state.canonicals[constraint];
    let Some(class) = toolkit::lookup_file_class(state, context, file_id, type_id)? else {
        return Ok(());
    };

    if class.superclasses.is_empty() {
        return Ok(());
    }

    let CanonicalConstraint { arguments, .. } = &state.canonicals[constraint];
    let Some(substitutions) = superclass_substitutions(context, &class, arguments)? else {
        return Ok(());
    };

    for superclass in class.superclasses {
        let superclass = SubstituteName::many(state, context, &substitutions, superclass)?;
        if let Some(superclass) = canonical::canonicalise(state, context, superclass)?
            && seen.insert(superclass)
        {
            constraints.push(superclass);
            pending.push_back(superclass);
        }
    }

    Ok(())
}

fn superclass_substitutions<Q>(
    context: &CheckContext<Q>,
    class: &CheckedClass,
    arguments: &[KindOrType],
) -> QueryResult<Option<NameToType>>
where
    Q: ExternalQueries,
{
    let mut bindings = NameToType::default();
    let mut arguments = arguments.iter().copied();

    for &binder_id in &class.kind_binders {
        let Some(KindOrType::Kind(argument)) = arguments.next() else {
            return Ok(None);
        };
        let binder = context.lookup_forall_binder(binder_id);
        bindings.insert(binder.name, argument);
    }

    for &binder_id in &class.type_parameters {
        let Some(KindOrType::Type(argument)) = arguments.next() else {
            return Ok(None);
        };
        let binder = context.lookup_forall_binder(binder_id);
        bindings.insert(binder.name, argument);
    }

    if arguments.next().is_some() {
        return Ok(None);
    }

    Ok(Some(bindings))
}

/// Elaborates useful [`CanonicalConstraint`] for `Coercible`.
pub fn elaborate_coercible<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut given: Vec<CanonicalConstraintId>,
) -> Vec<CanonicalConstraintId>
where
    Q: ExternalQueries,
{
    let symmetric = given.iter().filter_map(|given| {
        let CanonicalConstraint { file_id, type_id, ref arguments } = state.canonicals[*given];

        if (file_id, type_id) != (context.prim_coerce.file_id, context.prim_coerce.coercible) {
            return None;
        }

        let (kind @ KindOrType::Kind(_), left @ KindOrType::Type(_), right @ KindOrType::Type(_)) =
            arguments.iter().copied().collect_tuple()?
        else {
            return None;
        };

        let arguments = [kind, right, left].into();
        Some(state.canonicals.intern(CanonicalConstraint { file_id, type_id, arguments }))
    });

    let symmetric = symmetric.collect_vec();
    given.extend(symmetric);

    given
}

fn extract_compiler_solved<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: Vec<CanonicalConstraintId>,
) -> QueryResult<(Vec<CanonicalConstraintId>, NameToType)>
where
    Q: ExternalQueries,
{
    let mut substitution = NameToType::default();
    let mut conflicts = FxHashSet::default();

    safe_loop! {
        let given = canonical::substitute_canonicals(state, context, &substitution, &given)?;
        let mut changed = false;

        for &constraint in &given {
            let Some(matched) = compiler::match_compiler_instance(state, context, constraint, &given)?
            else {
                continue;
            };

            let MatchInstance::Match(instance) = matched else {
                continue;
            };

            for (left, right) in instance.unifications {
                let improvements = extract_improvements(state, context, &substitution, left, right)?;
                for (name, replacement) in improvements {
                    if register_improvement(
                        state,
                        context,
                        &mut substitution,
                        &mut conflicts,
                        name,
                        replacement,
                    )? {
                        changed = true;
                    }
                }
            }
        }

        if !changed {
            return Ok((given, substitution));
        }
    }
}

fn extract_improvements<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    substitution: &NameToType,
    left: TypeId,
    right: TypeId,
) -> QueryResult<Vec<(Name, TypeId)>>
where
    Q: ExternalQueries,
{
    let left = substitute_type(state, context, substitution, left)?;
    let right = substitute_type(state, context, substitution, right)?;

    let mut improvements = vec![];
    let mut seen = FxHashSet::default();
    improvements::collect_structural_improvements(
        state,
        context,
        left,
        right,
        &mut seen,
        &mut improvements,
    )?;

    Ok(improvements)
}

fn register_improvement<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    substitution: &mut NameToType,
    conflicts: &mut FxHashSet<Name>,
    name: Name,
    replacement: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    if conflicts.contains(&name) {
        return Ok(false);
    }

    let replacement = substitute_type(state, context, substitution, replacement)?;
    let replacement = normalise::expand(state, context, replacement)?;

    match context.lookup_type(replacement) {
        Type::Unification(_) | Type::Unknown(_) => return Ok(false),
        Type::Rigid(replacement, _, _) if replacement == name => return Ok(false),
        _ => {}
    }

    if contains_rigid(state, context, replacement, name)? {
        return Ok(false);
    }

    if let Some(current) = substitution.get(&name).copied() {
        let current = substitute_type(state, context, substitution, current)?;
        let current = normalise::expand(state, context, current)?;
        if current == replacement {
            return Ok(false);
        }

        substitution.remove(&name);
        conflicts.insert(name);
        return Ok(true);
    }

    substitution.insert(name, replacement);
    Ok(true)
}

fn substitute_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    substitution: &NameToType,
    mut type_id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if substitution.is_empty() {
        return Ok(type_id);
    }

    safe_loop! {
        let substituted = SubstituteName::many(state, context, substitution, type_id)?;
        if substituted == type_id {
            return Ok(type_id);
        }
        type_id = substituted;
    }
}

fn contains_rigid<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    target: Name,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    struct ContainsRigid {
        target: Name,
        contains: bool,
    }

    impl TypeWalker for ContainsRigid {
        fn visit<Q>(
            &mut self,
            _state: &mut CheckState,
            _context: &CheckContext<Q>,
            _id: TypeId,
            t: &Type,
        ) -> QueryResult<WalkAction>
        where
            Q: ExternalQueries,
        {
            if let Type::Rigid(name, _, _) = *t
                && name == self.target
            {
                self.contains = true;
                Ok(WalkAction::Stop)
            } else {
                Ok(WalkAction::Continue)
            }
        }
    }

    let mut walker = ContainsRigid { target, contains: false };
    walk_type(state, context, type_id, &mut walker)?;
    Ok(walker.contains)
}
