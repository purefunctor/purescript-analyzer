use std::collections::HashSet;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use rustc_hash::FxHashSet;

use crate::context::CheckContext;
use crate::core::constraint::compiler;
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::walk::{TypeWalker, WalkAction, walk_type};
use crate::core::{KindOrType, Name, Type, TypeId, normalise, toolkit};
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

use super::{ConstraintApplication, MatchInstance};

mod improvements;

pub struct ElaboratedGiven {
    pub given: Vec<ConstraintApplication>,
    pub substitution: NameToType,
}

pub fn elaborate_given<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[TypeId],
) -> QueryResult<ElaboratedGiven>
where
    Q: ExternalQueries,
{
    let given = elaborate_given_superclasses(state, context, given)?;
    let given = extract_given_applications(state, context, given)?;
    let given = extract_coercible_symmetry(context, given);
    let (given, substitution) = extract_compiler_solved(state, context, given)?;
    Ok(ElaboratedGiven { given, substitution })
}

fn elaborate_given_superclasses<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let mut elaborated = vec![];

    for &constraint in given {
        elaborated.push(constraint);
        elaborate_superclasses(state, context, constraint, &mut elaborated)?;
    }

    Ok(elaborated)
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
        let Some(application) = super::constraint_application(state, context, constraint)? else {
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
        let mut arguments = application.arguments.iter().copied();

        for &binder_id in &class_info.kind_binders {
            let Some(KindOrType::Kind(argument)) = arguments.next() else {
                return Ok(());
            };
            let binder = context.lookup_forall_binder(binder_id);
            bindings.insert(binder.name, argument);
        }

        for &binder_id in &class_info.type_parameters {
            let Some(KindOrType::Type(argument)) = arguments.next() else {
                return Ok(());
            };
            let binder = context.lookup_forall_binder(binder_id);
            bindings.insert(binder.name, argument);
        }

        if arguments.next().is_some() {
            return Ok(());
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

fn extract_given_applications<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constraints: Vec<TypeId>,
) -> QueryResult<Vec<ConstraintApplication>>
where
    Q: ExternalQueries,
{
    constraints
        .into_iter()
        .map(|constraint| super::constraint_application(state, context, constraint))
        .filter_map_ok(|constraint| constraint)
        .collect()
}

fn extract_coercible_symmetry<Q>(
    context: &CheckContext<Q>,
    mut applications: Vec<ConstraintApplication>,
) -> Vec<ConstraintApplication>
where
    Q: ExternalQueries,
{
    let symmetric = applications.iter().filter_map(|application| {
        if application.file_id != context.prim_coerce.file_id
            || application.item_id != context.prim_coerce.coercible
        {
            return None;
        }

        let (kind @ KindOrType::Kind(_), left @ KindOrType::Type(_), right @ KindOrType::Type(_)) =
            application.arguments.iter().copied().collect_tuple()?
        else {
            return None;
        };

        Some(ConstraintApplication {
            file_id: application.file_id,
            item_id: application.item_id,
            arguments: vec![kind, right, left],
        })
    });

    let symmetric = symmetric.collect_vec();
    applications.extend(symmetric);

    applications
}

fn extract_compiler_solved<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    applications: Vec<ConstraintApplication>,
) -> QueryResult<(Vec<ConstraintApplication>, NameToType)>
where
    Q: ExternalQueries,
{
    let mut substitution = NameToType::default();
    let mut conflicts = FxHashSet::default();

    safe_loop! {
        let substituted = applications
            .iter()
            .map(|application| substitute_application(state, context, &substitution, application))
            .collect::<QueryResult<Vec<_>>>()?;

        let mut changed = false;

        for application in &substituted {
            let Some(matched) =
                elaborate_compiler_instances(state, context, application, &substituted)?
            else {
                continue;
            };

            let MatchInstance::Match { equalities, .. } = matched else {
                continue;
            };

            for (left, right) in equalities {
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
            return Ok((substituted, substitution));
        }
    }
}

fn elaborate_compiler_instances<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    given: &[ConstraintApplication],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let ConstraintApplication { file_id, item_id, .. } = wanted;

    let match_instance = if *file_id == context.prim_int.file_id {
        if *item_id == context.prim_int.add {
            let Some(arguments) = wanted.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            compiler::prim_int::match_add(state, context, &arguments)?
        } else if *item_id == context.prim_int.mul {
            let Some(arguments) = wanted.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            compiler::prim_int::match_mul(state, context, &arguments)?
        } else if *item_id == context.prim_int.compare {
            let Some(arguments) = wanted.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            compiler::prim_int::match_compare(state, context, &arguments, given)?
        } else if *item_id == context.prim_int.to_string {
            let Some(arguments) = wanted.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            compiler::prim_int::match_to_string(state, context, &arguments)?
        } else {
            None
        }
    } else if *file_id == context.prim_symbol.file_id {
        if *item_id == context.prim_symbol.append {
            let Some(arguments) = wanted.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            compiler::prim_symbol::match_append(state, context, &arguments)?
        } else if *item_id == context.prim_symbol.compare {
            let Some(arguments) = wanted.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            compiler::prim_symbol::match_compare(state, context, &arguments)?
        } else if *item_id == context.prim_symbol.cons {
            let Some(arguments) = wanted.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            compiler::prim_symbol::match_cons(state, context, &arguments)?
        } else {
            None
        }
    } else if *file_id == context.prim_row.file_id {
        if *item_id == context.prim_row.union {
            let Some(arguments) = wanted.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            compiler::prim_row::match_union(state, context, &arguments)?
        } else if *item_id == context.prim_row.cons {
            let Some(arguments) = wanted.expect_type_arguments::<4>() else {
                return Ok(None);
            };
            compiler::prim_row::match_cons(state, context, &arguments)?
        } else if *item_id == context.prim_row.lacks {
            let Some(arguments) = wanted.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            compiler::prim_row::match_lacks(state, context, &arguments)?
        } else if *item_id == context.prim_row.nub {
            let Some(arguments) = wanted.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            compiler::prim_row::match_nub(state, context, &arguments)?
        } else {
            None
        }
    } else if *file_id == context.prim_row_list.file_id {
        if *item_id == context.prim_row_list.row_to_list {
            let Some(arguments) = wanted.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            compiler::prim_row_list::match_row_to_list(state, context, &arguments)?
        } else {
            None
        }
    } else if *file_id == context.prim_coerce.file_id {
        if *item_id == context.prim_coerce.coercible {
            let Some(arguments) = wanted.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            compiler::prim_coerce::match_coercible(state, context, &arguments)?
        } else {
            None
        }
    } else if context.known_reflectable.is_symbol == Some((*file_id, *item_id)) {
        let Some(arguments) = wanted.expect_type_arguments::<1>() else {
            return Ok(None);
        };
        compiler::prim_symbol::match_is_symbol(state, context, &arguments)?
    } else if context.known_reflectable.reflectable == Some((*file_id, *item_id)) {
        let Some(arguments) = wanted.expect_type_arguments::<2>() else {
            return Ok(None);
        };
        compiler::prim_reflectable::match_reflectable(state, context, &arguments)?
    } else {
        None
    };

    Ok(match_instance)
}

pub fn substitute_constraint<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    substitution: &NameToType,
    mut constraint: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    if substitution.is_empty() {
        return Ok(constraint);
    }
    safe_loop! {
        let substituted = SubstituteName::many(state, context, substitution, constraint)?;
        if substituted == constraint {
            return Ok(constraint);
        }
        constraint = substituted;
    }
}

pub fn substitute_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    substitution: &NameToType,
    constraints: Vec<TypeId>,
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    constraints
        .into_iter()
        .map(|constraint| substitute_constraint(state, context, substitution, constraint))
        .collect()
}

fn substitute_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    substitution: &NameToType,
    application: &ConstraintApplication,
) -> QueryResult<ConstraintApplication>
where
    Q: ExternalQueries,
{
    let substitute_argument = |argument| match argument {
        KindOrType::Kind(argument) => {
            substitute_constraint(state, context, substitution, argument).map(KindOrType::Kind)
        }
        KindOrType::Type(argument) => {
            substitute_constraint(state, context, substitution, argument).map(KindOrType::Type)
        }
    };

    let arguments = application
        .arguments
        .iter()
        .copied()
        .map(substitute_argument)
        .collect::<QueryResult<Vec<_>>>()?;

    Ok(ConstraintApplication {
        file_id: application.file_id,
        item_id: application.item_id,
        arguments,
    })
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
    let left = substitute_constraint(state, context, substitution, left)?;
    let right = substitute_constraint(state, context, substitution, right)?;

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

    let replacement = substitute_constraint(state, context, substitution, replacement)?;
    let replacement = normalise::expand(state, context, replacement)?;

    match context.lookup_type(replacement) {
        Type::Unification(_) | Type::Unknown(_) => return Ok(false),
        Type::Rigid(other, _, _) if other == name => return Ok(false),
        _ => {}
    }

    if contains_rigid(state, context, replacement, name)? {
        return Ok(false);
    }

    if let Some(current) = substitution.get(&name).copied() {
        let current = substitute_constraint(state, context, substitution, current)?;
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
