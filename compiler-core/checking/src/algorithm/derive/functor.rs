//! Implements derive for Functor and Bifunctor type classes.
//!
//! Unlike Eq/Ord which require constraints on all fields, Functor/Bifunctor
//! derivation is variance-aware: the derived parameter must only appear in
//! covariant positions (right of function arrows, type application arguments).

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;

use crate::algorithm::derive::tools;
use crate::ExternalQueries;
use crate::algorithm::derive::extract_type_arguments;
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{substitute, transfer};
use crate::core::{RowType, Type, TypeId, Variable, debruijn};
use crate::error::ErrorKind;

/// Variance of a type position.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Variance {
    Covariant,
    Contravariant,
}

impl Variance {
    fn flip(self) -> Variance {
        match self {
            Variance::Covariant => Variance::Contravariant,
            Variance::Contravariant => Variance::Covariant,
        }
    }
}

/// Tracks the Skolem variables representing derived type parameters.
struct DerivedSkolems {
    /// Skolem levels for the parameters being mapped over.
    /// - `Functor`: one level
    /// - `Bifunctor`: two levels
    levels: Vec<debruijn::Level>,
}

impl DerivedSkolems {
    fn contains(&self, level: debruijn::Level) -> bool {
        self.levels.contains(&level)
    }
}

/// Checks a derive instance for Functor.
pub fn check_derive_functor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let [(derived_type, _)] = input.arguments[..] else {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file: input.class_file,
            class_id: input.class_id,
            expected: 1,
            actual: input.arguments.len(),
        });
        return Ok(());
    };

    let Some((data_file, data_id)) = super::extract_type_constructor(state, derived_type) else {
        let global_type = transfer::globalize(state, context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
        return Ok(());
    };

    let functor = Some((input.class_file, input.class_id));
    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    generate_functor_constraints(state, context, data_file, data_id, derived_type, functor, 1)?;

    tools::solve_and_report_constraints(state, context)
}

/// Checks a derive instance for Bifunctor.
pub fn check_derive_bifunctor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let [(derived_type, _)] = input.arguments[..] else {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file: input.class_file,
            class_id: input.class_id,
            expected: 1,
            actual: input.arguments.len(),
        });
        return Ok(());
    };

    let Some((data_file, data_id)) = super::extract_type_constructor(state, derived_type) else {
        let global_type = transfer::globalize(state, context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
        return Ok(());
    };

    // Bifunctor derivation emits Functor constraints for wrapped parameters.
    let functor = context.known_types.functor;
    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    generate_functor_constraints(state, context, data_file, data_id, derived_type, functor, 2)?;

    tools::solve_and_report_constraints(state, context)
}

/// Generates variance-aware constraints for Functor/Bifunctor derivation.
fn generate_functor_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    data_file: FileId,
    data_id: TypeItemId,
    derived_type: TypeId,
    functor: Option<(FileId, TypeItemId)>,
    parameter_count: usize,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let constructors = tools::lookup_data_constructors(context, data_file, data_id)?;

    for constructor_id in constructors {
        let constructor_type =
            super::lookup_local_term_type(state, context, data_file, constructor_id)?;
        let Some(constructor_type) = constructor_type else { continue };

        let (fields, skolems) =
            extract_fields_with_skolems(state, constructor_type, derived_type, parameter_count);

        for field_type in fields {
            check_functor_field(state, context, field_type, Variance::Covariant, &skolems, functor);
        }
    }

    Ok(())
}

/// Extracts constructor fields and tracks Skolem variables for unmapped parameters.
///
/// Similar to `extract_constructor_fields` but also returns the Skolem variables
/// created for type parameters that weren't provided in the instance head.
fn extract_fields_with_skolems(
    state: &mut CheckState,
    constructor_type: TypeId,
    derived_type: TypeId,
    parameter_count: usize,
) -> (Vec<TypeId>, DerivedSkolems) {
    let type_arguments = extract_type_arguments(state, derived_type);
    let mut arguments_iter = type_arguments.into_iter();
    let mut current_id = constructor_type;
    let mut levels = vec![];

    safe_loop! {
        current_id = state.normalize_type(current_id);
        match &state.storage[current_id] {
            Type::Forall(binder, inner) => {
                let binder_level = binder.level;
                let binder_kind = binder.kind;
                let inner = *inner;

                let argument_type = arguments_iter.next().unwrap_or_else(|| {
                    levels.push(binder_level);
                    let skolem = Variable::Skolem(binder_level, binder_kind);
                    state.storage.intern(Type::Variable(skolem))
                });

                current_id = substitute::SubstituteBound::on(state, binder_level, argument_type, inner);
            }
            _ => break,
        }
    }

    // The last parameter_count variables are used for Functor/Bifunctor.
    // When arguments_iter runs out, it creates skolem variables instead.
    let levels = levels.into_iter().rev().take(parameter_count).collect_vec();

    let mut fields = vec![];
    safe_loop! {
        current_id = state.normalize_type(current_id);
        match state.storage[current_id] {
            Type::Function(argument, result) => {
                fields.push(argument);
                current_id = result;
            }
            _ => break,
        }
    }

    (fields, DerivedSkolems { levels })
}

/// Checks a field type for variance violations and emits Functor constraints.
fn check_functor_field<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    variance: Variance,
    skolems: &DerivedSkolems,
    functor: Option<(FileId, TypeItemId)>,
) where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);

    match state.storage[type_id].clone() {
        Type::Variable(Variable::Skolem(level, _)) if skolems.contains(level) => {
            if variance == Variance::Contravariant {
                let global = transfer::globalize(state, context, type_id);
                state.insert_error(ErrorKind::ContravariantOccurrence { type_id: global });
            }
        }

        Type::Function(argument, result) => {
            check_functor_field(state, context, argument, variance.flip(), skolems, functor);
            check_functor_field(state, context, result, variance, skolems, functor);
        }

        Type::Application(function, argument) => {
            let function = state.normalize_type(function);

            if function == context.prim.record {
                check_functor_field(state, context, argument, variance, skolems, functor);
            } else if contains_derived_skolem(state, argument, skolems) {
                if variance == Variance::Contravariant {
                    let global = transfer::globalize(state, context, type_id);
                    state.insert_error(ErrorKind::ContravariantOccurrence { type_id: global });
                } else if let Some(functor) = functor {
                    tools::emit_constraint(state, functor, function);
                } else {
                    state.insert_error(ErrorKind::DeriveMissingFunctor);
                }
                check_functor_field(state, context, argument, variance, skolems, functor);
            } else {
                check_functor_field(state, context, argument, variance, skolems, functor);
            }
        }

        Type::Row(RowType { ref fields, .. }) => {
            for field in fields.iter() {
                check_functor_field(state, context, field.id, variance, skolems, functor);
            }
        }

        Type::KindApplication(_, argument) => {
            check_functor_field(state, context, argument, variance, skolems, functor);
        }

        _ => (),
    }
}

/// Checks if a type contains any of the derived parameter Skolem variables.
fn contains_derived_skolem(
    state: &mut CheckState,
    type_id: TypeId,
    skolems: &DerivedSkolems,
) -> bool {
    let type_id = state.normalize_type(type_id);

    match state.storage[type_id].clone() {
        Type::Variable(Variable::Skolem(level, _)) => skolems.contains(level),

        Type::Application(function, argument) | Type::KindApplication(function, argument) => {
            contains_derived_skolem(state, function, skolems)
                || contains_derived_skolem(state, argument, skolems)
        }

        Type::Function(argument, result) => {
            contains_derived_skolem(state, argument, skolems)
                || contains_derived_skolem(state, result, skolems)
        }

        Type::Row(RowType { ref fields, tail }) => {
            fields.iter().any(|f| contains_derived_skolem(state, f.id, skolems))
                || tail.map_or(false, |t| contains_derived_skolem(state, t, skolems))
        }

        Type::Forall(_, inner) | Type::Constrained(_, inner) | Type::Kinded(inner, _) => {
            contains_derived_skolem(state, inner, skolems)
        }

        _ => false,
    }
}

