//! Shared infrastructure for variance-aware type class derivation.
//!
//! This module provides the core types and functions used by Functor, Bifunctor,
//! Contravariant, Profunctor, Foldable, and Bifoldable derivation.

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::algorithm::derive::{self, tools};
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{substitute, toolkit, transfer};
use crate::core::{RowType, Type, TypeId, Variable, debruijn};
use crate::error::ErrorKind;

/// Variance of a type position.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Variance {
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

/// A derived type parameter with its expected variance and wrapper class.
#[derive(Clone, Copy)]
struct DerivedParameter {
    level: debruijn::Level,
    /// Expected variance for this parameter.
    expected: Variance,
    /// The class to emit when this parameter appears wrapped in a type application.
    class: Option<(FileId, TypeItemId)>,
}

impl DerivedParameter {
    fn new(level: debruijn::Level, (expected, class): ParameterConfig) -> DerivedParameter {
        DerivedParameter { level, expected, class }
    }
}

/// Tracks the Skolem variables representing derived type parameters.
///
/// - `Invalid`: Insufficient type parameters for derivation
/// - `Single`: Functor/Foldable (covariant) or Contravariant (contravariant)
/// - `Pair`: Bifunctor/Bifoldable (both covariant) or Profunctor (contra, covariant)
enum DerivedSkolems {
    Invalid,
    Single(DerivedParameter),
    Pair(DerivedParameter, DerivedParameter),
}

impl DerivedSkolems {
    fn get(&self, level: debruijn::Level) -> Option<&DerivedParameter> {
        self.iter().find(|p| p.level == level)
    }

    fn iter(&self) -> impl Iterator<Item = &DerivedParameter> {
        let (first, second) = match self {
            DerivedSkolems::Invalid => (None, None),
            DerivedSkolems::Single(a) => (Some(a), None),
            DerivedSkolems::Pair(a, b) => (Some(a), Some(b)),
        };
        first.into_iter().chain(second)
    }
}

/// Expected variance and wrapper class for a derived parameter.
pub type ParameterConfig = (Variance, Option<(FileId, TypeItemId)>);

/// Configuration for variance-aware derivation.
pub enum VarianceConfig {
    Single(ParameterConfig),
    Pair(ParameterConfig, ParameterConfig),
}

/// Generates variance-aware constraints for Functor-like derivation.
pub fn generate_variance_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    data_file: FileId,
    data_id: TypeItemId,
    derived_type: TypeId,
    config: VarianceConfig,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let constructors = tools::lookup_data_constructors(context, data_file, data_id)?;

    for constructor_id in constructors {
        let constructor_type =
            derive::lookup_local_term_type(state, context, data_file, constructor_id)?;

        let Some(constructor_type) = constructor_type else {
            continue;
        };

        let (fields, skolems) =
            extract_fields_with_skolems(state, context, constructor_type, derived_type, &config);

        for field_type in fields {
            check_variance_field(state, context, field_type, Variance::Covariant, &skolems);
        }
    }

    Ok(())
}

/// Extracts constructor fields and tracks Skolem variables for unmapped parameters.
///
/// Uses the variance configuration to assign expected variance and wrapper class
/// to each derived parameter's Skolem variable.
fn extract_fields_with_skolems<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constructor_type: TypeId,
    derived_type: TypeId,
    config: &VarianceConfig,
) -> (Vec<TypeId>, DerivedSkolems)
where
    Q: ExternalQueries,
{
    let type_arguments = toolkit::extract_all_applications(state, derived_type);
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

    // The last N levels correspond to the N derived parameters.
    let skolems = match (config, &levels[..]) {
        (VarianceConfig::Single(config), [.., a]) => {
            DerivedSkolems::Single(DerivedParameter::new(*a, *config))
        }
        (VarianceConfig::Pair(a_config, b_config), [.., a, b]) => DerivedSkolems::Pair(
            DerivedParameter::new(*a, *a_config),
            DerivedParameter::new(*b, *b_config),
        ),
        _ => {
            let global_type = transfer::globalize(state, context, derived_type);
            state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
            DerivedSkolems::Invalid
        }
    };

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

    (fields, skolems)
}

/// Checks a field type for variance violations and emits wrapper class constraints.
fn check_variance_field<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    variance: Variance,
    skolems: &DerivedSkolems,
) where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);

    match state.storage[type_id].clone() {
        Type::Variable(Variable::Skolem(level, _)) => {
            if let Some(parameter) = skolems.get(level)
                && variance != parameter.expected
            {
                let global = transfer::globalize(state, context, type_id);
                if variance == Variance::Covariant {
                    state.insert_error(ErrorKind::CovariantOccurrence { type_id: global });
                } else {
                    state.insert_error(ErrorKind::ContravariantOccurrence { type_id: global });
                }
            }
        }

        Type::Function(argument, result) => {
            check_variance_field(state, context, argument, variance.flip(), skolems);
            check_variance_field(state, context, result, variance, skolems);
        }

        Type::Application(function, argument) => {
            let function = state.normalize_type(function);

            if function == context.prim.record {
                check_variance_field(state, context, argument, variance, skolems);
            } else {
                for parameter in skolems.iter() {
                    if contains_skolem_level(state, argument, parameter.level) {
                        if variance != parameter.expected {
                            let global = transfer::globalize(state, context, type_id);
                            if variance == Variance::Covariant {
                                state.insert_error(ErrorKind::CovariantOccurrence {
                                    type_id: global,
                                });
                            } else {
                                state.insert_error(ErrorKind::ContravariantOccurrence {
                                    type_id: global,
                                });
                            }
                        } else if let Some(class) = parameter.class {
                            tools::emit_constraint(state, class, function);
                        } else {
                            state.insert_error(ErrorKind::DeriveMissingFunctor);
                        }
                    }
                }
                check_variance_field(state, context, argument, variance, skolems);
            }
        }

        Type::Row(RowType { ref fields, .. }) => {
            for field in fields.iter() {
                check_variance_field(state, context, field.id, variance, skolems);
            }
        }

        Type::KindApplication(_, argument) => {
            check_variance_field(state, context, argument, variance, skolems);
        }

        _ => (),
    }
}

/// Checks if a type contains a specific Skolem level.
fn contains_skolem_level(state: &mut CheckState, type_id: TypeId, target: debruijn::Level) -> bool {
    let type_id = state.normalize_type(type_id);

    match state.storage[type_id].clone() {
        Type::Variable(Variable::Skolem(level, _)) => level == target,

        Type::Application(function, argument) | Type::KindApplication(function, argument) => {
            contains_skolem_level(state, function, target)
                || contains_skolem_level(state, argument, target)
        }

        Type::Function(argument, result) => {
            contains_skolem_level(state, argument, target)
                || contains_skolem_level(state, result, target)
        }

        Type::Row(RowType { ref fields, tail }) => {
            fields.iter().any(|f| contains_skolem_level(state, f.id, target))
                || tail.is_some_and(|t| contains_skolem_level(state, t, target))
        }

        Type::Forall(_, inner) | Type::Constrained(_, inner) | Type::Kinded(inner, _) => {
            contains_skolem_level(state, inner, target)
        }

        _ => false,
    }
}
