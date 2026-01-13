//! Implements derive for variance-aware type classes.
//!
//! This module handles derivation for Functor, Bifunctor, Contravariant, and
//! Profunctor. Unlike Eq/Ord which require constraints on all fields, these
//! classes are variance-aware: each derived parameter has an expected variance
//! (covariant or contravariant) that must be satisfied.

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::algorithm::derive::{extract_type_arguments, tools};
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

/// A derived type parameter with its expected variance and wrapper class.
#[derive(Clone, Copy)]
struct DerivedParam {
    level: debruijn::Level,
    /// Expected variance for this parameter.
    expected: Variance,
    /// The class to emit when this parameter appears wrapped in a type application.
    /// For Functor/Bifunctor this is Functor, for Contravariant/Profunctor's first
    /// param this is Contravariant.
    wrapper_class: Option<(FileId, TypeItemId)>,
}

/// Tracks the Skolem variables representing derived type parameters.
struct DerivedSkolems {
    /// Parameters being mapped over with their variance requirements.
    /// - `Functor`: one param (Covariant, Functor)
    /// - `Contravariant`: one param (Contravariant, Contravariant)
    /// - `Bifunctor`: two params (Covariant, Functor), (Covariant, Functor)
    /// - `Profunctor`: two params (Contravariant, Contravariant), (Covariant, Functor)
    params: Vec<DerivedParam>,
}

impl DerivedSkolems {
    fn get(&self, level: debruijn::Level) -> Option<&DerivedParam> {
        self.params.iter().find(|p| p.level == level)
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

    let config = VarianceConfig::new([(Variance::Covariant, functor)]);
    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

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

    let config =
        VarianceConfig::new([(Variance::Covariant, functor), (Variance::Covariant, functor)]);

    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}

/// Checks a derive instance for Contravariant.
pub fn check_derive_contravariant<Q>(
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

    let contravariant = Some((input.class_file, input.class_id));
    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    let config = VarianceConfig::new([(Variance::Contravariant, contravariant)]);
    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}

/// Checks a derive instance for Profunctor.
pub fn check_derive_profunctor<Q>(
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

    // Profunctor: first param is contravariant, second is covariant.
    let contravariant = context.known_types.contravariant;
    let functor = context.known_types.functor;
    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    let config = VarianceConfig::new([
        (Variance::Contravariant, contravariant),
        (Variance::Covariant, functor),
    ]);

    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}

/// Expected variance and wrapper class for a derived parameter.
type ParameterConfig = (Variance, Option<(FileId, TypeItemId)>);

/// Configuration for variance-aware derivation.
struct VarianceConfig {
    /// Expected variance and wrapper class for each derived parameter.
    /// Ordered from first to last (e.g., for `data T a b`, index 0 is `a`).
    params: Vec<ParameterConfig>,
}

impl VarianceConfig {
    fn new(params: impl IntoIterator<Item = ParameterConfig>) -> Self {
        Self { params: params.into_iter().collect() }
    }
}

/// Generates variance-aware constraints for Functor-like derivation.
fn generate_variance_constraints<Q>(
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
            super::lookup_local_term_type(state, context, data_file, constructor_id)?;
        let Some(constructor_type) = constructor_type else { continue };

        let (fields, skolems) =
            extract_fields_with_skolems(state, constructor_type, derived_type, &config);

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
fn extract_fields_with_skolems(
    state: &mut CheckState,
    constructor_type: TypeId,
    derived_type: TypeId,
    config: &VarianceConfig,
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

    // The last N levels correspond to the N derived parameters.
    // Take last N via rev().take(N), then restore original order.
    let param_count = config.params.len();
    let derived_levels: Vec<_> = levels.into_iter().rev().take(param_count).collect();
    let params = derived_levels
        .into_iter()
        .rev()
        .zip(config.params.iter())
        .map(|(level, &(expected, wrapper_class))| DerivedParam { level, expected, wrapper_class })
        .collect_vec();

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

    (fields, DerivedSkolems { params })
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
            if let Some(param) = skolems.get(level) {
                if variance != param.expected {
                    let global = transfer::globalize(state, context, type_id);
                    if variance == Variance::Covariant {
                        state.insert_error(ErrorKind::CovariantOccurrence { type_id: global });
                    } else {
                        state.insert_error(ErrorKind::ContravariantOccurrence { type_id: global });
                    }
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
                // Check each derived parameter that appears in the argument
                for param in &skolems.params {
                    if contains_skolem_level(state, argument, param.level) {
                        if variance != param.expected {
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
                        } else if let Some(wrapper_class) = param.wrapper_class {
                            tools::emit_constraint(state, wrapper_class, function);
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
                || tail.map_or(false, |t| contains_skolem_level(state, t, target))
        }

        Type::Forall(_, inner) | Type::Constrained(_, inner) | Type::Kinded(inner, _) => {
            contains_skolem_level(state, inner, target)
        }

        _ => false,
    }
}
