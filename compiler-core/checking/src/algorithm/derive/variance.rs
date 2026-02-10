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
use crate::algorithm::{substitute, toolkit};
use crate::core::{Name, RowType, Type, TypeId, Variable};
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
#[derive(Clone)]
struct DerivedParameter {
    name: Name,
    /// Expected variance for this parameter.
    expected: Variance,
    /// The class to emit when this parameter appears wrapped in a type application.
    class: Option<(FileId, TypeItemId)>,
}

impl DerivedParameter {
    fn new(name: Name, (expected, class): ParameterConfig) -> DerivedParameter {
        DerivedParameter { name, expected, class }
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
    fn get(&self, name: &Name) -> Option<&DerivedParameter> {
        self.iter().find(|p| p.name == *name)
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
#[derive(Clone, Copy)]
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
    let mut names = vec![];

    safe_loop! {
        current_id = state.normalize_type(current_id);
        match &state.storage[current_id] {
            Type::Forall(binder, inner) => {
                let binder_variable = binder.variable.clone();
                let binder_kind = binder.kind;
                let inner = *inner;

                let argument_type = arguments_iter.next().unwrap_or_else(|| {
                    names.push(binder_variable.clone());
                    let skolem = Variable::Skolem(binder_variable.clone(), binder_kind);
                    state.storage.intern(Type::Variable(skolem))
                });

                current_id = substitute::SubstituteBound::on(state, binder_variable, argument_type, inner);
            }
            _ => break,
        }
    }

    // The last N names correspond to the N derived parameters.
    let skolems = match (config, &names[..]) {
        (VarianceConfig::Single(config), [.., a]) => {
            DerivedSkolems::Single(DerivedParameter::new(a.clone(), *config))
        }
        (VarianceConfig::Pair(a_config, b_config), [.., a, b]) => DerivedSkolems::Pair(
            DerivedParameter::new(a.clone(), *a_config),
            DerivedParameter::new(b.clone(), *b_config),
        ),
        _ => {
            let type_message = state.render_local_type(context, derived_type);
            state.insert_error(ErrorKind::CannotDeriveForType { type_message });
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
        Type::Variable(Variable::Skolem(name, _)) => {
            if let Some(parameter) = skolems.get(&name)
                && variance != parameter.expected
            {
                let type_message = state.render_local_type(context, type_id);
                if variance == Variance::Covariant {
                    state.insert_error(ErrorKind::CovariantOccurrence { type_message });
                } else {
                    state.insert_error(ErrorKind::ContravariantOccurrence { type_message });
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
                    if contains_skolem_name(state, argument, &parameter.name) {
                        if variance != parameter.expected {
                            let type_message = state.render_local_type(context, type_id);
                            if variance == Variance::Covariant {
                                state.insert_error(ErrorKind::CovariantOccurrence { type_message });
                            } else {
                                state.insert_error(ErrorKind::ContravariantOccurrence {
                                    type_message,
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

/// Checks if a type contains a specific Skolem name.
fn contains_skolem_name(state: &mut CheckState, type_id: TypeId, target: &Name) -> bool {
    let type_id = state.normalize_type(type_id);

    match state.storage[type_id].clone() {
        Type::Variable(Variable::Skolem(name, _)) => name == *target,

        Type::Application(function, argument) | Type::KindApplication(function, argument) => {
            contains_skolem_name(state, function, target)
                || contains_skolem_name(state, argument, target)
        }

        Type::Function(argument, result) => {
            contains_skolem_name(state, argument, target)
                || contains_skolem_name(state, result, target)
        }

        Type::Row(RowType { ref fields, tail }) => {
            fields.iter().any(|f| contains_skolem_name(state, f.id, target))
                || tail.is_some_and(|t| contains_skolem_name(state, t, target))
        }

        Type::Forall(_, inner) | Type::Constrained(_, inner) | Type::Kinded(inner, _) => {
            contains_skolem_name(state, inner, target)
        }

        _ => false,
    }
}
