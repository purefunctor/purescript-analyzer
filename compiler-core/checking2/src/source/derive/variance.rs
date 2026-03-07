use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{KindOrType, Name, Type, TypeId, normalise, toolkit};
use crate::error::ErrorKind;
use crate::state::CheckState;

use super::tools;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(in crate::source) enum Variance {
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

type ParameterConfig = (Variance, Option<(FileId, TypeItemId)>);

#[derive(Clone, Copy)]
pub(in crate::source) enum VarianceConfig {
    Single(ParameterConfig),
    Pair(ParameterConfig, ParameterConfig),
}

struct DerivedParameter {
    name: Name,
    expected: Variance,
    class: Option<(FileId, TypeItemId)>,
}

enum DerivedRigids {
    Invalid,
    Single(DerivedParameter),
    Pair(DerivedParameter, DerivedParameter),
}

impl DerivedRigids {
    fn get(&self, name: Name) -> Option<&DerivedParameter> {
        self.iter().find(|parameter| parameter.name == name)
    }

    fn iter(&self) -> impl Iterator<Item = &DerivedParameter> {
        let (first, second) = match self {
            DerivedRigids::Invalid => (None, None),
            DerivedRigids::Single(first) => (Some(first), None),
            DerivedRigids::Pair(first, second) => (Some(first), Some(second)),
        };
        first.into_iter().chain(second)
    }
}

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
    for constructor_id in tools::lookup_data_constructors(context, data_file, data_id)? {
        let constructor_t = toolkit::lookup_file_term(state, context, data_file, constructor_id)?;
        let (fields, rigids) =
            extract_fields_with_rigids(state, context, constructor_t, derived_type, config)?;

        for field in fields {
            check_variance_field(state, context, field, Variance::Covariant, &rigids)?;
        }
    }

    Ok(())
}

fn extract_fields_with_rigids<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    constructor_t: TypeId,
    derived_type: TypeId,
    config: VarianceConfig,
) -> QueryResult<(Vec<TypeId>, DerivedRigids)>
where
    Q: ExternalQueries,
{
    let (_, arguments) = toolkit::extract_type_application(state, context, derived_type)?;
    let mut arguments = arguments.iter().copied();
    let mut current = constructor_t;
    let mut names = vec![];

    loop {
        current = normalise::normalise_expand(state, context, current)?;
        let Type::Forall(binder_id, inner) = context.lookup_type(current) else {
            break;
        };

        let binder = context.lookup_forall_binder(binder_id);
        let replacement = arguments.next().unwrap_or_else(|| {
            let rigid = state.fresh_rigid(context.queries, binder.kind);
            let Type::Rigid(name, _, _) = context.lookup_type(rigid) else {
                unreachable!("fresh_rigid must create Type::Rigid")
            };
            names.push(name);
            rigid
        });

        current = SubstituteName::one(state, context, binder.name, replacement, inner)?;
    }

    let rigids = match (config, &names[..]) {
        (VarianceConfig::Single((expected, class)), [.., a]) => {
            DerivedRigids::Single(DerivedParameter { name: *a, expected, class })
        }
        (
            VarianceConfig::Pair((first_expected, first_class), (second_expected, second_class)),
            [.., a, b],
        ) => DerivedRigids::Pair(
            DerivedParameter { name: *a, expected: first_expected, class: first_class },
            DerivedParameter { name: *b, expected: second_expected, class: second_class },
        ),
        _ => {
            let type_message = state.pretty_id(context, derived_type)?;
            state.insert_error(ErrorKind::CannotDeriveForType { type_message });
            DerivedRigids::Invalid
        }
    };

    let toolkit::InspectFunction { arguments: fields, .. } =
        toolkit::inspect_function(state, context, current)?;

    Ok((fields, rigids))
}

fn check_variance_field<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    variance: Variance,
    rigids: &DerivedRigids,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let type_id = normalise::normalise_expand(state, context, type_id)?;

    match context.lookup_type(type_id) {
        Type::Rigid(name, _, _) => {
            if let Some(parameter) = rigids.get(name) {
                emit_variance_error(state, context, type_id, variance, parameter.expected)?;
            }
        }
        Type::Function(argument, result) => {
            check_variance_field(state, context, argument, variance.flip(), rigids)?;
            check_variance_field(state, context, result, variance, rigids)?;
        }
        Type::Application(function, argument) => {
            let function = normalise::normalise_expand(state, context, function)?;
            if function == context.prim.record {
                check_variance_field(state, context, argument, variance, rigids)?;
            } else {
                for parameter in rigids.iter() {
                    if contains_rigid_name(state, context, argument, parameter.name)? {
                        emit_variance_error(state, context, type_id, variance, parameter.expected)?;
                        if variance == parameter.expected {
                            if let Some(class) = parameter.class {
                                tools::emit_constraint(context, state, class, function);
                            } else {
                                state.insert_error(ErrorKind::DeriveMissingFunctor);
                            }
                        }
                    }
                }
                check_variance_field(state, context, argument, variance, rigids)?;
            }
        }
        Type::KindApplication(_, argument) => {
            check_variance_field(state, context, argument, variance, rigids)?;
        }
        Type::Row(row_id) => {
            let row = context.lookup_row_type(row_id);
            for field in row.fields.iter() {
                check_variance_field(state, context, field.id, variance, rigids)?;
            }
            if let Some(tail) = row.tail {
                check_variance_field(state, context, tail, variance, rigids)?;
            }
        }
        _ => {}
    }

    Ok(())
}

fn emit_variance_error<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    actual: Variance,
    expected: Variance,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    if actual == expected {
        return Ok(());
    }

    let type_message = state.pretty_id(context, type_id)?;
    match actual {
        Variance::Covariant => state.insert_error(ErrorKind::CovariantOccurrence { type_message }),
        Variance::Contravariant => {
            state.insert_error(ErrorKind::ContravariantOccurrence { type_message })
        }
    }
    Ok(())
}

fn contains_rigid_name<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    name: Name,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let type_id = normalise::normalise(state, context, type_id)?;
    Ok(match context.lookup_type(type_id) {
        Type::Application(function, argument) | Type::KindApplication(function, argument) => {
            contains_rigid_name(state, context, function, name)?
                || contains_rigid_name(state, context, argument, name)?
        }
        Type::SynonymApplication(synonym_id) => {
            let synonym = context.lookup_synonym(synonym_id);
            let mut contains = false;
            for application in synonym.arguments.iter() {
                let argument = match application {
                    KindOrType::Kind(argument) | KindOrType::Type(argument) => *argument,
                };
                contains |= contains_rigid_name(state, context, argument, name)?;
            }
            contains
        }
        Type::Forall(_, inner) | Type::Constrained(_, inner) | Type::Kinded(inner, _) => {
            contains_rigid_name(state, context, inner, name)?
        }
        Type::Function(argument, result) => {
            contains_rigid_name(state, context, argument, name)?
                || contains_rigid_name(state, context, result, name)?
        }
        Type::Row(row_id) => {
            let row = context.lookup_row_type(row_id);
            let mut contains = false;
            for field in row.fields.iter() {
                contains |= contains_rigid_name(state, context, field.id, name)?;
            }
            if let Some(tail) = row.tail {
                contains |= contains_rigid_name(state, context, tail, name)?;
            }
            contains
        }
        Type::Rigid(rigid_name, _, _) => rigid_name == name,
        _ => false,
    })
}
