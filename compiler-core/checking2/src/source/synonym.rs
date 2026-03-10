//! Implements syntax-driven checking rules for synonym detection.
use std::ops::ControlFlow;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;

use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{
    CheckedSynonym, KindOrType, Saturation, Synonym, Type, TypeId, normalise, toolkit, unification,
};
use crate::error::ErrorKind;
use crate::source::types;
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

#[derive(Debug, Clone, Copy)]
pub struct ParsedSynonym {
    pub file_id: FileId,
    pub type_id: TypeItemId,
    pub kind: TypeId,
    pub arity: usize,
}

#[derive(Debug, Clone, Copy)]
enum Argument {
    Syntax(lowering::TypeId),
    Core(TypeId, TypeId),
}

pub fn parse_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: lowering::TypeId,
) -> QueryResult<Option<ParsedSynonym>>
where
    Q: ExternalQueries,
{
    let Some(lowering::TypeKind::Constructor { resolution }) =
        context.lowered.info.get_type_kind(function)
    else {
        return Ok(None);
    };

    let Some((file_id, type_id)) = *resolution else {
        return Ok(None);
    };

    let Some(CheckedSynonym { kind, parameters, .. }) =
        toolkit::lookup_file_synonym(state, context, file_id, type_id)?
    else {
        return Ok(None);
    };

    let arity = parameters.len();
    Ok(Some(ParsedSynonym { file_id, type_id, kind, arity }))
}

pub fn infer_synonym_constructor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    synonym: ParsedSynonym,
    id: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let ParsedSynonym { file_id, type_id, kind, arity } = synonym;

    if arity > 0 {
        if state.defer_expansion {
            let synonym_type = context.queries.intern_type(Type::Constructor(file_id, type_id));
            return Ok((synonym_type, kind));
        }

        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        let unknown = context.unknown("partial synonym application");
        return Ok((unknown, unknown));
    }

    if is_recursive_synonym(context, file_id, type_id)? {
        state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, type_id });
    }

    let synonym = Synonym {
        saturation: Saturation::Full,
        reference: (file_id, type_id),
        arguments: Arc::default(),
    };

    let synonym_id = context.intern_synonym(synonym);
    let synonym_type = context.intern_synonym_application(synonym_id);

    Ok((synonym_type, kind))
}

pub fn infer_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
    synonym: ParsedSynonym,
    arguments: &[lowering::TypeId],
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let ParsedSynonym { file_id, type_id, kind, arity } = synonym;

    if arguments.len() < arity {
        if state.defer_expansion {
            return infer_partial_synonym_application(
                state,
                context,
                (file_id, type_id),
                kind,
                arguments,
            );
        }

        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        let unknown = context.unknown("partial synonym application");
        return Ok((unknown, unknown));
    }

    let arguments = arguments.iter().copied().map(Argument::Syntax).collect_vec();

    state.with_defer_expansion(|state| {
        check_synonym_application_arguments(state, context, synonym, &arguments)
    })
}

pub fn check_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    synonym: ParsedSynonym,
    arguments: &[(TypeId, TypeId)],
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let arguments = arguments
        .iter()
        .copied()
        .map(|(type_id, kind_id)| Argument::Core(type_id, kind_id))
        .collect_vec();

    state.with_defer_expansion(|state| {
        check_synonym_application_arguments(state, context, synonym, &arguments)
    })
}

pub fn try_check_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (file_id, type_id): (FileId, TypeItemId),
    kind: TypeId,
    arguments: &[(TypeId, TypeId)],
) -> QueryResult<Option<(TypeId, TypeId)>>
where
    Q: ExternalQueries,
{
    let Some(CheckedSynonym { parameters, .. }) =
        toolkit::lookup_file_synonym(state, context, file_id, type_id)?
    else {
        return Ok(None);
    };

    let synonym = ParsedSynonym { file_id, type_id, kind, arity: parameters.len() };
    let checked = check_synonym_application(state, context, synonym, arguments)?;
    Ok(Some(checked))
}

fn check_synonym_application_arguments<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    synonym: ParsedSynonym,
    arguments: &[Argument],
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let ParsedSynonym { file_id, type_id, kind, arity } = synonym;
    debug_assert!(arguments.len() >= arity);

    if is_recursive_synonym(context, file_id, type_id)? {
        state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, type_id });
    }

    let function_type = context.queries.intern_type(Type::Constructor(file_id, type_id));
    let (synonym_arguments, excess_arguments) = arguments.split_at(arity);

    let (applications, (_, synonym_kind)) =
        collect_synonym_applications(state, context, (function_type, kind), synonym_arguments)?;

    let synonym = Synonym {
        saturation: Saturation::Full,
        reference: (file_id, type_id),
        arguments: Arc::from(applications),
    };

    let synonym_id = context.intern_synonym(synonym);
    let mut synonym_type = context.intern_synonym_application(synonym_id);
    let mut synonym_kind = synonym_kind;

    for &argument in excess_arguments {
        let mut applications = vec![];
        (synonym_type, synonym_kind) = apply_synonym_argument(
            state,
            context,
            &mut applications,
            (synonym_type, synonym_kind),
            argument,
        )?;
    }

    Ok((synonym_type, synonym_kind))
}

fn infer_partial_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (file_id, type_id): (FileId, TypeItemId),
    kind: TypeId,
    arguments: &[lowering::TypeId],
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let function_type = context.queries.intern_type(Type::Constructor(file_id, type_id));
    let arguments = arguments.iter().copied().map(Argument::Syntax).collect_vec();

    let (applications, (_, synonym_kind)) =
        collect_synonym_applications(state, context, (function_type, kind), &arguments)?;

    let synonym = Synonym {
        saturation: Saturation::Partial,
        reference: (file_id, type_id),
        arguments: Arc::from(applications),
    };

    let synonym_id = context.intern_synonym(synonym);
    let synonym_type = context.intern_synonym_application(synonym_id);

    Ok((synonym_type, synonym_kind))
}

fn collect_synonym_applications<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut function: (TypeId, TypeId),
    arguments: &[Argument],
) -> QueryResult<(Vec<KindOrType>, (TypeId, TypeId))>
where
    Q: ExternalQueries,
{
    let mut applications = vec![];

    for &argument in arguments {
        function = apply_synonym_argument(state, context, &mut applications, function, argument)?;
    }

    Ok((applications, function))
}

fn apply_synonym_argument_step<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    applications: &mut Vec<KindOrType>,
    (function_type, function_kind): (TypeId, TypeId),
    argument: Argument,
) -> QueryResult<ControlFlow<(TypeId, TypeId), (TypeId, TypeId)>>
where
    Q: ExternalQueries,
{
    match context.lookup_type(function_kind) {
        Type::Function(expected_kind, result_kind) => {
            let argument_type = match argument {
                Argument::Syntax(argument_id) => {
                    let (argument_type, _) =
                        types::check_kind(state, context, argument_id, expected_kind)?;
                    argument_type
                }
                Argument::Core(argument_type, argument_kind) => {
                    unification::subtype(state, context, argument_kind, expected_kind)?;
                    argument_type
                }
            };

            let result_type = context.intern_application(function_type, argument_type);
            let result_kind = normalise::normalise(state, context, result_kind)?;

            applications.push(KindOrType::Type(argument_type));
            Ok(ControlFlow::Break((result_type, result_kind)))
        }

        Type::Unification(unification_id) => {
            let argument_kind = state.fresh_unification(context.queries, context.prim.t);
            let result_kind = state.fresh_unification(context.queries, context.prim.t);

            let function = context.intern_function(argument_kind, result_kind);
            unification::solve(state, context, function_kind, unification_id, function)?;

            let argument_type = match argument {
                Argument::Syntax(argument_id) => {
                    let (argument_type, _) =
                        types::check_kind(state, context, argument_id, argument_kind)?;
                    argument_type
                }
                Argument::Core(argument_type, checked_kind) => {
                    unification::subtype(state, context, checked_kind, argument_kind)?;
                    argument_type
                }
            };

            let result_type = context.intern_application(function_type, argument_type);
            let result_kind = normalise::normalise(state, context, result_kind)?;

            applications.push(KindOrType::Type(argument_type));
            Ok(ControlFlow::Break((result_type, result_kind)))
        }

        Type::Forall(binder_id, inner_kind) => {
            let binder = context.lookup_forall_binder(binder_id);
            let binder_kind = normalise::normalise(state, context, binder.kind)?;

            let kind_argument = state.fresh_unification(context.queries, binder_kind);

            let function_type = context.intern_kind_application(function_type, kind_argument);
            let function_kind =
                SubstituteName::one(state, context, binder.name, kind_argument, inner_kind)?;

            applications.push(KindOrType::Kind(kind_argument));
            Ok(ControlFlow::Continue((function_type, function_kind)))
        }

        _ => {
            let argument_type = match argument {
                Argument::Syntax(argument_id) => {
                    let (argument_type, _) = types::infer_kind(state, context, argument_id)?;
                    argument_type
                }
                Argument::Core(argument_type, _) => argument_type,
            };

            let invalid_type = context.intern_application(function_type, argument_type);
            let unknown_kind = context.unknown("cannot apply synonym type");

            toolkit::report_invalid_type_application(
                state,
                context,
                function_type,
                function_kind,
                argument_type,
            )?;

            applications.push(KindOrType::Type(argument_type));
            Ok(ControlFlow::Break((invalid_type, unknown_kind)))
        }
    }
}

fn apply_synonym_argument<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    applications: &mut Vec<KindOrType>,
    (mut function_type, mut function_kind): (TypeId, TypeId),
    argument: Argument,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    safe_loop! {
        function_kind = normalise::normalise(state, context, function_kind)?;
        match apply_synonym_argument_step(
            state,
            context,
            applications,
            (function_type, function_kind),
            argument,
        )? {
            ControlFlow::Break(result) => break Ok(result),
            ControlFlow::Continue((next_type, next_kind)) => {
                function_type = next_type;
                function_kind = next_kind;
            }
        };
    }
}

fn is_recursive_synonym<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let has_recursive_error = |grouped: &lowering::GroupedModule| {
        grouped.cycle_errors.iter().any(|error| {
            if let lowering::LoweringError::RecursiveSynonym(recursive) = error {
                recursive.group.contains(&type_id)
            } else {
                false
            }
        })
    };

    let grouped = if file_id == context.id {
        Arc::clone(&context.grouped)
    } else {
        context.queries.grouped(file_id)?
    };

    Ok(has_recursive_error(&grouped))
}
