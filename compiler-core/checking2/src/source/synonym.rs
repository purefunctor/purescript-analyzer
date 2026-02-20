//! Implements syntax-driven checking rules for synonym detection.

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{Saturation, Synonym, Type, TypeId, normalise, unification};
use crate::error::ErrorKind;
use crate::state::CheckState;

use super::types;

pub fn lookup_file_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<Option<(Synonym, TypeId)>>
where
    Q: ExternalQueries,
{
    let (synonym, kind) = if file_id == context.id {
        (state.checked.lookup_synonym(type_id), state.checked.lookup_type(type_id))
    } else {
        let checked = context.queries.checked(file_id)?;
        (checked.lookup_synonym(type_id), checked.lookup_type(type_id))
    };

    Ok(synonym.zip(kind))
}

pub fn parse_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: lowering::TypeId,
) -> QueryResult<Option<(FileId, TypeItemId, Synonym, TypeId)>>
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

    let Some((synonym, kind)) = lookup_file_synonym(state, context, file_id, type_id)? else {
        return Ok(None);
    };

    Ok(Some((file_id, type_id, synonym, kind)))
}

pub fn infer_synonym_constructor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (file_id, item_id, mut synonym, kind): (FileId, TypeItemId, Synonym, TypeId),
    id: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    if is_recursive_synonym(context, file_id, item_id)? {
        state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, item_id });
    }

    let expected_arity = expected_synonym_arity(context, file_id, item_id)?;
    if expected_arity > 0 {
        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        let unknown = context.unknown("partial synonym application");
        return Ok((unknown, unknown));
    }

    synonym.saturation = Saturation::Full;
    synonym.arguments = Arc::default();

    let synonym_id = context.intern_synonym(synonym);
    let synonym_type = context.intern_synonym_application(synonym_id);

    Ok((synonym_type, kind))
}

pub fn infer_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
    (file_id, type_id, synonym, kind): (FileId, TypeItemId, Synonym, TypeId),
    arguments: &[lowering::TypeId],
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    if is_recursive_synonym(context, file_id, type_id)? {
        state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, item_id: type_id });
    }

    let expected_arity = expected_synonym_arity(context, file_id, type_id)?;
    let actual_arity = arguments.len();

    if actual_arity < expected_arity {
        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        let unknown = context.unknown("partial synonym application");
        return Ok((unknown, unknown));
    }

    let (synonym_arguments, excess_arguments) = arguments.split_at(expected_arity);

    let (argument_types, synonym_kind) =
        infer_synonym_application_arguments(state, context, kind, synonym_arguments)?;

    let synonym = Synonym {
        saturation: Saturation::Full,
        reference: synonym.reference,
        arguments: Arc::from(argument_types),
    };

    let synonym_id = context.intern_synonym(synonym);
    let mut synonym_type = context.intern_synonym_application(synonym_id);
    let mut synonym_kind = synonym_kind;

    for &argument in excess_arguments {
        (synonym_type, synonym_kind) =
            types::infer_application_kind(state, context, (synonym_type, synonym_kind), argument)?;
    }

    Ok((synonym_type, synonym_kind))
}

fn infer_synonym_application_arguments<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    kind: TypeId,
    arguments: &[lowering::TypeId],
) -> QueryResult<(Vec<TypeId>, TypeId)>
where
    Q: ExternalQueries,
{
    let mut argument_types = vec![];
    let mut synonym_kind = kind;

    for &argument_id in arguments {
        let (argument_type, result_kind) =
            infer_synonym_application_argument(state, context, synonym_kind, argument_id)?;

        argument_types.push(argument_type);
        synonym_kind = result_kind;
    }

    Ok((argument_types, synonym_kind))
}

fn infer_synonym_application_argument<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_kind: TypeId,
    argument: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let function_kind = normalise::normalise(state, context, function_kind)?;

    match context.lookup_type(function_kind) {
        Type::Function(argument_kind, result_kind) => {
            let (argument_type, _) = types::check_kind(state, context, argument, argument_kind)?;
            let result_kind = normalise::normalise(state, context, result_kind)?;
            Ok((argument_type, result_kind))
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification(context.queries, context.prim.t);
            let result_u = state.fresh_unification(context.queries, context.prim.t);

            let function_u = context.intern_function(argument_u, result_u);
            unification::solve(state, context, function_kind, unification_id, function_u)?;

            let (argument_type, _) = types::check_kind(state, context, argument, argument_u)?;
            let result_kind = normalise::normalise(state, context, result_u)?;

            Ok((argument_type, result_kind))
        }

        Type::Forall(binder_id, inner_kind) => {
            let binder = context.lookup_forall_binder(binder_id);
            let binder_kind = normalise::normalise(state, context, binder.kind)?;

            let kind_argument = state.fresh_unification(context.queries, binder_kind);
            let function_kind =
                SubstituteName::one(state, context, binder.name, kind_argument, inner_kind)?;

            infer_synonym_application_argument(state, context, function_kind, argument)
        }

        _ => {
            let (argument_type, _) = types::infer_kind(state, context, argument)?;

            {
                let function_type = context.unknown("unknown function type");
                let function_type = state.pretty_id(context, function_type)?;

                let function_kind = state.pretty_id(context, function_kind)?;
                let argument_type = state.pretty_id(context, argument_type)?;

                state.insert_error(ErrorKind::InvalidTypeApplication {
                    function_type,
                    function_kind,
                    argument_type,
                });
            }

            let k = context.unknown("cannot apply synonym");
            Ok((argument_type, k))
        }
    }
}

fn expected_synonym_arity<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<usize>
where
    Q: ExternalQueries,
{
    let expected_arity = |lowered: &lowering::LoweredModule| {
        let Some(lowering::TypeItemIr::SynonymGroup { synonym, .. }) =
            lowered.info.get_type_item(type_id)
        else {
            return 0;
        };
        let Some(synonym) = synonym else {
            return 0;
        };
        synonym.variables.len()
    };
    if file_id == context.id {
        Ok(expected_arity(&context.lowered))
    } else {
        let lowered = context.queries.lowered(file_id)?;
        Ok(expected_arity(&lowered))
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
