//! Implements syntax-driven checking rules for synonym detection.

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{CheckedSynonym, Saturation, Synonym, Type, TypeId, normalise, unification};
use crate::error::ErrorKind;
use crate::source::types;
use crate::state::CheckState;

pub fn lookup_file_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<Option<(CheckedSynonym, TypeId)>>
where
    Q: ExternalQueries,
{
    let (synonym, kind) = if file_id == context.id {
        (state.checked.lookup_synonym(type_id), state.checked.lookup_type(type_id))
    } else {
        let checked = context.queries.checked2(file_id)?;
        (checked.lookup_synonym(type_id), checked.lookup_type(type_id))
    };

    Ok(synonym.zip(kind))
}

pub fn parse_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: lowering::TypeId,
) -> QueryResult<Option<(FileId, TypeItemId, TypeId, usize)>>
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

    let Some((checked_synonym, kind)) = lookup_file_synonym(state, context, file_id, type_id)?
    else {
        return Ok(None);
    };

    let arity = checked_synonym.parameters.len();
    Ok(Some((file_id, type_id, kind, arity)))
}

pub fn infer_synonym_constructor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (file_id, item_id, kind, arity): (FileId, TypeItemId, TypeId, usize),
    id: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    if is_recursive_synonym(context, file_id, item_id)? {
        state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, item_id });
    }

    if arity > 0 {
        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        let unknown = context.unknown("partial synonym application");
        return Ok((unknown, unknown));
    }

    let synonym = Synonym {
        saturation: Saturation::Full,
        reference: (file_id, item_id),
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
    (file_id, type_id, function_kind, arity): (FileId, TypeItemId, TypeId, usize),
    arguments: &[lowering::TypeId],
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    if is_recursive_synonym(context, file_id, type_id)? {
        state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, item_id: type_id });
    }

    if arguments.len() < arity {
        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        let unknown = context.unknown("partial synonym application");
        return Ok((unknown, unknown));
    }

    let (synonym_arguments, excess_arguments) = arguments.split_at(arity);

    let function_type = context.queries.intern_type(Type::Constructor(file_id, type_id));
    let (argument_types, (_, synonym_kind)) = infer_synonym_application_chain(
        state,
        context,
        (function_type, function_kind),
        synonym_arguments,
    )?;

    let synonym = Synonym {
        saturation: Saturation::Full,
        reference: (file_id, type_id),
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

fn infer_synonym_application_chain<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: (TypeId, TypeId),
    arguments: &[lowering::TypeId],
) -> QueryResult<(Vec<TypeId>, (TypeId, TypeId))>
where
    Q: ExternalQueries,
{
    let mut argument_types = vec![];
    let mut current_function = function;

    for &argument_id in arguments {
        let (argument_type, result_function) =
            infer_synonym_application_kind(state, context, current_function, argument_id)?;

        argument_types.push(argument_type);
        current_function = result_function;
    }

    Ok((argument_types, current_function))
}

fn infer_synonym_application_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (function_type, function_kind): (TypeId, TypeId),
    argument: lowering::TypeId,
) -> QueryResult<(TypeId, (TypeId, TypeId))>
where
    Q: ExternalQueries,
{
    let function_kind = normalise::normalise(state, context, function_kind)?;

    match context.lookup_type(function_kind) {
        Type::Function(argument_kind, result_kind) => {
            let (argument_type, _) = types::check_kind(state, context, argument, argument_kind)?;
            let result_kind = normalise::normalise(state, context, result_kind)?;
            let result_type = context.intern_application(function_type, argument_type);
            Ok((argument_type, (result_type, result_kind)))
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification(context.queries, context.prim.t);
            let result_u = state.fresh_unification(context.queries, context.prim.t);

            let function_u = context.intern_function(argument_u, result_u);
            unification::solve(state, context, function_kind, unification_id, function_u)?;

            let (argument_type, _) = types::check_kind(state, context, argument, argument_u)?;
            let result_kind = normalise::normalise(state, context, result_u)?;
            let result_type = context.intern_application(function_type, argument_type);

            Ok((argument_type, (result_type, result_kind)))
        }

        Type::Forall(binder_id, inner_kind) => {
            let binder = context.lookup_forall_binder(binder_id);
            let binder_kind = normalise::normalise(state, context, binder.kind)?;

            let kind_argument = state.fresh_unification(context.queries, binder_kind);
            let function_type = context.intern_kind_application(function_type, kind_argument);
            let function_kind =
                SubstituteName::one(state, context, binder.name, kind_argument, inner_kind)?;

            infer_synonym_application_kind(state, context, (function_type, function_kind), argument)
        }

        _ => {
            let (argument_type, _) = types::infer_kind(state, context, argument)?;

            let t = context.intern_application(function_type, argument_type);
            let k = context.unknown("cannot apply synonym type");

            {
                let function_type = state.pretty_id(context, function_type)?;
                let function_kind = state.pretty_id(context, function_kind)?;
                let argument_type = state.pretty_id(context, argument_type)?;

                state.insert_error(ErrorKind::InvalidTypeApplication {
                    function_type,
                    function_kind,
                    argument_type,
                });
            }

            Ok((argument_type, (t, k)))
        }
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
