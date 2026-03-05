//! Implements syntax-driven checking rules for synonym detection.

use std::mem;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{KindOrType, Saturation, Synonym, Type, TypeId, normalise, toolkit, unification};
use crate::error::ErrorKind;
use crate::source::types;
use crate::state::CheckState;

#[derive(Debug, Clone, Copy)]
pub struct ParsedSynonym {
    pub file_id: FileId,
    pub type_id: TypeItemId,
    pub kind: TypeId,
    pub arity: usize,
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

    let Some(checked_synonym) = toolkit::lookup_file_synonym(state, context, file_id, type_id)?
    else {
        return Ok(None);
    };

    let kind = checked_synonym.kind;
    let arity = checked_synonym.parameters.len();

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
    let ParsedSynonym { file_id, type_id, kind: function_kind, arity } = synonym;

    if is_recursive_synonym(context, file_id, type_id)? {
        state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, type_id });
    }

    if arguments.len() < arity {
        if state.defer_expansion {
            return infer_partial_synonym_application(
                state,
                context,
                (file_id, type_id),
                function_kind,
                arguments,
            );
        }

        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        let unknown = context.unknown("partial synonym application");
        return Ok((unknown, unknown));
    }

    let (synonym_arguments, excess_arguments) = arguments.split_at(arity);
    let function_type = context.queries.intern_type(Type::Constructor(file_id, type_id));

    let defer_expansion = mem::replace(&mut state.defer_expansion, true);
    let chain_result = infer_synonym_application_chain(
        state,
        context,
        (function_type, function_kind),
        synonym_arguments,
    );
    state.defer_expansion = defer_expansion;

    let (applications, (_, synonym_kind)) = chain_result?;

    let synonym = Synonym {
        saturation: Saturation::Full,
        reference: (file_id, type_id),
        arguments: Arc::from(applications),
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

fn infer_partial_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (file_id, type_id): (FileId, TypeItemId),
    function_kind: TypeId,
    arguments: &[lowering::TypeId],
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let function_type = context.queries.intern_type(Type::Constructor(file_id, type_id));
    let (applications, (_, synonym_kind)) =
        infer_synonym_application_chain(state, context, (function_type, function_kind), arguments)?;

    let synonym = Synonym {
        saturation: Saturation::Partial,
        reference: (file_id, type_id),
        arguments: Arc::from(applications),
    };

    let synonym_id = context.intern_synonym(synonym);
    let synonym_type = context.intern_synonym_application(synonym_id);

    Ok((synonym_type, synonym_kind))
}

fn infer_synonym_application_chain<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: (TypeId, TypeId),
    arguments: &[lowering::TypeId],
) -> QueryResult<(Vec<KindOrType>, (TypeId, TypeId))>
where
    Q: ExternalQueries,
{
    let mut applications = vec![];
    let mut current_function = function;

    for &argument_id in arguments {
        current_function = infer_synonym_application_kind(
            state,
            context,
            &mut applications,
            current_function,
            argument_id,
        )?;
    }

    Ok((applications, current_function))
}

fn infer_synonym_application_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    applications: &mut Vec<KindOrType>,
    (function_type, function_kind): (TypeId, TypeId),
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
            let result_type = context.intern_application(function_type, argument_type);
            applications.push(KindOrType::Type(argument_type));
            Ok((result_type, result_kind))
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification(context.queries, context.prim.t);
            let result_u = state.fresh_unification(context.queries, context.prim.t);

            let function_u = context.intern_function(argument_u, result_u);
            unification::solve(state, context, function_kind, unification_id, function_u)?;

            let (argument_type, _) = types::check_kind(state, context, argument, argument_u)?;
            let result_kind = normalise::normalise(state, context, result_u)?;
            let result_type = context.intern_application(function_type, argument_type);

            applications.push(KindOrType::Type(argument_type));
            Ok((result_type, result_kind))
        }

        Type::Forall(binder_id, inner_kind) => {
            let binder = context.lookup_forall_binder(binder_id);
            let binder_kind = normalise::normalise(state, context, binder.kind)?;

            let kind_argument = state.fresh_unification(context.queries, binder_kind);
            let function_type = context.intern_kind_application(function_type, kind_argument);
            let function_kind =
                SubstituteName::one(state, context, binder.name, kind_argument, inner_kind)?;

            applications.push(KindOrType::Kind(kind_argument));
            infer_synonym_application_kind(
                state,
                context,
                applications,
                (function_type, function_kind),
                argument,
            )
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

            applications.push(KindOrType::Type(argument_type));
            Ok((t, k))
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
