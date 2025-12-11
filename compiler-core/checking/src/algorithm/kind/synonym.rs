//! Synonym-related kind checking.
//!
//! This module handles the kind checking of type synonyms, including:
//! - Looking up synonyms from local and external modules
//! - Checking partial vs full synonym applications
//! - Instantiating and applying synonym type arguments

use std::mem;
use std::sync::Arc;

use files::FileId;
use indexing::TypeItemId;

use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, substitute, transfer, unification};
use crate::core::{ForallBinder, Synonym, Type, TypeId};
use crate::error::ErrorKind;
use crate::{CheckedModule, ExternalQueries};

fn localize_synonym_and_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    synonym: Synonym,
    kind: TypeId,
) -> (Synonym, TypeId)
where
    Q: ExternalQueries,
{
    let synonym_type = transfer::localize(state, context, synonym.synonym_type);
    let synonym = synonym.with_synonym_type(synonym_type);
    let kind = transfer::localize(state, context, kind);
    (synonym, kind)
}

fn lookup_local_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeItemId,
) -> Option<(Synonym, TypeId)>
where
    Q: ExternalQueries,
{
    if let Some(synonym) = state.binding_group.lookup_synonym(type_id)
        && let Some(kind) = state.binding_group.lookup_type(type_id)
    {
        Some((synonym, kind))
    } else if let Some(synonym) = state.checked.lookup_synonym(type_id)
        && let Some(kind) = state.checked.lookup_type(type_id)
    {
        Some(localize_synonym_and_kind(state, context, synonym, kind))
    } else {
        None
    }
}

fn lookup_global_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    checked: &CheckedModule,
    type_id: TypeItemId,
) -> Option<(Synonym, TypeId)>
where
    Q: ExternalQueries,
{
    if let Some(synonym) = checked.lookup_synonym(type_id)
        && let Some(kind) = checked.lookup_type(type_id)
    {
        Some(localize_synonym_and_kind(state, context, synonym, kind))
    } else {
        None
    }
}

pub fn lookup_file_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> Option<(Synonym, TypeId)>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        lookup_local_synonym(state, context, type_id)
    } else {
        let checked = context.queries.checked(file_id).ok()?;
        lookup_global_synonym(state, context, &checked, type_id)
    }
}

pub fn infer_synonym_constructor<Q: ExternalQueries>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (file_id, type_id, synonym, kind): (FileId, TypeItemId, Synonym, TypeId),
    id: lowering::TypeId,
) -> (TypeId, TypeId) {
    let unknown = (context.prim.unknown, context.prim.unknown);

    if synonym.has_arguments() {
        if state.defer_synonym_expansion {
            let synonym_type = state.storage.intern(Type::Constructor(file_id, type_id));
            return (synonym_type, kind);
        } else {
            state.insert_error(ErrorKind::PartialSynonymApplication { id });
            return unknown;
        }
    }

    (synonym.synonym_type, kind)
}

pub fn parse_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: lowering::TypeId,
) -> Option<(FileId, TypeItemId, Synonym, TypeId)>
where
    Q: ExternalQueries,
{
    let &lowering::TypeKind::Constructor { resolution } =
        context.lowered.info.get_type_kind(function)?
    else {
        return None;
    };

    let (file_id, type_id) = resolution?;
    let (synonym, kind) = lookup_file_synonym(state, context, file_id, type_id)?;

    Some((file_id, type_id, synonym, kind))
}

pub fn infer_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
    (file_id, type_id, synonym, kind): (FileId, TypeItemId, Synonym, TypeId),
    arguments: &[lowering::TypeId],
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let unknown = (context.prim.unknown, context.prim.unknown);

    let expected_arity = synonym.type_variables.0 as usize;
    let actual_arity = arguments.len();

    if expected_arity != actual_arity {
        if state.defer_synonym_expansion {
            let (synonym_type, synonym_kind) =
                infer_partial_synonym_application(state, context, file_id, type_id, arguments);
            return (synonym_type, synonym_kind);
        } else {
            state.insert_error(ErrorKind::PartialSynonymApplication { id });
            return unknown;
        }
    }

    let defer_synonym_expansion = mem::replace(&mut state.defer_synonym_expansion, true);

    let (synonym_type, synonym_kind) =
        infer_synonym_application_arguments(state, context, (file_id, type_id), kind, arguments);

    state.defer_synonym_expansion = defer_synonym_expansion;

    (synonym_type, synonym_kind)
}

fn infer_synonym_application_arguments<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (file_id, type_id): (FileId, TypeItemId),
    kind: TypeId,
    arguments: &[lowering::TypeId],
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let mut argument_types = vec![];
    let mut synonym_kind = kind;

    for &argument_id in arguments {
        let (argument_type, result_kind) =
            infer_synonym_application_argument(state, context, synonym_kind, argument_id);

        argument_types.push(argument_type);
        synonym_kind = result_kind;
    }

    let synonym_type =
        state.storage.intern(Type::SynonymApplication(file_id, type_id, Arc::from(argument_types)));

    (synonym_type, synonym_kind)
}

/// Checks a single argument in a synonym application against the synonym's kind.
///
/// This mirrors [`kind::infer_surface_app_kind`] but for synonym applications.
///
/// It handles three cases:
/// - [`Type::Function`]: check argument against the domain kind
/// - [`Type::Unification`]: materialize a function type and check against it
/// - [`Type::Forall`]: instantiate the binder and recurse
fn infer_synonym_application_argument<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_k: TypeId,
    argument: lowering::TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let function_k = state.normalize_type(function_k);
    match state.storage[function_k] {
        Type::Function(argument_kind, result_kind) => {
            let (t, _) = kind::check_surface_kind(state, context, argument, argument_kind);
            let k = state.normalize_type(result_kind);

            (t, k)
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification_type(context);
            let result_u = state.fresh_unification_type(context);

            let function_u = state.storage.intern(Type::Function(argument_u, result_u));
            let _ = unification::solve(state, context, unification_id, function_u);

            let (t, _) = kind::check_surface_kind(state, context, argument, argument_u);
            let k = state.normalize_type(result_u);

            (t, k)
        }

        Type::Forall(ForallBinder { kind, .. }, function_k) => {
            let k = state.normalize_type(kind);
            let t = state.fresh_unification_kinded(k);

            let function_k = substitute::substitute_bound(state, t, function_k);
            infer_synonym_application_argument(state, context, function_k, argument)
        }

        _ => (context.prim.unknown, context.prim.unknown),
    }
}

fn infer_partial_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
    arguments: &[lowering::TypeId],
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let mut t = state.storage.intern(Type::Constructor(file_id, type_id));
    let mut k =
        kind::lookup_file_type(state, context, file_id, type_id).unwrap_or(context.prim.unknown);

    for &argument in arguments {
        (t, k) = kind::infer_surface_app_kind(state, context, (t, k), argument);
    }

    (t, k)
}

pub fn expand_type_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let Type::SynonymApplication(file_id, item_id, ref arguments) = state.storage[type_id] else {
        return type_id;
    };

    let arguments = Arc::clone(arguments);

    let Some((synonym, _)) = lookup_file_synonym(state, context, file_id, item_id) else {
        return type_id;
    };

    instantiate_synonym(state, synonym, &arguments)
}

fn instantiate_synonym(state: &mut CheckState, synonym: Synonym, arguments: &[TypeId]) -> TypeId {
    let count = synonym.quantified_variables.0 as usize + synonym.kind_variables.0 as usize;
    let mut instantiated = state.normalize_type(synonym.synonym_type);

    for _ in 0..count {
        if let Type::Forall(ForallBinder { kind, .. }, inner) = state.storage[instantiated] {
            let unification = state.fresh_unification_kinded(kind);
            instantiated = substitute::substitute_bound(state, unification, inner);
        } else {
            break;
        }
    }

    for &argument in arguments {
        if let Type::Forall(_, inner) = state.storage[instantiated] {
            instantiated = substitute::substitute_bound(state, argument, inner);
        } else {
            break;
        }
    }

    instantiated
}

pub fn normalize_expand_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut type_id: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    const EXPANSION_LIMIT: u32 = 1_000_000;

    for _ in 0..EXPANSION_LIMIT {
        let normalized = state.normalize_type(type_id);
        let expanded = expand_type_synonym(state, context, normalized);

        if expanded == type_id {
            return type_id;
        }

        type_id = expanded;
    }

    unreachable!("critical violation: limit reached in normalize_expand_type")
}
