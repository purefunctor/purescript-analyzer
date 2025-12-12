//! Synonym-related kind checking.
//!
//! This module handles the kind checking of type synonyms, including:
//! - Looking up synonyms from local and external modules
//! - Checking partial vs full synonym applications
//! - Instantiating and applying synonym type arguments

use std::sync::Arc;
use std::{iter, mem};

use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;

use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, substitute, transfer, unification};
use crate::core::{ForallBinder, Saturation, Synonym, Type, TypeId};
use crate::error::ErrorKind;
use crate::{CheckedModule, ExternalQueries};

fn is_recursive_synonym<Q>(context: &CheckContext<Q>, file_id: FileId, item_id: TypeItemId) -> bool
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        context.lowered.errors.iter().any(|error| {
            if let lowering::LoweringError::RecursiveSynonym(recursive) = error {
                recursive.group.contains(&item_id)
            } else {
                false
            }
        })
    } else {
        let Ok(lowered) = context.queries.lowered(file_id) else {
            return true;
        };

        lowered.errors.iter().any(|error| {
            if let lowering::LoweringError::RecursiveSynonym(recursive) = error {
                recursive.group.contains(&item_id)
            } else {
                false
            }
        })
    }
}

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
            let (synonym_type, synonym_kind) = infer_partial_synonym_application(
                state,
                context,
                (file_id, type_id),
                kind,
                arguments,
            );
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

    let synonym_type = state.storage.intern(Type::SynonymApplication(
        Saturation::Full,
        file_id,
        type_id,
        Arc::from(argument_types),
    ));

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

    let synonym_type = state.storage.intern(Type::SynonymApplication(
        Saturation::Partial,
        file_id,
        type_id,
        Arc::from(argument_types),
    ));

    (synonym_type, synonym_kind)
}

enum DiscoveredSynonym {
    Saturated { synonym: Synonym, arguments: Vec<TypeId> },
    Additional { synonym: Synonym, arguments: Arc<[TypeId]>, additional: Vec<TypeId> },
}

fn discover_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> Option<DiscoveredSynonym>
where
    Q: ExternalQueries,
{
    let mut additional = vec![];
    let mut current_id = type_id;

    while let Type::Application(function, argument) | Type::KindApplication(function, argument) =
        state.storage[current_id]
    {
        additional.push(argument);
        current_id = function;
    }

    additional.reverse();

    match state.storage[current_id] {
        Type::Constructor(file_id, item_id) => {
            let (synonym, _) = lookup_file_synonym(state, context, file_id, item_id)?;

            let actual_arity = additional.len();
            let expected_arity = synonym.type_variables.0 as usize;

            if actual_arity != expected_arity {
                return None;
            }

            if is_recursive_synonym(context, file_id, item_id) {
                state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, item_id });
                return None;
            }

            let arguments = additional;
            Some(DiscoveredSynonym::Saturated { synonym, arguments })
        }

        Type::SynonymApplication(Saturation::Partial, file_id, item_id, ref partial_arguments) => {
            let partial_arguments = Arc::clone(partial_arguments);
            let (synonym, _) = lookup_file_synonym(state, context, file_id, item_id)?;

            let arguments = {
                let partial = partial_arguments.iter().copied();
                let additional = additional.into_iter();
                iter::chain(partial, additional).collect_vec()
            };

            let actual_arity = arguments.len();
            let expected_arity = synonym.type_variables.0 as usize;

            if actual_arity != expected_arity {
                return None;
            }

            if is_recursive_synonym(context, file_id, item_id) {
                state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, item_id });
                return None;
            }

            Some(DiscoveredSynonym::Saturated { synonym, arguments })
        }

        Type::SynonymApplication(Saturation::Full, file_id, item_id, ref full_arguments) => {
            let arguments = Arc::clone(full_arguments);
            let (synonym, _) = lookup_file_synonym(state, context, file_id, item_id)?;

            if is_recursive_synonym(context, file_id, item_id) {
                state.insert_error(ErrorKind::RecursiveSynonymExpansion { file_id, item_id });
                return None;
            }

            if additional.is_empty() {
                let arguments = arguments.to_vec();
                Some(DiscoveredSynonym::Saturated { synonym, arguments })
            } else {
                Some(DiscoveredSynonym::Additional { synonym, arguments, additional })
            }
        }

        _ => None,
    }
}

fn instantiate_saturated(state: &mut CheckState, synonym: Synonym, arguments: &[TypeId]) -> TypeId {
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

pub fn expand_type_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let Some(discovered) = discover_synonym_application(state, context, type_id) else {
        return type_id;
    };

    match discovered {
        DiscoveredSynonym::Saturated { synonym, arguments } => {
            instantiate_saturated(state, synonym, &arguments)
        }
        DiscoveredSynonym::Additional { synonym, arguments, additional } => {
            let expanded = instantiate_saturated(state, synonym, &arguments);
            additional.into_iter().fold(expanded, |result, argument| {
                state.storage.intern(Type::Application(result, argument))
            })
        }
    }
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
        let normalized_id = state.normalize_type(type_id);
        let expanded_id = expand_type_synonym(state, context, normalized_id);

        if expanded_id == type_id {
            return type_id;
        }

        type_id = expanded_id;
    }

    unreachable!("critical violation: limit reached in normalize_expand_type")
}
