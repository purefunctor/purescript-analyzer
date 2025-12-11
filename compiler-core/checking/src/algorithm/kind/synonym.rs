//! Synonym-related kind checking.
//!
//! This module handles the kind checking of type synonyms, including:
//! - Looking up synonyms from local and external modules
//! - Checking partial vs full synonym applications
//! - Instantiating and applying synonym type arguments

use files::FileId;
use indexing::TypeItemId;

use crate::algorithm::kind::{check_surface_kind, elaborate_kind};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::substitute::substitute_bound;
use crate::algorithm::{kind, transfer};
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
    file_id: FileId,
    type_id: TypeItemId,
    synonym: Synonym,
    kind: TypeId,
    id: lowering::TypeId,
) -> (TypeId, TypeId) {
    if synonym.has_arguments() {
        if state.allow_partial_synonym {
            let t = state.storage.intern(Type::Constructor(file_id, type_id));
            return (t, kind);
        }
        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        return (context.prim.unknown, context.prim.unknown);
    }
    (synonym.synonym_type, kind)
}

fn instantiate_count(state: &mut CheckState, mut type_id: TypeId, count: usize) -> TypeId {
    for _ in 0..count {
        if let Type::Forall(ForallBinder { kind, .. }, inner) = state.storage[type_id] {
            let unification = state.fresh_unification_kinded(kind);
            type_id = substitute_bound(state, unification, inner);
        } else {
            break;
        }
    }
    type_id
}

fn apply_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    inner_id: TypeId,
    argument_id: lowering::TypeId,
) -> Option<TypeId>
where
    Q: ExternalQueries,
{
    if let Type::Forall(ForallBinder { kind, .. }, inner) = state.storage[inner_id] {
        let previous = std::mem::replace(&mut state.allow_partial_synonym, true);
        let (argument_type, _) = check_surface_kind(state, context, argument_id, kind);
        state.allow_partial_synonym = previous;
        Some(substitute_bound(state, argument_type, inner))
    } else {
        None
    }
}

pub fn parse_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: lowering::TypeId,
) -> Option<Synonym>
where
    Q: ExternalQueries,
{
    let &lowering::TypeKind::Constructor { resolution } =
        context.lowered.info.get_type_kind(function)?
    else {
        return None;
    };

    let (file_id, type_id) = resolution?;
    let (synonym, _) = lookup_file_synonym(state, context, file_id, type_id)?;

    Some(synonym)
}

pub fn infer_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
    function: lowering::TypeId,
    synonym: Synonym,
    arguments: &[lowering::TypeId],
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let unknown = (context.prim.unknown, context.prim.unknown);

    let expected_arity = synonym.type_variables.0 as usize;
    if expected_arity != arguments.len() {
        if state.allow_partial_synonym {
            return infer_partial_synonym_application(state, context, function, arguments);
        }
        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        return unknown;
    }

    let to_instantiate = state.normalize_type(synonym.synonym_type);
    let count = synonym.quantified_variables.0 as usize + synonym.kind_variables.0 as usize;

    let mut result_type = instantiate_count(state, to_instantiate, count);

    for &argument_id in arguments {
        if let Some(applied_type) = apply_synonym(state, context, result_type, argument_id) {
            result_type = applied_type;
        } else {
            return unknown;
        };
    }

    let result_kind = elaborate_kind(state, context, result_type);

    (result_type, result_kind)
}

fn infer_partial_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function: lowering::TypeId,
    arguments: &[lowering::TypeId],
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let (mut t, mut k) = kind::infer_surface_kind(state, context, function);

    for &argument in arguments {
        (t, k) = kind::infer_surface_app_kind(state, context, (t, k), argument);
    }

    (t, k)
}
