//! Implements the kind checker.

use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;

use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::substitute::substitute_bound;
use crate::algorithm::{convert, substitute, transfer, unification};
use crate::core::{ForallBinder, Synonym, Type, TypeId, Variable};
use crate::error::ErrorKind;
use crate::{CheckedModule, ExternalQueries};

pub fn infer_surface_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let default = (context.prim.unknown, context.prim.unknown);

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return default;
    };

    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let Some(function) = function else {
                return default;
            };

            if let Some(synonym) = parse_synonym_application(state, context, *function) {
                return infer_synonym_application(state, context, id, synonym, arguments);
            }

            let (mut t, mut k) = infer_surface_kind(state, context, *function);

            for argument in arguments.iter() {
                (t, k) = infer_surface_app_kind(state, context, (t, k), *argument)
            }

            (t, k)
        }

        lowering::TypeKind::Arrow { argument, result } => {
            let argument = argument.map(|argument| {
                let (argument, _) = check_surface_kind(state, context, argument, context.prim.t);
                argument
            });

            let result = result.map(|result| {
                let (result, _) = check_surface_kind(state, context, result, context.prim.t);
                result
            });

            let argument = argument.unwrap_or(context.prim.unknown);
            let result = result.unwrap_or(context.prim.unknown);

            let t = state.storage.intern(Type::Function(argument, result));
            let k = context.prim.t;

            (t, k)
        }

        lowering::TypeKind::Constrained { .. } => default,

        lowering::TypeKind::Constructor { resolution } => {
            let Some((file_id, type_id)) = *resolution else { return default };

            if let Some((s, k)) = lookup_synonym_by_file(state, context, file_id, type_id) {
                return infer_synonym_constructor(state, context, s, k, id);
            }

            let t = convert::type_to_core(state, context, id);
            let k =
                lookup_file_type(state, context, file_id, type_id).unwrap_or(context.prim.unknown);

            (t, k)
        }

        lowering::TypeKind::Forall { bindings, inner } => {
            let binders = bindings
                .iter()
                .map(|binding| convert::convert_forall_binding(state, context, binding))
                .collect_vec();

            let inner = inner.map(|inner| {
                let (inner, _) = check_surface_kind(state, context, inner, context.prim.t);
                inner
            });

            let inner = inner.unwrap_or(context.prim.unknown);

            let t = binders.iter().rfold(inner, |inner, binder| {
                let binder = binder.clone();
                state.storage.intern(Type::Forall(binder, inner))
            });

            let k = context.prim.t;

            if let Some(binder) = binders.first() {
                state.unbind(binder.level);
            }

            (t, k)
        }

        lowering::TypeKind::Hole => {
            let t = convert::type_to_core(state, context, id);
            let k = state.fresh_unification(context);
            (t, k)
        }

        lowering::TypeKind::Integer { .. } => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.int;
            (t, k)
        }

        lowering::TypeKind::Kinded { type_, kind } => {
            let Some(type_) = type_ else { return default };
            let Some(kind) = kind else { return default };

            let k = convert::type_to_core(state, context, *kind);
            let (t, _) = check_surface_kind(state, context, *type_, k);

            (t, k)
        }

        lowering::TypeKind::Operator { resolution } => {
            let Some((file_id, type_id)) = *resolution else { return default };

            let t = convert::type_to_core(state, context, id);
            let k =
                lookup_file_type(state, context, file_id, type_id).unwrap_or(context.prim.unknown);

            (t, k)
        }

        lowering::TypeKind::OperatorChain { .. } => default,

        lowering::TypeKind::String { .. } => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.symbol;
            (t, k)
        }

        lowering::TypeKind::Variable { resolution, .. } => {
            let t = convert::type_to_core(state, context, id);

            let k = match resolution {
                Some(lowering::TypeVariableResolution::Forall(forall)) => state
                    .forall_binding_kind(*forall)
                    .expect("invariant violated: CheckState::bind_forall"),
                Some(lowering::TypeVariableResolution::Implicit(implicit)) => state
                    .implicit_binding_kind(implicit.node, implicit.id)
                    .expect("invariant violated: CheckState::bind_implicit"),
                None => state.fresh_unification(context),
            };

            (t, k)
        }

        lowering::TypeKind::Wildcard => default,

        lowering::TypeKind::Record { .. } => default,

        lowering::TypeKind::Row { .. } => default,

        lowering::TypeKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return default };
            infer_surface_kind(state, context, *parenthesized)
        }
    }
}

pub fn infer_surface_app_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (function_t, function_k): (TypeId, TypeId),
    argument: lowering::TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let function_k = state.normalize_type(function_k);
    match state.storage[function_k] {
        Type::Function(argument_k, result_k) => {
            let (argument_t, _) = check_surface_kind(state, context, argument, argument_k);

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            let k = state.normalize_type(result_k);

            (t, k)
        }

        Type::Unification(unification_id) => {
            let argument_u = state.fresh_unification_type(context);
            let result_u = state.fresh_unification_type(context);

            let function_u = state.storage.intern(Type::Function(argument_u, result_u));
            let _ = unification::solve(state, context, unification_id, function_u);

            let (argument_t, _) = check_surface_kind(state, context, argument, argument_u);

            let t = state.storage.intern(Type::Application(function_t, argument_t));
            let k = state.normalize_type(result_u);

            (t, k)
        }

        Type::Forall(ForallBinder { kind, .. }, function_k) => {
            let k = state.normalize_type(kind);
            let t = state.fresh_unification_kinded(k);

            let function_t = state.storage.intern(Type::KindApplication(function_t, t));
            let function_k = substitute_bound(state, t, function_k);

            infer_surface_app_kind(state, context, (function_t, function_k), argument)
        }

        _ => (context.prim.unknown, context.prim.unknown),
    }
}

pub fn elaborate_kind<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> TypeId
where
    Q: ExternalQueries,
{
    let default = context.prim.unknown;
    let id = state.normalize_type(id);
    match state.storage[id] {
        Type::Application(function, _) => {
            let function_kind = elaborate_kind(state, context, function);
            let function_kind = state.normalize_type(function_kind);
            match state.storage[function_kind] {
                Type::Function(_, result) => result,

                Type::Unification(unification_id) => {
                    let domain = state.unification.get(unification_id).domain;

                    let argument_u = state.fresh_unification_kinded_at(domain, context.prim.t);
                    let result_u = state.fresh_unification_kinded_at(domain, context.prim.t);
                    let function = state.storage.intern(Type::Function(argument_u, result_u));

                    let _ = unification::solve(state, context, unification_id, function);
                    result_u
                }

                _ => default,
            }
        }

        Type::Constrained(_, _) => context.prim.t,

        Type::Constructor(file_id, type_id) => {
            lookup_file_type(state, context, file_id, type_id).unwrap_or(default)
        }

        Type::Forall(_, _) => context.prim.t,

        Type::Function(_, _) => context.prim.t,

        Type::Integer(_) => context.prim.int,

        Type::KindApplication(function, argument) => {
            let function_kind = elaborate_kind(state, context, function);
            let function_kind = state.normalize_type(function_kind);
            match state.storage[function_kind] {
                Type::Forall(_, inner) => {
                    let argument = state.normalize_type(argument);
                    substitute::substitute_bound(state, argument, inner)
                }
                _ => default,
            }
        }

        Type::Kinded(_, kind) => kind,

        Type::Operator(file_id, type_id) => {
            lookup_file_type(state, context, file_id, type_id).unwrap_or(default)
        }

        Type::String(_, _) => context.prim.symbol,

        Type::Unification(unification_id) => state.unification.get(unification_id).kind,

        Type::Variable(ref variable) => match variable {
            Variable::Implicit(_) => default,
            Variable::Skolem(_, kind) => *kind,
            Variable::Bound(index) => state.core_kind(*index).unwrap_or(default),
            Variable::Free(_) => default,
        },

        Type::Unknown => default,
    }
}

pub fn check_surface_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
    kind: TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let (inferred_type, inferred_kind) = infer_surface_kind(state, context, id);
    unification::subsumes(state, context, inferred_kind, kind);
    (inferred_type, inferred_kind)
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

fn lookup_synonym_by_file<Q>(
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

pub(crate) fn lookup_file_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> Option<TypeId>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        if let Some(&k) = state.binding_group.types.get(&type_id) {
            Some(k)
        } else if let Some(&k) = state.checked.types.get(&type_id) {
            Some(transfer::localize(state, context, k))
        } else {
            None
        }
    } else {
        let checked = context.queries.checked(file_id).ok()?;
        let global_id = checked.types.get(&type_id)?;
        Some(transfer::localize(state, context, *global_id))
    }
}

fn infer_synonym_constructor<Q: ExternalQueries>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    synonym: Synonym,
    kind: TypeId,
    id: lowering::TypeId,
) -> (TypeId, TypeId) {
    if synonym.has_arguments() {
        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        return (context.prim.unknown, context.prim.unknown);
    }
    (synonym.synonym_type, kind)
}

fn synonym_instantiate(state: &mut CheckState, mut type_id: TypeId, count: usize) -> TypeId {
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

fn synonym_apply_forall<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    inner_ty: TypeId,
    argument_id: lowering::TypeId,
) -> Option<TypeId>
where
    Q: ExternalQueries,
{
    if let Type::Forall(ForallBinder { kind, .. }, inner) = state.storage[inner_ty] {
        let (argument_type, _) = check_surface_kind(state, context, argument_id, kind);
        Some(substitute_bound(state, argument_type, inner))
    } else {
        None
    }
}

fn parse_synonym_application<Q>(
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
    let (synonym, _) = lookup_synonym_by_file(state, context, file_id, type_id)?;

    Some(synonym)
}

fn infer_synonym_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
    synonym: Synonym,
    arguments: &[lowering::TypeId],
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let default = (context.prim.unknown, context.prim.unknown);

    let expected_arity = synonym.type_variables.0 as usize;
    if expected_arity != arguments.len() {
        state.insert_error(ErrorKind::PartialSynonymApplication { id });
        return default;
    }

    let instantiated = state.normalize_type(synonym.synonym_type);

    let to_instantiate =
        synonym.quantified_variables.0 as usize + synonym.kind_variables.0 as usize;

    let mut result_type = synonym_instantiate(state, instantiated, to_instantiate);

    for &argument_id in arguments {
        if let Some(applied_type) = synonym_apply_forall(state, context, result_type, argument_id) {
            result_type = applied_type;
        } else {
            return default;
        };
    }

    let result_kind = elaborate_kind(state, context, result_type);

    (result_type, result_kind)
}
