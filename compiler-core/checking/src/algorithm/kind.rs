//! Implements the kind checker.

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::substitute::substitute_bound;
use crate::algorithm::{convert, substitute, transfer, unification};
use crate::core::{ForallBinder, Type, TypeId, Variable};

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
            let Some(function) = function else { return default };

            let (mut t, mut k) = infer_surface_kind(state, context, *function);

            for argument in arguments.iter() {
                (t, k) = infer_surface_app_kind(state, context, (t, k), *argument)
            }

            (t, k)
        }

        lowering::TypeKind::Arrow { .. } => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
            (t, k)
        }

        lowering::TypeKind::Constrained { .. } => default,

        lowering::TypeKind::Constructor { resolution } => {
            let Some((file_id, type_id)) = *resolution else { return default };

            if file_id == context.id {
                let t = convert::type_to_core(state, context, id);
                if let Some(&k) = state.binding_group.types.get(&type_id) {
                    (t, k)
                } else if let Some(&k) = state.checked.types.get(&type_id) {
                    (t, transfer::localize(state, context, k))
                } else {
                    default
                }
            } else {
                let Ok(checked) = context.queries.checked(file_id) else { return default };
                let Some(global_id) = checked.lookup_type(type_id) else { return default };

                let t = convert::type_to_core(state, context, id);
                let k = transfer::localize(state, context, global_id);

                (t, k)
            }
        }

        lowering::TypeKind::Forall { .. } => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
            (t, k)
        }

        lowering::TypeKind::Hole => {
            let t = convert::type_to_core(state, context, id);
            let k = state.fresh_unification(context);
            (t, k)
        }

        lowering::TypeKind::Integer => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.int;
            (t, k)
        }

        lowering::TypeKind::Kinded { type_, kind } => {
            let Some(type_) = type_ else { return default };
            let Some(kind) = kind else { return default };

            let t = convert::type_to_core(state, context, *type_);
            let k = convert::type_to_core(state, context, *kind);

            (t, k)
        }

        lowering::TypeKind::Operator { .. } => default,

        lowering::TypeKind::OperatorChain { .. } => default,

        lowering::TypeKind::String => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
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

        Type::Constructor(file_id, type_id) => {
            if file_id == context.id {
                if let Some(&k) = state.binding_group.types.get(&type_id) {
                    k
                } else if let Some(&k) = state.checked.types.get(&type_id) {
                    k
                } else {
                    default
                }
            } else {
                let Ok(checked) = context.queries.checked(file_id) else { return default };
                let Some(global_id) = checked.types.get(&type_id) else { return default };
                transfer::localize(state, context, *global_id)
            }
        }

        Type::Forall(_, _) => context.prim.t,

        Type::Function(_, _) => context.prim.t,

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
