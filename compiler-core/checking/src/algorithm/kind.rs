//! Implements the kind checker.
//!
//! This module is organized into submodules:
//! - [`operator`]: Kind checking for type operator chains
//! - [`synonym`]: Kind checking for type synonym applications

mod operator;
mod synonym;

use files::FileId;
use indexing::TypeItemId;
use itertools::Itertools;
use lowering::TypeVariableBindingId;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{substitute, transfer, unification};
use crate::core::{ForallBinder, Type, TypeId, Variable};

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

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

            if let Some(synonym) = synonym::parse_synonym_application(state, context, *function) {
                return synonym::infer_synonym_application(state, context, id, synonym, arguments);
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

            if let Some((s, k)) = synonym::lookup_file_synonym(state, context, file_id, type_id) {
                return synonym::infer_synonym_constructor(state, context, s, k, id);
            }

            let t = state.storage.intern(Type::Constructor(file_id, type_id));
            let k =
                lookup_file_type(state, context, file_id, type_id).unwrap_or(context.prim.unknown);

            (t, k)
        }

        lowering::TypeKind::Forall { bindings, inner } => {
            let binders = bindings
                .iter()
                .map(|binding| check_type_variable_binding(state, context, binding))
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
            let t = context.prim.unknown;
            let k = state.fresh_unification(context);
            (t, k)
        }

        lowering::TypeKind::Integer { value } => {
            let Some(value) = value else { return default };

            let t = state.storage.intern(Type::Integer(*value));
            let k = context.prim.int;

            (t, k)
        }

        lowering::TypeKind::Kinded { type_, kind } => {
            let Some(type_) = type_ else { return default };
            let Some(kind) = kind else { return default };

            let (k, _) = infer_surface_kind(state, context, *kind);
            let (t, _) = check_surface_kind(state, context, *type_, k);

            (t, k)
        }

        lowering::TypeKind::Operator { resolution } => {
            let Some((file_id, type_id)) = *resolution else { return default };

            let t = state.storage.intern(Type::Operator(file_id, type_id));
            let k =
                lookup_file_type(state, context, file_id, type_id).unwrap_or(context.prim.unknown);

            (t, k)
        }

        lowering::TypeKind::OperatorChain { .. } => {
            operator::infer_operator_chain_kind(state, context, id)
        }

        lowering::TypeKind::String { kind, value } => {
            let value = value.clone().unwrap_or(MISSING_NAME);

            let t = state.storage.intern(Type::String(*kind, value));
            let k = context.prim.symbol;

            (t, k)
        }

        lowering::TypeKind::Variable { name, resolution } => match resolution {
            Some(lowering::TypeVariableResolution::Forall(forall)) => {
                infer_forall_variable(state, *forall)
            }

            Some(lowering::TypeVariableResolution::Implicit(implicit)) => {
                infer_implicit_variable(state, context, implicit)
            }

            None => {
                let name = name.clone().unwrap_or(MISSING_NAME);

                let t = state.storage.intern(Type::Variable(Variable::Free(name)));
                let k = state.fresh_unification(context);

                (t, k)
            }
        },

        lowering::TypeKind::Wildcard => default,

        lowering::TypeKind::Record { .. } => default,

        lowering::TypeKind::Row { .. } => default,

        lowering::TypeKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else { return default };
            infer_surface_kind(state, context, *parenthesized)
        }
    }
}

fn infer_forall_variable(
    state: &mut CheckState,
    forall: TypeVariableBindingId,
) -> (TypeId, TypeId) {
    let index = state.lookup_forall(forall).expect("invariant violated: CheckState::bind_forall");
    let variable = Variable::Bound(index);

    let t = state.storage.intern(Type::Variable(variable));
    let k = state.forall_binding_kind(forall).expect("invariant violated: CheckState::bind_forall");

    (t, k)
}

fn infer_implicit_variable<Q: ExternalQueries>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    implicit: &lowering::ImplicitTypeVariable,
) -> (TypeId, TypeId) {
    let t = if implicit.binding {
        let kind = state.fresh_unification(context);

        let level = state.bind_implicit(implicit.node, implicit.id, kind);
        let variable = Variable::Implicit(level);

        state.storage.intern(Type::Variable(variable))
    } else {
        let index = state
            .lookup_implicit(implicit.node, implicit.id)
            .expect("invariant violated: CheckState::bind_implicit");
        let variable = Variable::Bound(index);
        state.storage.intern(Type::Variable(variable))
    };

    let k = state
        .implicit_binding_kind(implicit.node, implicit.id)
        .expect("invariant violated: CheckState::bind_implicit");

    (t, k)
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
            let function_k = substitute::substitute_bound(state, t, function_k);

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

        Type::OperatorApplication(file_id, type_id, _, _) => {
            operator::elaborate_operator_application_kind(state, context, file_id, type_id)
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

pub fn check_type_variable_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binding: &lowering::TypeVariableBinding,
) -> ForallBinder
where
    Q: ExternalQueries,
{
    let visible = binding.visible;
    let name = binding.name.clone().unwrap_or(MISSING_NAME);

    let kind = match binding.kind {
        Some(id) => {
            let (kind, _) = check_surface_kind(state, context, id, context.prim.t);
            kind
        }
        None => state.fresh_unification_type(context),
    };

    let level = state.bind_forall(binding.id, kind);
    ForallBinder { visible, name, level, kind }
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
