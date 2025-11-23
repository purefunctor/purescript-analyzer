use std::iter;

use itertools::Itertools;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::check::{CheckContext, CheckState};
use crate::core::{ForallBinder, Type, TypeId, Variable};

pub fn type_to_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let default = context.prim.unknown;

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return default;
    };

    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let function = function.map_or(default, |id| type_to_core(state, context, id));
            arguments.iter().fold(function, |function, argument| {
                let argument = type_to_core(state, context, *argument);
                state.storage.intern(Type::Application(function, argument))
            })
        }
        lowering::TypeKind::Arrow { argument, result } => {
            let argument = argument.map_or(default, |id| type_to_core(state, context, id));
            let result = result.map_or(default, |id| type_to_core(state, context, id));
            state.storage.intern(Type::Function(argument, result))
        }
        lowering::TypeKind::Constrained { .. } => default,
        lowering::TypeKind::Constructor { resolution } => {
            let Some((file_id, type_id)) = *resolution else {
                return default;
            };
            state.storage.intern(Type::Constructor(file_id, type_id))
        }
        lowering::TypeKind::Forall { bindings, inner } => {
            let binders = bindings
                .iter()
                .map(|binding| convert_forall_binding(state, context, binding))
                .collect_vec();

            let inner = inner.map_or(default, |id| type_to_core(state, context, id));

            let forall = binders
                .into_iter()
                .rfold(inner, |inner, binder| state.storage.intern(Type::Forall(binder, inner)));

            if let Type::Forall(ForallBinder { level, .. }, _) = state.storage[forall] {
                state.unbind(level);
            }

            forall
        }
        lowering::TypeKind::Hole => default,
        lowering::TypeKind::Integer => default,
        lowering::TypeKind::Kinded { .. } => default,
        lowering::TypeKind::Operator { .. } => default,
        lowering::TypeKind::OperatorChain { .. } => default,
        lowering::TypeKind::String => default,
        lowering::TypeKind::Variable { name, resolution } => {
            let Some(resolution) = resolution else {
                let name = name.clone().unwrap_or(MISSING_NAME);
                let kind = Variable::Free(name);
                return state.storage.intern(Type::Variable(kind));
            };
            match resolution {
                lowering::TypeVariableResolution::Forall(forall) => {
                    convert_forall_resolution(state, *forall)
                }
                lowering::TypeVariableResolution::Implicit(implicit) => {
                    convert_implicit_resolution(state, context, implicit)
                }
            }
        }
        lowering::TypeKind::Wildcard => default,
        lowering::TypeKind::Record { .. } => default,
        lowering::TypeKind::Row { .. } => default,
        lowering::TypeKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else {
                return default;
            };
            type_to_core(state, context, *parenthesized)
        }
    }
}

/// A variant of [`type_to_core`] for use with signature declarations.
///
/// Unlike the regular [`type_to_core`], this function does not call
/// [`CheckState::unbind`] after each [`lowering::TypeKind::Forall`]
/// node. This allows type variables to be scoped for the entire
/// declaration group rather than just the type signature.
pub fn signature_type_to_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let default = context.prim.unknown;

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return default;
    };

    match kind {
        lowering::TypeKind::Forall { bindings, inner } => {
            let binders = bindings
                .iter()
                .map(|binding| convert_forall_binding(state, context, binding))
                .collect_vec();

            let inner = inner.map_or(default, |id| type_to_core(state, context, id));

            binders
                .into_iter()
                .rfold(inner, |inner, binder| state.storage.intern(Type::Forall(binder, inner)))
        }

        lowering::TypeKind::Parenthesized { parenthesized } => {
            parenthesized.map(|id| signature_type_to_core(state, context, id)).unwrap_or(default)
        }

        _ => type_to_core(state, context, id),
    }
}

pub struct InspectSignature {
    pub variables: Vec<ForallBinder>,
    pub arguments: Vec<TypeId>,
    pub result: TypeId,
}

pub fn inspect_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> InspectSignature
where
    Q: ExternalQueries,
{
    let unknown = || {
        let variables = [].into();
        let arguments = [].into();
        let result = context.prim.unknown;
        InspectSignature { variables, arguments, result }
    };

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return unknown();
    };

    match kind {
        lowering::TypeKind::Forall { bindings, inner } => {
            let variables = bindings
                .iter()
                .map(|binding| convert_forall_binding(state, context, binding))
                .collect();

            let inner = inner.map_or(context.prim.unknown, |id| type_to_core(state, context, id));
            let (arguments, result) = signature_components(state, inner);

            InspectSignature { variables, arguments, result }
        }

        lowering::TypeKind::Parenthesized { parenthesized } => {
            parenthesized.map(|id| inspect_signature(state, context, id)).unwrap_or_else(unknown)
        }

        _ => {
            let variables = [].into();

            let id = type_to_core(state, context, id);
            let (arguments, result) = signature_components(state, id);

            InspectSignature { variables, arguments, result }
        }
    }
}

fn signature_components(state: &mut CheckState, id: TypeId) -> (Vec<TypeId>, TypeId) {
    let mut components = iter::successors(Some(id), |&id| match state.storage[id] {
        Type::Function(_, id) => Some(id),
        _ => None,
    })
    .map(|id| match state.storage[id] {
        Type::Function(id, _) => id,
        _ => id,
    })
    .collect_vec();

    let Some(id) = components.pop() else {
        unreachable!("invariant violated: expected non-empty components");
    };

    (components, id)
}

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

pub fn convert_forall_binding<Q>(
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
        Some(id) => type_to_core(state, context, id),
        None => state.fresh_unification_type(context),
    };

    let level = state.bind_forall(binding.id, kind);
    ForallBinder { visible, name, level, kind }
}

fn convert_forall_resolution(
    state: &mut CheckState,
    id: lowering::TypeVariableBindingId,
) -> TypeId {
    let index = state.lookup_forall(id).expect("invariant violated: CheckState::bind_forall");
    let variable = Variable::Bound(index);
    state.storage.intern(Type::Variable(variable))
}

fn convert_implicit_resolution<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    implicit: &lowering::ImplicitTypeVariable,
) -> TypeId
where
    Q: ExternalQueries,
{
    if implicit.binding {
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
    }
}
