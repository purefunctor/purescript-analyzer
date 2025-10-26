use itertools::Itertools;
use smol_str::SmolStr;

use crate::{
    ExternalQueries,
    check::{CheckContext, CheckState},
    core::{ForallBinder, Type, TypeId, Variable},
};

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
        lowering::TypeKind::Forall { bindings, type_ } => {
            let binders = bindings
                .iter()
                .map(|binding| convert_forall_binding(state, context, binding))
                .collect_vec();

            let inner = type_.map_or(default, |id| type_to_core(state, context, id));

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
                let name = name.clone().unwrap_or(INVALID_NAME);
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

const INVALID_NAME: SmolStr = SmolStr::new_inline("<invalid>");

fn convert_forall_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    binding: &lowering::TypeVariableBinding,
) -> ForallBinder
where
    Q: ExternalQueries,
{
    let visible = binding.visible;
    let name = binding.name.clone().unwrap_or(INVALID_NAME);

    let kind = match binding.kind {
        Some(id) => type_to_core(state, context, id),
        None => state.fresh_unification(context),
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
