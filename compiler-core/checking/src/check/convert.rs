use itertools::Itertools;
use smol_str::SmolStr;

use crate::{
    check::{CheckContext, CheckState},
    core::{ForallBinder, Type, TypeId, TypeStorage, Variable},
};

pub fn type_to_core<S>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    id: lowering::TypeId,
) -> TypeId
where
    S: TypeStorage,
{
    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return context.prim.unknown;
    };

    match kind {
        lowering::TypeKind::ApplicationChain { function, arguments } => {
            let function =
                function.map_or(context.prim.unknown, |id| type_to_core(state, context, id));
            arguments.iter().fold(function, |function, argument| {
                let argument = type_to_core(state, context, *argument);
                state.storage.intern(Type::Application(function, argument))
            })
        }
        lowering::TypeKind::Arrow { argument, result } => {
            let argument =
                argument.map_or(context.prim.unknown, |id| type_to_core(state, context, id));
            let result = result.map_or(context.prim.unknown, |id| type_to_core(state, context, id));
            state.storage.intern(Type::Function(argument, result))
        }
        lowering::TypeKind::Constrained { .. } => context.prim.unknown,
        lowering::TypeKind::Constructor { resolution } => {
            let Some((file_id, type_id)) = *resolution else {
                return context.prim.unknown;
            };
            state.storage.intern(Type::Constructor(file_id, type_id))
        }
        lowering::TypeKind::Forall { bindings, type_ } => {
            let binders = bindings
                .iter()
                .map(|binding| convert_forall_binding(state, context, binding))
                .collect_vec();

            let inner = type_.map_or(context.prim.unknown, |id| type_to_core(state, context, id));

            let forall = binders
                .into_iter()
                .rfold(inner, |inner, binder| state.storage.intern(Type::Forall(binder, inner)));

            if let Type::Forall(ForallBinder { level, .. }, _) = state.storage.index(forall) {
                state.unbind(*level);
            }

            forall
        }
        lowering::TypeKind::Hole => context.prim.unknown,
        lowering::TypeKind::Integer => context.prim.unknown,
        lowering::TypeKind::Kinded { .. } => context.prim.unknown,
        lowering::TypeKind::Operator { .. } => context.prim.unknown,
        lowering::TypeKind::OperatorChain { .. } => context.prim.unknown,
        lowering::TypeKind::String => context.prim.unknown,
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
                    convert_implicit_resolution(state, implicit)
                }
            }
        }
        lowering::TypeKind::Wildcard => context.prim.unknown,
        lowering::TypeKind::Record { .. } => context.prim.unknown,
        lowering::TypeKind::Row { .. } => context.prim.unknown,
        lowering::TypeKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else {
                return context.prim.unknown;
            };
            type_to_core(state, context, *parenthesized)
        }
    }
}

const INVALID_NAME: SmolStr = SmolStr::new_inline("<invalid>");

fn convert_forall_binding<S>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    binding: &lowering::TypeVariableBinding,
) -> ForallBinder
where
    S: TypeStorage,
{
    let visible = binding.visible;
    let name = binding.name.clone().unwrap_or(INVALID_NAME);

    let kind = match binding.kind {
        Some(id) => type_to_core(state, context, id),
        None => state.fresh_unification(),
    };

    let level = state.bind_forall(binding.id, kind);
    ForallBinder { visible, name, level, kind }
}

fn convert_forall_resolution<S>(
    state: &mut CheckState<S>,
    id: lowering::TypeVariableBindingId,
) -> TypeId
where
    S: TypeStorage,
{
    let index = state.lookup_forall(id).expect("invariant violated: CheckState::bind_forall");
    let variable = Variable::Bound(index);
    state.storage.intern(Type::Variable(variable))
}

fn convert_implicit_resolution<S: TypeStorage>(
    state: &mut CheckState<S>,
    implicit: &lowering::ImplicitTypeVariable,
) -> TypeId {
    if implicit.binding {
        let level = state.bind_implicit(implicit.node, implicit.id);
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
