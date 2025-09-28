use itertools::Itertools;

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
                function.map(|id| type_to_core(state, context, id)).unwrap_or(context.prim.unknown);
            arguments.iter().fold(function, |function, argument| {
                let argument = type_to_core(state, context, *argument);
                state.storage.intern(Type::Application(function, argument))
            })
        }
        lowering::TypeKind::Arrow { argument, result } => {
            let argument =
                argument.map(|id| type_to_core(state, context, id)).unwrap_or(context.prim.unknown);
            let result =
                result.map(|id| type_to_core(state, context, id)).unwrap_or(context.prim.unknown);
            state.storage.intern(Type::Function(argument, result))
        }
        lowering::TypeKind::Constrained { .. } => context.prim.unknown,
        lowering::TypeKind::Constructor { resolution } => {
            let Some((file_id, type_id)) = *resolution else { return context.prim.unknown };
            state.storage.intern(Type::Constructor(file_id, type_id))
        }
        lowering::TypeKind::Forall { bindings, type_ } => {
            let binders = bindings.iter().filter_map(|binding| {
                let visible = binding.visible;
                let name = binding.name.clone()?;
                let kind = binding
                    .kind
                    .map(|id| type_to_core(state, context, id))
                    .unwrap_or(context.prim.unknown);

                let level = state.bind_forall(binding.id, kind);
                Some(ForallBinder { visible, name, level, kind })
            });

            let binders = binders.collect_vec().into_iter();
            let inner =
                type_.map(|id| type_to_core(state, context, id)).unwrap_or(context.prim.unknown);

            let id = binders
                .rfold(inner, |inner, binder| state.storage.intern(Type::Forall(binder, inner)));

            if let Type::Forall(ForallBinder { level, .. }, _) = state.storage.index(id) {
                state.unbind(*level);
            }

            id
        }
        lowering::TypeKind::Hole => context.prim.unknown,
        lowering::TypeKind::Integer => context.prim.unknown,
        lowering::TypeKind::Kinded { .. } => context.prim.unknown,
        lowering::TypeKind::Operator { .. } => context.prim.unknown,
        lowering::TypeKind::OperatorChain { .. } => context.prim.unknown,
        lowering::TypeKind::String => context.prim.unknown,
        lowering::TypeKind::Variable { name, resolution } => {
            let Some(resolution) = resolution else {
                let name = name.clone();
                let kind = Variable::Free(name);
                return state.storage.intern(Type::Variable(kind));
            };
            match resolution {
                lowering::TypeVariableResolution::Forall(id) => {
                    let index = state
                        .lookup_forall(*id)
                        .expect("invariant violated: unbound type variable");
                    let kind = Variable::Bound(index);
                    state.storage.intern(Type::Variable(kind))
                }
                lowering::TypeVariableResolution::Implicit(lowering::ImplicitTypeVariable {
                    binding,
                    node,
                    id,
                }) => {
                    if *binding {
                        let level = state.bind_implicit(*node, *id);
                        let kind = Variable::Implicit(level);
                        state.storage.intern(Type::Variable(kind))
                    } else {
                        let index = state
                            .lookup_implicit(*node, *id)
                            .expect("invariant violated: unbound type variable");
                        let kind = Variable::Bound(index);
                        state.storage.intern(Type::Variable(kind))
                    }
                }
            }
        }
        lowering::TypeKind::Wildcard => context.prim.unknown,
        lowering::TypeKind::Record { .. } => context.prim.unknown,
        lowering::TypeKind::Row { .. } => context.prim.unknown,
        lowering::TypeKind::Parenthesized { parenthesized } => {
            parenthesized.map(|id| type_to_core(state, context, id)).unwrap_or(context.prim.unknown)
        }
    }
}
