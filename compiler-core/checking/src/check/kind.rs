use crate::{
    check::{CheckContext, CheckState},
    core::{TypeId, storage::TypeStorage},
};

pub fn infer_surface_kind<S: TypeStorage>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    id: lowering::TypeId,
) -> (TypeId, TypeId) {
    let default = (context.prim.unknown, context.prim.unknown);

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return default;
    };

    match kind {
        lowering::TypeKind::ApplicationChain { .. } => default,
        lowering::TypeKind::Arrow { .. } => default,
        lowering::TypeKind::Constrained { .. } => default,
        lowering::TypeKind::Constructor { .. } => default,
        lowering::TypeKind::Forall { .. } => default,
        lowering::TypeKind::Hole => default,
        lowering::TypeKind::Integer => default,
        lowering::TypeKind::Kinded { .. } => default,
        lowering::TypeKind::Operator { .. } => default,
        lowering::TypeKind::OperatorChain { .. } => default,
        lowering::TypeKind::String => default,
        lowering::TypeKind::Variable { .. } => default,
        lowering::TypeKind::Wildcard => default,
        lowering::TypeKind::Record { .. } => default,
        lowering::TypeKind::Row { .. } => default,
        lowering::TypeKind::Parenthesized { parenthesized } => {
            let Some(parenthesized) = parenthesized else {
                return default;
            };
            infer_surface_kind(state, context, *parenthesized)
        }
    }
}
