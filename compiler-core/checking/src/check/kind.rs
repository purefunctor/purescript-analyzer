use crate::{
    check::state::{CheckContext, CheckState},
    core::{TypeId, storage::TypeStorage},
};

pub fn infer_surface_kind<S: TypeStorage>(
    state: &mut CheckState<S>,
    context: CheckContext,
    id: lowering::TypeId,
) -> (TypeId, TypeId) {
    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        let unknown = state.storage.unknown();
        return (unknown, unknown);
    };
    match kind {
        lowering::TypeKind::ApplicationChain { .. } => todo!(),
        lowering::TypeKind::Arrow { .. } => todo!(),
        lowering::TypeKind::Constrained { .. } => todo!(),
        lowering::TypeKind::Constructor { .. } => todo!(),
        lowering::TypeKind::Forall { .. } => todo!(),
        lowering::TypeKind::Hole => todo!(),
        lowering::TypeKind::Integer => todo!(),
        lowering::TypeKind::Kinded { .. } => todo!(),
        lowering::TypeKind::Operator { .. } => todo!(),
        lowering::TypeKind::OperatorChain { .. } => todo!(),
        lowering::TypeKind::String => todo!(),
        lowering::TypeKind::Variable { .. } => todo!(),
        lowering::TypeKind::Wildcard => todo!(),
        lowering::TypeKind::Record { .. } => todo!(),
        lowering::TypeKind::Row { .. } => todo!(),
        lowering::TypeKind::Parenthesized { .. } => todo!(),
    }
}
