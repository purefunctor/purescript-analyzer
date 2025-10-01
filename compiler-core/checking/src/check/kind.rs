use crate::{
    check::{CheckContext, CheckState, convert},
    core::{Type, TypeId, storage::TypeStorage},
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

        lowering::TypeKind::Arrow { .. } => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
            (t, k)
        }

        lowering::TypeKind::Constrained { .. } => default,

        lowering::TypeKind::Constructor { resolution } => {
            let Some((_, type_id)) = *resolution else { return default };
            let t = convert::type_to_core(state, context, id);
            let k = lookup_prim_type(state, context, type_id);
            (t, k)
        }

        lowering::TypeKind::Forall { .. } => {
            let t = convert::type_to_core(state, context, id);
            let k = context.prim.t;
            (t, k)
        },

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
            let Some(id) = parenthesized else { return default };
            infer_surface_kind(state, context, *id)
        }
    }
}

fn lookup_prim_type<S>(
    state: &mut CheckState<S>,
    context: &CheckContext,
    type_id: indexing::TypeItemId,
) -> TypeId
where
    S: TypeStorage,
{
    let item = &context.prim_indexed.items[type_id];

    let Some(name) = &item.name else {
        return context.prim.unknown;
    };

    let t_to_t = state.storage.intern(Type::Function(context.prim.t, context.prim.t));
    let row_t = state.storage.intern(Type::Application(context.prim.row, context.prim.t));

    match name.as_str() {
        "Type" => context.prim.t,
        "Function" => state.storage.intern(Type::Function(context.prim.t, t_to_t)),
        "Array" => t_to_t,
        "Record" => state.storage.intern(Type::Function(row_t, context.prim.t)),
        "Number" => context.prim.t,
        "Int" => context.prim.t,
        "String" => context.prim.t,
        "Char" => context.prim.t,
        "Boolean" => context.prim.t,
        "Partial" => context.prim.constraint,
        "Constraint" => context.prim.t,
        "Symbol" => context.prim.t,
        "Row" => t_to_t,
        _ => context.prim.unknown,
    }
}
