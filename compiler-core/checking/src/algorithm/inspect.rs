//! Implements type signature inspection.
use std::iter;

use itertools::Itertools;

use crate::ExternalQueries;
use crate::algorithm::kind;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::{ForallBinder, Type, TypeId};

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
                .map(|binding| kind::check_type_variable_binding(state, context, binding))
                .collect();

            let inner = inner.map_or(context.prim.unknown, |id| {
                let (inner, _) = kind::check_surface_kind(state, context, id, context.prim.t);
                inner
            });
            let (arguments, result) = signature_components(state, inner);

            InspectSignature { variables, arguments, result }
        }

        lowering::TypeKind::Parenthesized { parenthesized } => {
            parenthesized.map(|id| inspect_signature(state, context, id)).unwrap_or_else(unknown)
        }

        _ => {
            let variables = [].into();

            let (id, _) = kind::check_surface_kind(state, context, id, context.prim.t);
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
