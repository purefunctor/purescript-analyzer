//! Implements type signature inspection.
use std::iter;

use building_types::QueryResult;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::algorithm::kind;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::{ForallBinder, Type, TypeId};

pub struct InspectSignature {
    pub variables: Vec<ForallBinder>,
    pub function: TypeId,
    pub arguments: Vec<TypeId>,
    pub result: TypeId,
}

impl InspectSignature {
    pub fn restore(&self, state: &mut CheckState) -> TypeId {
        self.variables.iter().rfold(self.function, |inner, binder| {
            let binder = binder.clone();
            state.storage.intern(Type::Forall(binder, inner))
        })
    }
}

pub fn inspect_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> QueryResult<InspectSignature>
where
    Q: ExternalQueries,
{
    let unknown = || {
        let variables = [].into();
        let function = context.prim.unknown;
        let arguments = [].into();
        let result = context.prim.unknown;
        InspectSignature { variables, function, arguments, result }
    };

    let Some(kind) = context.lowered.info.get_type_kind(id) else {
        return Ok(unknown());
    };

    match kind {
        lowering::TypeKind::Forall { bindings, inner } => {
            let variables = bindings
                .iter()
                .map(|binding| kind::check_type_variable_binding(state, context, binding))
                .collect::<QueryResult<Vec<_>>>()?;

            let function = if let Some(function) = inner {
                let (function, _) =
                    kind::check_surface_kind(state, context, *function, context.prim.t)?;
                function
            } else {
                context.prim.unknown
            };

            let (arguments, result) = signature_components(state, function);

            Ok(InspectSignature { variables, function, arguments, result })
        }

        lowering::TypeKind::Parenthesized { parenthesized } => match parenthesized {
            Some(id) => inspect_signature(state, context, *id),
            None => Ok(unknown()),
        },

        _ => {
            let variables = [].into();

            let (function, _) = kind::check_surface_kind(state, context, id, context.prim.t)?;
            let (arguments, result) = signature_components(state, function);

            Ok(InspectSignature { variables, function, arguments, result })
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
