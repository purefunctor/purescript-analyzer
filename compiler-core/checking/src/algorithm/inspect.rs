//! Implements type signature inspection.
use std::mem;
use std::sync::Arc;

use building_types::QueryResult;
use lowering::TypeVariableBindingId;

use crate::ExternalQueries;
use crate::algorithm::kind::synonym;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::substitute;
use crate::core::{ForallBinder, Type, TypeId, Variable, debruijn};

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

pub fn collect_signature_variables<Q>(
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> Arc<[TypeVariableBindingId]>
where
    Q: ExternalQueries,
{
    fn aux<Q>(
        context: &CheckContext<Q>,
        id: lowering::TypeId,
        variables: &mut Vec<TypeVariableBindingId>,
    ) where
        Q: ExternalQueries,
    {
        let Some(kind) = context.lowered.info.get_type_kind(id) else {
            return;
        };

        match kind {
            lowering::TypeKind::Forall { bindings, inner } => {
                variables.extend(bindings.iter().map(|binding| binding.id));
                if let Some(inner_id) = inner {
                    aux(context, *inner_id, variables);
                }
            }
            lowering::TypeKind::Parenthesized { parenthesized: Some(parenthesized) } => {
                aux(context, *parenthesized, variables);
            }
            _ => (),
        }
    }

    let mut variables = vec![];
    aux(context, id, &mut variables);
    Arc::from(variables)
}

pub fn inspect_signature_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
    surface_bindings: &[TypeVariableBindingId],
) -> QueryResult<InspectSignature>
where
    Q: ExternalQueries,
{
    const INSPECTION_LIMIT: u32 = 1_000_000;

    let mut surface_bindings = surface_bindings.iter();
    let mut variables = vec![];
    let mut current_id = type_id;

    for _ in 0..INSPECTION_LIMIT {
        let expanded_id = synonym::normalize_expand_type(state, context, current_id)?;
        match state.storage[expanded_id] {
            // Foralls can and will have levels that are different from
            // when they were originally checked, such as when a Forall
            // appears on a type synonym. We rebind each type variable
            // to get the current level then substitute it within the
            // quantified type to ensure correct scoping.
            Type::Forall(ref binder, inner) => {
                let mut binder = binder.clone();

                let new_level = if let Some(&binding_id) = surface_bindings.next() {
                    state.bound.bind(debruijn::Variable::Forall(binding_id))
                } else {
                    state.bound.bind(debruijn::Variable::Core)
                };

                let old_level = mem::replace(&mut binder.level, new_level);

                state.kinds.insert(new_level, binder.kind);

                let variable = state.storage.intern(Type::Variable(Variable::Bound(new_level)));
                let inner = substitute::substitute_bound(state, old_level, variable, inner);

                variables.push(binder);

                current_id = inner;
            }

            Type::Constrained(_, constrained) => {
                current_id = constrained;
            }

            _ => {
                let function = expanded_id;
                let (arguments, result) = signature_components_core(state, context, expanded_id)?;
                return Ok(InspectSignature { variables, function, arguments, result });
            }
        }
    }

    unreachable!("critical violation: limit reached in inspect_signature_core")
}

fn signature_components_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> QueryResult<(Vec<TypeId>, TypeId)>
where
    Q: ExternalQueries,
{
    const INSPECTION_LIMIT: u32 = 1_000_000;

    let mut arguments = vec![];
    let mut current_id = type_id;

    for _ in 0..INSPECTION_LIMIT {
        let expanded = synonym::normalize_expand_type(state, context, current_id)?;
        match state.storage[expanded] {
            Type::Function(argument_id, return_id) => {
                arguments.push(argument_id);
                current_id = return_id;
            }

            Type::Forall(ref binder, inner) => {
                let binder_level = binder.level;
                let binder_kind = binder.kind;

                let level = state.bound.bind(debruijn::Variable::Core);
                state.kinds.insert(level, binder_kind);

                let variable = state.storage.intern(Type::Variable(Variable::Bound(level)));
                current_id = substitute::substitute_bound(state, binder_level, variable, inner);
            }

            _ => {
                return Ok((arguments, expanded));
            }
        }
    }

    unreachable!("critical violation: limit reached in signature_components_core")
}
