use building_types::QueryResult;

use crate::context::CheckContext;
use crate::core::{ForallBinder, Type, TypeId, normalise};
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

pub struct InspectSignature {
    pub binders: Vec<ForallBinder>,
    pub arguments: Vec<TypeId>,
    pub result: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InspectMode {
    Bindings,
    Patterns { required: usize },
}

fn inspect_signature_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut current: TypeId,
    mode: InspectMode,
) -> QueryResult<InspectSignature>
where
    Q: ExternalQueries,
{
    let mut binders = vec![];
    let mut arguments = vec![];

    safe_loop! {
        current = normalise::normalise_expand(state, context, current)?;

        match context.lookup_type(current) {
            Type::Forall(binder_id, inner) => {
                binders.push(context.lookup_forall_binder(binder_id));
                current = inner;
            }

            Type::Constrained(constraint, constrained) => {
                if matches!(mode, InspectMode::Patterns { .. }) {
                    state.push_given(constraint);
                    current = constrained;
                } else {
                    return Ok(InspectSignature { binders, arguments, result: current });
                }
            }

            Type::Function(argument, result) => {
                if let InspectMode::Patterns { required } = mode
                    && arguments.len() >= required
                {
                    return Ok(InspectSignature { binders, arguments, result: current });
                }

                arguments.push(argument);
                current = result;
            }

            Type::Application(function_argument, result) => {
                if let InspectMode::Patterns { required } = mode
                    && arguments.len() >= required
                {
                    return Ok(InspectSignature { binders, arguments, result: current });
                }

                let function_argument =
                    normalise::normalise_expand(state, context, function_argument)?;

                let Type::Application(function, argument) = context.lookup_type(function_argument)
                else {
                    return Ok(InspectSignature { binders, arguments, result: current });
                };

                let function = normalise::normalise_expand(state, context, function)?;
                if function == context.prim.function {
                    arguments.push(argument);
                    current = result;
                } else {
                    return Ok(InspectSignature { binders, arguments, result: current });
                }
            }

            _ => return Ok(InspectSignature { binders, arguments, result: current }),
        }
    }
}

pub fn inspect_signature_bindings<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_type: TypeId,
) -> QueryResult<InspectSignature>
where
    Q: ExternalQueries,
{
    inspect_signature_core(state, context, signature_type, InspectMode::Bindings)
}

pub fn inspect_signature_patterns<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_type: TypeId,
    required: usize,
) -> QueryResult<InspectSignature>
where
    Q: ExternalQueries,
{
    inspect_signature_core(state, context, signature_type, InspectMode::Patterns { required })
}
