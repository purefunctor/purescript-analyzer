use building_types::QueryResult;
use lowering::TypeVariableBinding;

use crate::context::CheckContext;
use crate::core::{ForallBinder, Type, TypeId, normalise};
use crate::error::ErrorKind;
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

pub struct DecomposedSignature {
    pub binders: Vec<ForallBinder>,
    pub constraints: Vec<TypeId>,
    pub arguments: Vec<TypeId>,
    pub result: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecomposeSignatureMode {
    Full,
    Patterns { required: usize },
}

pub fn decompose_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut current: TypeId,
    mode: DecomposeSignatureMode,
) -> QueryResult<DecomposedSignature>
where
    Q: ExternalQueries,
{
    let mut binders = vec![];
    let mut constraints = vec![];
    let mut arguments = vec![];

    safe_loop! {
        current = normalise::normalise_expand(state, context, current)?;

        match context.lookup_type(current) {
            Type::Forall(binder_id, inner) => {
                binders.push(context.lookup_forall_binder(binder_id));
                current = inner;
            }

            Type::Constrained(constraint, constrained) => {
                constraints.push(constraint);
                current = constrained;
            }

            Type::Function(argument, result) => {
                if let DecomposeSignatureMode::Patterns { required } = mode
                    && arguments.len() >= required
                {
                    return Ok(DecomposedSignature { binders, constraints, arguments, result: current });
                }

                arguments.push(argument);
                current = result;
            }

            Type::Application(function_argument, result) => {
                if let DecomposeSignatureMode::Patterns { required } = mode
                    && arguments.len() >= required
                {
                    return Ok(DecomposedSignature { binders, constraints, arguments, result: current });
                }

                let function_argument =
                    normalise::normalise_expand(state, context, function_argument)?;

                let Type::Application(function, argument) = context.lookup_type(function_argument)
                else {
                    return Ok(DecomposedSignature { binders, constraints, arguments, result: current });
                };

                let function = normalise::normalise_expand(state, context, function)?;
                if function == context.prim.function {
                    arguments.push(argument);
                    current = result;
                } else {
                    return Ok(DecomposedSignature { binders, constraints, arguments, result: current });
                }
            }

            _ => return Ok(DecomposedSignature { binders, constraints, arguments, result: current }),
        }
    }
}

pub fn expect_signature_bindings<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    (signature_id, signature_type): (lowering::TypeId, TypeId),
    bindings: &[TypeVariableBinding],
) -> QueryResult<DecomposedSignature>
where
    Q: ExternalQueries,
{
    let signature =
        decompose_signature(state, context, signature_type, DecomposeSignatureMode::Full)?;

    let actual = bindings.len() as u32;
    let expected = signature.arguments.len() as u32;

    if actual > expected {
        state.insert_error(ErrorKind::TypeSignatureVariableMismatch {
            id: signature_id,
            expected,
            actual,
        });
    }

    let mut remaining = signature.arguments.into_iter();
    let arguments = remaining.by_ref().take(actual as usize).collect();
    let result = context.intern_function_chain_iter(remaining, signature.result);

    Ok(DecomposedSignature {
        binders: signature.binders,
        constraints: signature.constraints,
        arguments,
        result,
    })
}

pub fn expect_signature_patterns<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_type: TypeId,
    required: usize,
) -> QueryResult<DecomposedSignature>
where
    Q: ExternalQueries,
{
    let signature = decompose_signature(
        state,
        context,
        signature_type,
        DecomposeSignatureMode::Patterns { required },
    )?;

    for &constraint in &signature.constraints {
        state.push_given(constraint);
    }

    Ok(signature)
}
