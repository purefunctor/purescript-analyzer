use building_types::QueryResult;
use lowering::TypeVariableBinding;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{ForallBinder, TypeId, toolkit};
use crate::error::ErrorKind;
use crate::state::CheckState;

pub struct InspectSignature {
    pub binders: Vec<ForallBinder>,
    pub arguments: Vec<TypeId>,
    pub result: TypeId,
}

// TODO: Inline into check_data_equation_check; inspect_quantified and
// inspect_function are still needed for error reporting (mismatch), but
// InspectSignature can be collapsed to just Vec<TypeId> for arguments.
pub fn inspect_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: (lowering::TypeId, TypeId),
    bindings: &[TypeVariableBinding],
) -> QueryResult<InspectSignature>
where
    Q: ExternalQueries,
{
    let (signature_id, signature_kind) = signature;

    let toolkit::InspectQuantified { binders, quantified } =
        toolkit::inspect_quantified(state, context, signature_kind)?;

    let toolkit::InspectFunction { arguments, result } =
        toolkit::inspect_function_with(state, context, quantified, toolkit::InspectMode::Full)?;

    if bindings.len() > arguments.len() {
        state.insert_error(ErrorKind::TypeSignatureVariableMismatch {
            id: signature_id,
            expected: arguments.len() as u32,
            actual: bindings.len() as u32,
        });
    }

    let count = bindings.len().min(arguments.len());
    let arguments = arguments.iter().take(count).copied().collect();

    Ok(InspectSignature { binders, arguments, result })
}

pub fn inspect_signature_bindings<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: (lowering::TypeId, TypeId),
    bindings: &[TypeVariableBinding],
) -> QueryResult<InspectSignature>
where
    Q: ExternalQueries,
{
    let (signature_id, signature_kind) = signature;

    let toolkit::InspectQuantified { binders, quantified } =
        toolkit::inspect_quantified(state, context, signature_kind)?;

    let toolkit::InspectFunction { arguments, result } =
        toolkit::inspect_function_with(state, context, quantified, toolkit::InspectMode::Full)?;

    if bindings.len() > arguments.len() {
        state.insert_error(ErrorKind::TypeSignatureVariableMismatch {
            id: signature_id,
            expected: arguments.len() as u32,
            actual: bindings.len() as u32,
        });
    }

    let mut remaining = arguments.into_iter();

    let arguments = remaining.by_ref().take(bindings.len()).collect();
    let result = context.intern_function_chain_iter(remaining, result);

    Ok(InspectSignature { binders, arguments, result })
}
