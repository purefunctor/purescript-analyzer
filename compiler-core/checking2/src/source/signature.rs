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
        toolkit::inspect_function(state, context, quantified)?;

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
