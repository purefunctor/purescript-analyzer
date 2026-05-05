use building_types::QueryResult;
use itertools::Itertools;
use lowering::TypeVariableBinding;

use crate::context::CheckContext;
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::{ForallBinder, Type, TypeId, normalise, unification};
use crate::error::ErrorKind;
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

pub struct DecomposedSignature {
    pub binders: Vec<ForallBinder>,
    pub constraints: Vec<TypeId>,
    pub arguments: Vec<TypeId>,
    pub result: TypeId,
}

pub struct SkolemisedSignature {
    pub substitution: NameToType,
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
        current = normalise::expand(state, context, current)?;

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
                    normalise::expand(state, context, function_argument)?;

                let Type::Application(function, argument) = context.lookup_type(function_argument)
                else {
                    return Ok(DecomposedSignature { binders, constraints, arguments, result: current });
                };

                let function = normalise::expand(state, context, function)?;
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

pub fn expect_type_signature<Q>(
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
    let result = context.intern_function_iter(remaining, signature.result);

    Ok(DecomposedSignature {
        binders: signature.binders,
        constraints: signature.constraints,
        arguments,
        result,
    })
}

pub fn expect_term_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_type: TypeId,
    required: usize,
) -> QueryResult<SkolemisedSignature>
where
    Q: ExternalQueries,
{
    let signature =
        decompose_signature(state, context, signature_type, DecomposeSignatureMode::Full)?;

    let SkolemisedSignature { substitution, constraints, arguments, result } =
        skolemise_decomposed_signature(state, context, signature)?;

    let mut remaining = arguments.into_iter();
    let mut arguments = remaining.by_ref().take(required).collect_vec();

    let mut result = context.intern_function_iter(remaining, result);
    synthesise_functions(state, context, &mut arguments, &mut result, required)?;

    Ok(SkolemisedSignature { substitution, constraints, arguments, result })
}

fn synthesise_functions<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &mut Vec<TypeId>,
    result_type: &mut TypeId,
    required: usize,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    while arguments.len() < required {
        let current = normalise::expand(state, context, *result_type)?;

        let Type::Unification(unification_id) = context.lookup_type(current) else {
            break;
        };

        let argument = state.fresh_unification(context.queries, context.prim.t);
        let result = state.fresh_unification(context.queries, context.prim.t);
        let function = context.intern_function(argument, result);

        if !unification::solve(state, context, current, unification_id, function)? {
            break;
        }

        arguments.push(argument);
        *result_type = result;
    }

    Ok(())
}

fn skolemise_decomposed_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: DecomposedSignature,
) -> QueryResult<SkolemisedSignature>
where
    Q: ExternalQueries,
{
    let mut substitution = NameToType::default();

    for binder in &signature.binders {
        let kind = SubstituteName::many(state, context, &substitution, binder.kind)?;
        let text = state.checked.lookup_name(binder.name);
        let rigid = state.fresh_rigid_named(context.queries, kind, text);
        substitution.insert(binder.name, rigid);
    }

    let constraints = signature
        .constraints
        .iter()
        .map(|&constraint| SubstituteName::many(state, context, &substitution, constraint))
        .collect::<QueryResult<Vec<_>>>()?;

    let arguments = signature
        .arguments
        .iter()
        .map(|&argument| SubstituteName::many(state, context, &substitution, argument))
        .collect::<QueryResult<Vec<_>>>()?;

    let result = SubstituteName::many(state, context, &substitution, signature.result)?;

    Ok(SkolemisedSignature { substitution, constraints, arguments, result })
}
