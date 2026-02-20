//! Implements shared utilities for core type operations.

use building_types::QueryResult;

use crate::context::CheckContext;
use crate::core::{Type, TypeId, normalise};
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

/// Splits a function-like type into argument kinds and a result kind.
pub fn function_components<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<(Vec<TypeId>, TypeId)>
where
    Q: ExternalQueries,
{
    let mut arguments = vec![];
    let mut current = id;

    safe_loop! {
        current = normalise::normalise(state, context, current)?;

        match context.lookup_type(current) {
            Type::Forall(_, inner) => {
                current = inner;
            }
            Type::Function(argument, result) => {
                arguments.push(argument);
                current = result;
            }
            Type::Application(function_argument, result) => {
                let function_argument = normalise::normalise(state, context, function_argument)?;
                let Type::Application(function, argument) = context.lookup_type(function_argument) else {
                    break;
                };
                let function = normalise::normalise(state, context, function)?;
                if function == context.prim.function {
                    arguments.push(argument);
                    current = result;
                } else {
                    break;
                }
            }
            _ => break,
        }
    }

    Ok((arguments, current))
}
