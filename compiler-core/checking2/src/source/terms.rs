pub mod equations;

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::TypeId;
use crate::state::CheckState;

pub fn infer_expression_stub<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    _expression: lowering::ExpressionId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    // TODO(expressions): implement expression inference
    Ok(state.fresh_unification(context.queries, context.prim.t))
}

pub fn check_expression_stub<Q>(
    _state: &mut CheckState,
    _context: &CheckContext<Q>,
    _expression: lowering::ExpressionId,
    expected: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    // TODO(expressions): implement expression checking
    Ok(expected)
}
