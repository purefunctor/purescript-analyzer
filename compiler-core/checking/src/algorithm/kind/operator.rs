//! Kind checking for operators

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, operator, toolkit};
use crate::core::{Type, TypeId};

pub fn infer_operator_chain_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> QueryResult<(TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    operator::infer_operator_chain(state, context, id)
}

pub fn elaborate_operator_application_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let operator_kind = kind::lookup_file_type(state, context, file_id, type_id)?;
    let operator_kind = toolkit::instantiate_forall(state, operator_kind);

    let operator_kind = state.normalize_type(operator_kind);
    let Type::Function(_, operator_kind) = state.storage[operator_kind] else {
        return Ok(context.prim.unknown);
    };

    let operator_kind = state.normalize_type(operator_kind);
    let Type::Function(_, result_kind) = state.storage[operator_kind] else {
        return Ok(context.prim.unknown);
    };

    Ok(result_kind)
}
