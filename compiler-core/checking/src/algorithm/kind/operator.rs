//! Kind checking for operators

use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;
use lowering::{LoweredModule, TypeItemIr};

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

pub fn resolve_type_operator_target(
    lowered: &LoweredModule,
    item_id: TypeItemId,
) -> Option<(FileId, TypeItemId)> {
    let TypeItemIr::Operator { resolution, .. } = lowered.info.get_type_item(item_id)? else {
        return None;
    };
    *resolution
}

pub fn expand_type_operator<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let Type::OperatorApplication(file_id, item_id, left, right) = state.storage[type_id] else {
        return Ok(type_id);
    };

    let resolution = if file_id == context.id {
        context.lowered.info.get_type_item(item_id).and_then(|ir| match ir {
            TypeItemIr::Operator { resolution, .. } => *resolution,
            _ => None,
        })
    } else {
        context.queries.lowered(file_id)?.info.get_type_item(item_id).and_then(|ir| match ir {
            TypeItemIr::Operator { resolution, .. } => *resolution,
            _ => None,
        })
    };

    let Some((file_id, item_id)) = resolution else {
        return Ok(type_id);
    };

    let constructor = state.storage.intern(Type::Constructor(file_id, item_id));
    let left = state.storage.intern(Type::Application(constructor, left));
    let right = state.storage.intern(Type::Application(left, right));

    Ok(right)
}
