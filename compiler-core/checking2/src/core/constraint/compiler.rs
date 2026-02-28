use building_types::QueryResult;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{RowType, Type, TypeId, normalise};
use crate::state::CheckState;

use super::{ConstraintApplication, MatchInstance};

pub fn extract_integer<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<i32>>
where
    Q: ExternalQueries,
{
    let id = normalise::normalise(state, context, id)?;
    match context.lookup_type(id) {
        Type::Integer(value) => Ok(Some(value)),
        _ => Ok(None),
    }
}

pub fn extract_symbol<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<SmolStr>>
where
    Q: ExternalQueries,
{
    let id = normalise::normalise(state, context, id)?;
    if let Type::String(_, id) = context.lookup_type(id) {
        Ok(Some(context.queries.lookup_smol_str(id)))
    } else {
        Ok(None)
    }
}

pub fn extract_row<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<RowType>>
where
    Q: ExternalQueries,
{
    let id = normalise::normalise(state, context, id)?;
    if let Type::Row(id) = context.lookup_type(id) {
        Ok(Some(context.lookup_row_type(id)))
    } else {
        Ok(None)
    }
}

pub fn intern_symbol<Q>(context: &CheckContext<Q>, value: &str) -> TypeId
where
    Q: ExternalQueries,
{
    let smol_str_id = context.queries.intern_smol_str(SmolStr::new(value));
    context.queries.intern_type(Type::String(lowering::StringKind::String, smol_str_id))
}

pub fn intern_integer<Q>(context: &CheckContext<Q>, value: i32) -> TypeId
where
    Q: ExternalQueries,
{
    context.queries.intern_type(Type::Integer(value))
}

pub fn match_compiler_instances<Q>(
    _state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    _given: &[ConstraintApplication],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let ConstraintApplication { file_id, item_id, arguments: _ } = *wanted;

    let match_instance = if file_id == context.prim_int.file_id {
        if item_id == context.prim_int.add {
            None
        } else if item_id == context.prim_int.mul {
            None
        } else if item_id == context.prim_int.compare {
            None
        } else if item_id == context.prim_int.to_string {
            None
        } else {
            None
        }
    } else if file_id == context.prim_symbol.file_id {
        if item_id == context.prim_symbol.append {
            None
        } else if item_id == context.prim_symbol.compare {
            None
        } else if item_id == context.prim_symbol.cons {
            None
        } else {
            None
        }
    } else if file_id == context.prim_row.file_id {
        if item_id == context.prim_row.union {
            None
        } else if item_id == context.prim_row.cons {
            None
        } else if item_id == context.prim_row.lacks {
            None
        } else if item_id == context.prim_row.nub {
            None
        } else {
            None
        }
    } else if file_id == context.prim_row_list.file_id {
        if item_id == context.prim_row_list.row_to_list { None } else { None }
    } else if file_id == context.prim_coerce.file_id {
        if item_id == context.prim_coerce.coercible { None } else { None }
    } else if file_id == context.prim_type_error.file_id {
        if item_id == context.prim_type_error.warn {
            None
        } else if item_id == context.prim_type_error.fail {
            None
        } else {
            None
        }
    } else if context.known_reflectable.is_symbol == Some((file_id, item_id)) {
        None
    } else if context.known_reflectable.reflectable == Some((file_id, item_id)) {
        None
    } else {
        None
    };

    Ok(match_instance)
}
