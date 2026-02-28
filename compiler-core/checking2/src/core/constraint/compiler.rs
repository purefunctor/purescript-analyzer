use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::state::CheckState;

use super::{ConstraintApplication, MatchInstance};

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
