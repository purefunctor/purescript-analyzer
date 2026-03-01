mod prim_int;
mod prim_reflectable;
mod prim_row;
mod prim_row_list;
mod prim_symbol;
mod prim_type_error;

use building_types::QueryResult;
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::unification::{CanUnify, can_unify};
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

fn match_equality<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    actual: TypeId,
    expected: TypeId,
) -> QueryResult<MatchInstance>
where
    Q: ExternalQueries,
{
    Ok(match can_unify(state, context, actual, expected)? {
        CanUnify::Apart => MatchInstance::Apart,
        CanUnify::Equal | CanUnify::Unify => {
            MatchInstance::Match { constraints: vec![], equalities: vec![(actual, expected)] }
        }
    })
}

pub fn match_compiler_instances<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    given: &[ConstraintApplication],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let ConstraintApplication { file_id, item_id, arguments } = wanted;

    let match_instance = if *file_id == context.prim_int.file_id {
        if *item_id == context.prim_int.add {
            prim_int::match_add(state, context, arguments)?
        } else if *item_id == context.prim_int.mul {
            prim_int::match_mul(state, context, arguments)?
        } else if *item_id == context.prim_int.compare {
            prim_int::match_compare(state, context, arguments, given)?
        } else if *item_id == context.prim_int.to_string {
            prim_int::match_to_string(state, context, arguments)?
        } else {
            None
        }
    } else if *file_id == context.prim_symbol.file_id {
        if *item_id == context.prim_symbol.append {
            prim_symbol::match_append(state, context, arguments)?
        } else if *item_id == context.prim_symbol.compare {
            prim_symbol::match_compare(state, context, arguments)?
        } else if *item_id == context.prim_symbol.cons {
            prim_symbol::match_cons(state, context, arguments)?
        } else {
            None
        }
    } else if *file_id == context.prim_row.file_id {
        if *item_id == context.prim_row.union {
            prim_row::match_union(state, context, arguments)?
        } else if *item_id == context.prim_row.cons {
            prim_row::match_cons(state, context, arguments)?
        } else if *item_id == context.prim_row.lacks {
            prim_row::match_lacks(state, context, arguments)?
        } else if *item_id == context.prim_row.nub {
            prim_row::match_nub(state, context, arguments)?
        } else {
            None
        }
    } else if *file_id == context.prim_row_list.file_id {
        if *item_id == context.prim_row_list.row_to_list {
            prim_row_list::match_row_to_list(state, context, arguments)?
        } else {
            None
        }
    } else if *file_id == context.prim_coerce.file_id {
        None
    } else if *file_id == context.prim_type_error.file_id {
        if *item_id == context.prim_type_error.warn {
            prim_type_error::match_warn(state, context, arguments)?
        } else if *item_id == context.prim_type_error.fail {
            prim_type_error::match_fail(state, context, arguments)?
        } else {
            None
        }
    } else if context.known_reflectable.is_symbol == Some((*file_id, *item_id)) {
        prim_symbol::match_is_symbol(state, context, arguments)?
    } else if context.known_reflectable.reflectable == Some((*file_id, *item_id)) {
        prim_reflectable::match_reflectable(state, context, arguments)?
    } else {
        None
    };

    Ok(match_instance)
}
