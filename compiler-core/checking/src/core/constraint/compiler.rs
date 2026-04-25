pub mod prim_coerce;
pub mod prim_int;
pub mod prim_reflectable;
pub mod prim_row;
pub mod prim_row_list;
pub mod prim_symbol;
pub mod prim_type_error;

use std::sync::Arc;

use building_types::QueryResult;
use smol_str::SmolStr;

use crate::context::CheckContext;
use crate::core::fold::{FoldAction, TypeFold, fold_type};
use crate::core::unification::{CanUnify, can_unify};
use crate::core::{RowField, RowType, Type, TypeId, normalise};
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

use super::matching::{InstanceMatch, MatchInstance};
use super::{CanonicalConstraintId, WorkItem};

#[derive(Clone)]
pub enum RowView {
    Closed { fields: Arc<[RowField]> },
    EmptyOpen { tail: TypeId },
    Open { fields: Arc<[RowField]>, tail: TypeId },
}

impl RowView {
    fn from_row(row: RowType) -> RowView {
        let fields = Arc::clone(&row.fields);
        match row.tail {
            Some(tail) => RowView::Open { fields, tail },
            None => RowView::Closed { fields },
        }
    }

    fn from_tail(tail: TypeId) -> RowView {
        RowView::EmptyOpen { tail }
    }

    pub fn fields(&self) -> &[RowField] {
        match self {
            RowView::Closed { fields } => fields,
            RowView::EmptyOpen { .. } => &[],
            RowView::Open { fields, .. } => fields,
        }
    }

    pub fn tail(&self) -> Option<TypeId> {
        match self {
            RowView::Closed { fields: _ } => None,
            RowView::EmptyOpen { tail } => Some(*tail),
            RowView::Open { tail, .. } => Some(*tail),
        }
    }
}

struct RecursivelyNormalise;

impl TypeFold for RecursivelyNormalise {
    fn transform<Q>(
        &mut self,
        state: &mut CheckState,
        context: &CheckContext<Q>,
        id: TypeId,
        _t: &Type,
    ) -> QueryResult<FoldAction>
    where
        Q: ExternalQueries,
    {
        let expanded = normalise::expand(state, context, id)?;
        if expanded == id {
            Ok(FoldAction::Continue)
        } else {
            Ok(FoldAction::ReplaceThen(expanded))
        }
    }
}

fn recursively_normalise<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut folder = RecursivelyNormalise;

    safe_loop! {
        let next = fold_type(state, context, id, &mut folder)?;
        if next == id {
            return Ok(id);
        }
        id = next;
    }
}

pub fn extract_integer<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<i32>>
where
    Q: ExternalQueries,
{
    let id = recursively_normalise(state, context, id)?;
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
    let id = recursively_normalise(state, context, id)?;
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
) -> QueryResult<Option<RowView>>
where
    Q: ExternalQueries,
{
    let id = recursively_normalise(state, context, id)?;
    let row = if let Type::Row(id) = context.lookup_type(id) {
        RowView::from_row(context.lookup_row_type(id))
    } else {
        RowView::from_tail(id)
    };
    Ok(Some(row))
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
            MatchInstance::Match(InstanceMatch { goals: vec![WorkItem::Unify(actual, expected)] })
        }
    })
}

pub fn match_compiler_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: CanonicalConstraintId,
    given: &[CanonicalConstraintId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let canonical = &state.canonicals[wanted];
    let file_id = canonical.file_id;
    let item_id = canonical.type_id;

    let match_instance = if file_id == context.prim_int.file_id {
        if item_id == context.prim_int.add {
            let Some(arguments) = canonical.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            prim_int::match_add(state, context, &arguments)?
        } else if item_id == context.prim_int.mul {
            let Some(arguments) = canonical.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            prim_int::match_mul(state, context, &arguments)?
        } else if item_id == context.prim_int.compare {
            let Some(arguments) = canonical.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            prim_int::match_compare(state, context, &arguments, given)?
        } else if item_id == context.prim_int.to_string {
            let Some(arguments) = canonical.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            prim_int::match_to_string(state, context, &arguments)?
        } else {
            None
        }
    } else if file_id == context.prim_symbol.file_id {
        if item_id == context.prim_symbol.append {
            let Some(arguments) = canonical.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            prim_symbol::match_append(state, context, &arguments)?
        } else if item_id == context.prim_symbol.compare {
            let Some(arguments) = canonical.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            prim_symbol::match_compare(state, context, &arguments)?
        } else if item_id == context.prim_symbol.cons {
            let Some(arguments) = canonical.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            prim_symbol::match_cons(state, context, &arguments)?
        } else {
            None
        }
    } else if file_id == context.prim_row.file_id {
        if item_id == context.prim_row.union {
            let Some(arguments) = canonical.expect_type_arguments::<3>() else {
                return Ok(None);
            };
            prim_row::match_union(state, context, &arguments)?
        } else if item_id == context.prim_row.cons {
            let Some(arguments) = canonical.expect_type_arguments::<4>() else {
                return Ok(None);
            };
            prim_row::match_cons(state, context, &arguments)?
        } else if item_id == context.prim_row.lacks {
            let Some(arguments) = canonical.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            prim_row::match_lacks(state, context, &arguments)?
        } else if item_id == context.prim_row.nub {
            let Some(arguments) = canonical.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            prim_row::match_nub(state, context, &arguments)?
        } else {
            None
        }
    } else if file_id == context.prim_row_list.file_id {
        if item_id == context.prim_row_list.row_to_list {
            let Some(arguments) = canonical.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            prim_row_list::match_row_to_list(state, context, &arguments)?
        } else {
            None
        }
    } else if file_id == context.prim_coerce.file_id {
        if item_id == context.prim_coerce.coercible {
            let Some(arguments) = canonical.expect_type_arguments::<2>() else {
                return Ok(None);
            };
            prim_coerce::match_coercible(state, context, &arguments)?
        } else {
            None
        }
    } else if file_id == context.prim_type_error.file_id {
        if item_id == context.prim_type_error.warn {
            let Some(arguments) = canonical.expect_type_arguments::<1>() else {
                return Ok(None);
            };
            prim_type_error::match_warn(state, context, &arguments)?
        } else if item_id == context.prim_type_error.fail {
            let Some(arguments) = canonical.expect_type_arguments::<1>() else {
                return Ok(None);
            };
            prim_type_error::match_fail(state, context, &arguments)?
        } else {
            None
        }
    } else if context.known_reflectable.is_symbol == Some((file_id, item_id)) {
        let Some(arguments) = canonical.expect_type_arguments::<1>() else {
            return Ok(None);
        };
        prim_symbol::match_is_symbol(state, context, &arguments)?
    } else if context.known_reflectable.reflectable == Some((file_id, item_id)) {
        let Some(arguments) = canonical.expect_type_arguments::<2>() else {
            return Ok(None);
        };
        prim_reflectable::match_reflectable(state, context, &arguments)?
    } else {
        None
    };

    Ok(match_instance)
}
