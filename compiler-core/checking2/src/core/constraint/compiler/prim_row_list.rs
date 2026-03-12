use std::sync::Arc;

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint::MatchInstance;
use crate::core::{RowField, RowType, Type, TypeId, normalise};
use crate::source::types;
use crate::state::CheckState;

use super::{extract_row, intern_symbol, match_equality};

pub fn match_row_to_list<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[row, list] = arguments else {
        return Ok(None);
    };

    let Some(row_value) = extract_row(state, context, row)? else {
        return Ok(Some(MatchInstance::Stuck));
    };
    if row_value.tail.is_some() {
        return Ok(Some(MatchInstance::Stuck));
    }

    let element_kind = row_element_kind(state, context, &row_value)?;

    let nil = context.intern_kind_application(context.prim_row_list.nil, element_kind);
    let cons = context.intern_kind_application(context.prim_row_list.cons, element_kind);

    let result = row_value.fields.iter().rev().fold(nil, |rest, field| {
        let label = intern_symbol(context, field.label.as_str());
        let cons_label = context.intern_application(cons, label);
        let cons_type = context.intern_application(cons_label, field.id);
        context.intern_application(cons_type, rest)
    });

    Ok(Some(match_equality(state, context, list, result)?))
}

fn row_element_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    row: &RowType,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let Some(RowField { id, .. }) = row.fields.first() else {
        return Ok(state.fresh_unification(context.queries, context.prim.t));
    };

    let fields = Arc::from([RowField { label: row.fields[0].label.clone(), id: *id }]);
    let singleton_row = RowType { fields, tail: None };
    let singleton_row_id = context.intern_row_type(singleton_row);
    let singleton_row_type = context.intern_row(singleton_row_id);
    let row_kind = types::elaborate_kind(state, context, singleton_row_type)?;

    let row_kind = normalise::expand(state, context, row_kind)?;
    let Type::Application(_, element_kind) = context.lookup_type(row_kind) else {
        return Ok(state.fresh_unification(context.queries, context.prim.t));
    };

    Ok(element_kind)
}
