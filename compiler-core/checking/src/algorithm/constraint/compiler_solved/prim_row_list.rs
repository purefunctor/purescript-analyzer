use lowering::StringKind;

use crate::algorithm::constraint::MatchInstance;
use crate::algorithm::kind;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::core::RowType;
use crate::{ExternalQueries, Type, TypeId};

fn extract_closed_row(state: &CheckState, id: TypeId) -> Option<RowType> {
    let Type::Row(row) = &state.storage[id] else { return None };
    if row.tail.is_some() {
        return None;
    }
    Some(row.clone())
}

fn extract_row_element_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);

    if let Type::Row(ref row_type) = state.storage[type_id]
        && let Some(field) = row_type.fields.first()
        && let Ok(kind) = kind::elaborate_kind(state, context, field.id)
    {
        return kind;
    }

    state.fresh_unification_type(context)
}

pub fn prim_rowlist_row_to_list<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[row, list] = arguments else {
        return None;
    };

    let row = state.normalize_type(row);
    let list = state.normalize_type(list);

    let Some(row_row) = extract_closed_row(state, row) else {
        return Some(MatchInstance::Stuck);
    };

    let element_kind = extract_row_element_kind(state, context, row);

    let mut result =
        state.storage.intern(Type::KindApplication(context.prim_row_list.nil, element_kind));

    let cons_kinded =
        state.storage.intern(Type::KindApplication(context.prim_row_list.cons, element_kind));

    for field in row_row.fields.iter().rev() {
        let label_type =
            state.storage.intern(Type::String(StringKind::String, field.label.clone()));

        let cons_label = state.storage.intern(Type::Application(cons_kinded, label_type));
        let cons_type = state.storage.intern(Type::Application(cons_label, field.id));

        result = state.storage.intern(Type::Application(cons_type, result));
    }

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![(list, result)] })
}
