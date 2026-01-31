use std::sync::Arc;

use files::FileId;
use indexing::TermItemId;
use itertools::Itertools;
use lowering::{BinderId, TermOperatorId};
use smol_str::SmolStr;
use sugar::OperatorTree;

use crate::algorithm::exhaustiveness::{Constructor, PatternId, PatternKind, RecordElement};
use crate::algorithm::state::{CheckContext, CheckState, OperatorBranchTypes};
use crate::{ExternalQueries, TypeId};

const MISSING_NAME: SmolStr = SmolStr::new_inline("<MissingName>");

pub fn convert_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: BinderId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let t = state.term_scope.lookup_binder(id).unwrap_or(context.prim.unknown);

    let Some(kind) = context.lowered.info.get_binder_kind(id) else {
        return state.allocate_wildcard(t);
    };

    match kind {
        lowering::BinderKind::Typed { binder, .. } => match binder {
            Some(id) => convert_binder(state, context, *id),
            None => state.allocate_wildcard(t),
        },
        lowering::BinderKind::OperatorChain { .. } => {
            convert_operator_chain_binder(state, context, id, t)
        }
        lowering::BinderKind::Integer { value } => match value {
            Some(v) => state.allocate_pattern(PatternKind::Integer(*v), t),
            None => state.allocate_wildcard(t),
        },
        lowering::BinderKind::Number { negative, value } => {
            if let Some(value) = value {
                let kind = PatternKind::Number(*negative, SmolStr::clone(value));
                state.allocate_pattern(kind, t)
            } else {
                state.allocate_wildcard(t)
            }
        }
        lowering::BinderKind::Constructor { resolution, arguments } => {
            convert_constructor_binder(state, context, resolution, arguments, t)
        }
        lowering::BinderKind::Variable { .. } => state.allocate_wildcard(t),
        lowering::BinderKind::Named { binder, .. } => match binder {
            Some(id) => convert_binder(state, context, *id),
            None => state.allocate_wildcard(t),
        },
        lowering::BinderKind::Wildcard => state.allocate_wildcard(t),
        lowering::BinderKind::String { value, .. } => {
            if let Some(value) = value {
                let kind = PatternKind::String(SmolStr::clone(value));
                state.allocate_pattern(kind, t)
            } else {
                state.allocate_wildcard(t)
            }
        }
        lowering::BinderKind::Char { value } => match value {
            Some(v) => state.allocate_pattern(PatternKind::Char(*v), t),
            None => state.allocate_wildcard(t),
        },
        lowering::BinderKind::Boolean { boolean } => {
            state.allocate_pattern(PatternKind::Boolean(*boolean), t)
        }
        lowering::BinderKind::Array { array } => lower_array_binder(state, context, array, t),
        lowering::BinderKind::Record { record } => lower_record_binder(state, context, record, t),
        lowering::BinderKind::Parenthesized { parenthesized } => match parenthesized {
            Some(id) => convert_binder(state, context, *id),
            None => state.allocate_wildcard(t),
        },
    }
}

fn lower_array_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    array: &[BinderId],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let elements = array.iter().map(|element| convert_binder(state, context, *element)).collect();
    state.allocate_pattern(PatternKind::Array { elements }, t)
}

fn lower_record_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    record: &[lowering::BinderRecordItem],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let elements =
        record.iter().map(|element| lower_record_element(state, context, element)).collect();
    state.allocate_pattern(PatternKind::Record { elements }, t)
}

fn lower_record_element<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    element: &lowering::BinderRecordItem,
) -> RecordElement
where
    Q: ExternalQueries,
{
    match element {
        lowering::BinderRecordItem::RecordField { name, value } => {
            let name = name.clone().unwrap_or(MISSING_NAME);
            let value = if let Some(value) = value {
                convert_binder(state, context, *value)
            } else {
                state.allocate_wildcard(context.prim.unknown)
            };
            RecordElement::Named(name, value)
        }
        lowering::BinderRecordItem::RecordPun { name, .. } => {
            let name = name.clone().unwrap_or(MISSING_NAME);
            RecordElement::Pun(name)
        }
    }
}

fn convert_constructor_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: &Option<(FileId, TermItemId)>,
    arguments: &Arc<[BinderId]>,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some((file_id, item_id)) = resolution else {
        return state.allocate_wildcard(t);
    };

    let fields =
        arguments.iter().map(|argument| convert_binder(state, context, *argument)).collect_vec();

    let constructor = Constructor { file_id: *file_id, item_id: *item_id, fields };
    state.allocate_pattern(PatternKind::Constructor { constructor }, t)
}

fn convert_operator_chain_binder<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: BinderId,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some(tree) = context.bracketed.binders.get(&id) else {
        return state.allocate_wildcard(t);
    };

    let Ok(tree) = tree else {
        return state.allocate_wildcard(t);
    };

    convert_operator_tree(state, context, tree, t)
}

fn convert_operator_tree<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    tree: &OperatorTree<BinderId>,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    match tree {
        OperatorTree::Leaf(None) => state.allocate_wildcard(t),
        OperatorTree::Leaf(Some(binder_id)) => convert_binder(state, context, *binder_id),
        OperatorTree::Branch(operator_id, children) => {
            convert_operator_branch(state, context, *operator_id, children, t)
        }
    }
}

fn convert_operator_branch<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    operator_id: TermOperatorId,
    children: &[OperatorTree<BinderId>; 2],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some((file_id, item_id)) = context.lowered.info.get_term_operator(operator_id) else {
        return state.allocate_wildcard(t);
    };

    let Some(OperatorBranchTypes { left, right, result }) =
        state.term_scope.lookup_operator_node(operator_id)
    else {
        return state.allocate_wildcard(t);
    };

    let [left_tree, right_tree] = children;

    let left_pattern = convert_operator_tree(state, context, left_tree, left);
    let right_pattern = convert_operator_tree(state, context, right_tree, right);

    let constructor = Constructor { file_id, item_id, fields: vec![left_pattern, right_pattern] };
    state.allocate_pattern(PatternKind::Constructor { constructor }, result)
}
