#![allow(dead_code, unused_variables)]

use std::iter;
use std::sync::Arc;

use files::FileId;
use indexing::TermItemId;
use itertools::Itertools;
use lowering::{BinderId, TermOperatorId};
use smol_str::SmolStr;
use sugar::OperatorTree;

use crate::algorithm::state::{CheckContext, CheckState, OperatorBranchTypes};
use crate::{ExternalQueries, TypeId};

const MISSING_NAME: SmolStr = SmolStr::new_inline("<MissingName>");

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pattern {
    kind: PatternKind,
    t: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PatternKind {
    Wildcard,
    Boolean(bool),
    Char(char),
    String(SmolStr),
    Integer(i32),
    Number(bool, SmolStr),
    Array { elements: Vec<PatternId> },
    Record { elements: Vec<RecordElement> },
    Constructor { constructor: Constructor },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constructor {
    file_id: FileId,
    item_id: TermItemId,
    fields: Vec<PatternId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RecordElement {
    Named(SmolStr, PatternId),
    Pun(SmolStr),
}

pub type PatternId = interner::Id<Pattern>;

type PatternVector = Vec<PatternId>;
type PatternMatrix = Vec<PatternVector>;
type WitnessVector = Vec<PatternId>;

struct ExhaustivenessState {
    interner: interner::Interner<Pattern>,
}

impl ExhaustivenessState {
    fn allocate(&mut self, kind: PatternKind, t: TypeId) -> PatternId {
        let pattern = Pattern { kind, t };
        self.interner.intern(pattern)
    }

    fn allocate_wildcard(&mut self, t: TypeId) -> PatternId {
        self.allocate(PatternKind::Wildcard, t)
    }
}

fn lower_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    id: BinderId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let t = check_state.term_scope.lookup_binder(id).unwrap_or(context.prim.unknown);

    let Some(kind) = context.lowered.info.get_binder_kind(id) else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    match kind {
        lowering::BinderKind::Typed { binder, .. } => match binder {
            Some(id) => lower_binder(check_state, exhaustiveness_state, context, *id),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::OperatorChain { .. } => {
            lower_operator_chain_binder(check_state, exhaustiveness_state, context, id, t)
        }
        lowering::BinderKind::Integer { value } => match value {
            Some(v) => exhaustiveness_state.allocate(PatternKind::Integer(*v), t),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::Number { negative, value } => {
            if let Some(value) = value {
                let kind = PatternKind::Number(*negative, SmolStr::clone(value));
                exhaustiveness_state.allocate(kind, t)
            } else {
                exhaustiveness_state.allocate_wildcard(t)
            }
        }
        lowering::BinderKind::Constructor { resolution, arguments } => lower_constructor_binder(
            check_state,
            exhaustiveness_state,
            context,
            resolution,
            arguments,
            t,
        ),
        lowering::BinderKind::Variable { .. } => exhaustiveness_state.allocate_wildcard(t),
        lowering::BinderKind::Named { binder, .. } => match binder {
            Some(id) => lower_binder(check_state, exhaustiveness_state, context, *id),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::Wildcard => exhaustiveness_state.allocate_wildcard(t),
        lowering::BinderKind::String { value, .. } => {
            if let Some(value) = value {
                let kind = PatternKind::String(SmolStr::clone(value));
                exhaustiveness_state.allocate(kind, t)
            } else {
                exhaustiveness_state.allocate_wildcard(t)
            }
        }
        lowering::BinderKind::Char { value } => match value {
            Some(v) => exhaustiveness_state.allocate(PatternKind::Char(*v), t),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::Boolean { boolean } => {
            exhaustiveness_state.allocate(PatternKind::Boolean(*boolean), t)
        }
        lowering::BinderKind::Array { array } => {
            lower_array_binder(check_state, exhaustiveness_state, context, array, t)
        }
        lowering::BinderKind::Record { record } => {
            lower_record_binder(check_state, exhaustiveness_state, context, record, t)
        }
        lowering::BinderKind::Parenthesized { parenthesized } => match parenthesized {
            Some(id) => lower_binder(check_state, exhaustiveness_state, context, *id),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
    }
}

fn lower_array_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    array: &[BinderId],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let elements = array
        .iter()
        .map(|element| lower_binder(check_state, exhaustiveness_state, context, *element))
        .collect();
    exhaustiveness_state.allocate(PatternKind::Array { elements }, t)
}

fn lower_record_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    record: &[lowering::BinderRecordItem],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let elements = record
        .iter()
        .map(|element| lower_record_element(check_state, exhaustiveness_state, context, element))
        .collect();
    exhaustiveness_state.allocate(PatternKind::Record { elements }, t)
}

fn lower_record_element<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
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
                lower_binder(check_state, exhaustiveness_state, context, *value)
            } else {
                exhaustiveness_state.allocate_wildcard(context.prim.unknown)
            };
            RecordElement::Named(name, value)
        }
        lowering::BinderRecordItem::RecordPun { name, .. } => {
            let name = name.clone().unwrap_or(MISSING_NAME);
            RecordElement::Pun(name)
        }
    }
}

fn lower_constructor_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    resolution: &Option<(FileId, TermItemId)>,
    arguments: &Arc<[BinderId]>,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some((file_id, item_id)) = resolution else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    let fields = arguments
        .iter()
        .map(|argument| lower_binder(check_state, exhaustiveness_state, context, *argument))
        .collect_vec();

    let constructor = Constructor { file_id: *file_id, item_id: *item_id, fields };
    exhaustiveness_state.allocate(PatternKind::Constructor { constructor }, t)
}

fn lower_operator_chain_binder<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    id: BinderId,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some(tree) = context.bracketed.binders.get(&id) else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    let Ok(tree) = tree else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    lower_operator_tree(check_state, exhaustiveness_state, context, tree, t)
}

fn lower_operator_tree<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    tree: &OperatorTree<BinderId>,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    match tree {
        OperatorTree::Leaf(None) => exhaustiveness_state.allocate_wildcard(t),
        OperatorTree::Leaf(Some(binder_id)) => {
            lower_binder(check_state, exhaustiveness_state, context, *binder_id)
        }
        OperatorTree::Branch(operator_id, children) => lower_operator_branch(
            check_state,
            exhaustiveness_state,
            context,
            *operator_id,
            children,
            t,
        ),
    }
}

fn lower_operator_branch<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    operator_id: TermOperatorId,
    children: &[OperatorTree<BinderId>; 2],
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    let Some((file_id, item_id)) = context.lowered.info.get_term_operator(operator_id) else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    let Some(OperatorBranchTypes { left, right, result }) =
        check_state.term_scope.lookup_operator_node(operator_id)
    else {
        return exhaustiveness_state.allocate_wildcard(t);
    };

    let [left_tree, right_tree] = children;

    let left_pattern =
        lower_operator_tree(check_state, exhaustiveness_state, context, left_tree, left);

    let right_pattern =
        lower_operator_tree(check_state, exhaustiveness_state, context, right_tree, right);

    let constructor = Constructor { file_id, item_id, fields: vec![left_pattern, right_pattern] };
    exhaustiveness_state.allocate(PatternKind::Constructor { constructor }, result)
}

fn algorithm_u<Q>(
    _check_state: &mut CheckState,
    _exhaustiveness_state: &mut ExhaustivenessState,
    _context: &CheckContext<Q>,
    _matrix: &PatternMatrix,
    _vector: &PatternVector,
) -> bool
where
    Q: ExternalQueries,
{
    todo!()
}

fn algorithm_m<Q>(
    _check_state: &mut CheckState,
    _exhaustiveness_state: &mut ExhaustivenessState,
    _context: &CheckContext<Q>,
    _matrix: &PatternMatrix,
    _vector: &PatternVector,
) -> Option<Vec<WitnessVector>>
where
    Q: ExternalQueries,
{
    todo!()
}

fn specialise_matrix<Q>(
    check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    expected: Constructor,
    matrix: &PatternMatrix,
) -> PatternMatrix
where
    Q: ExternalQueries,
{
    let matrix = matrix.iter().filter_map(|row| {
        specialise_vector(check_state, exhaustiveness_state, context, &expected, row)
    });
    matrix.collect()
}

fn specialise_vector<Q>(
    _check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    context: &CheckContext<Q>,
    expected: &Constructor,
    vector: &PatternVector,
) -> Option<PatternVector>
where
    Q: ExternalQueries,
{
    let [first_column, ref tail_columns @ ..] = vector[..] else {
        unreachable!("invariant violated: specialise_vector processed empty row");
    };

    let first_column = &exhaustiveness_state.interner[first_column];

    if let PatternKind::Wildcard = first_column.kind {
        let wildcards = expected.fields.iter().map(|&pattern| {
            let t = exhaustiveness_state.interner[pattern].t;
            exhaustiveness_state.allocate_wildcard(t)
        });
        let tail_columns = tail_columns.iter().copied();
        return Some(iter::chain(wildcards, tail_columns).collect());
    }

    let PatternKind::Constructor { constructor } = &first_column.kind else {
        return Some(tail_columns.to_vec());
    };

    if (constructor.file_id, constructor.item_id) != (expected.file_id, expected.item_id) {
        return None;
    }

    Some(iter::chain(&constructor.fields, tail_columns).copied().collect())
}

fn default_matrix(
    exhaustiveness_state: &ExhaustivenessState,
    matrix: &PatternMatrix,
) -> PatternMatrix {
    let filter_map = matrix.iter().filter_map(|row| {
        let [first_column, ref default_columns @ ..] = row[..] else {
            unreachable!("invariant violated: default_matrix processed empty row");
        };
        if let PatternKind::Wildcard = exhaustiveness_state.interner[first_column].kind {
            Some(default_columns.to_vec())
        } else {
            None
        }
    });
    filter_map.collect()
}
