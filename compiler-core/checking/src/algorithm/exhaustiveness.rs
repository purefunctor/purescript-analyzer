use std::sync::Arc;

use files::FileId;
use indexing::TermItemId;
use itertools::Itertools;
use lowering::BinderId;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::{
    ExternalQueries, TypeId,
    algorithm::state::{CheckContext, CheckState},
};

const MISSING_NAME: SmolStr = SmolStr::new_inline("<MissingName>");

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Pattern {
    Wildcard,
    Boolean(bool),
    Char(char),
    String(SmolStr),
    Integer(i32),
    Number(bool, SmolStr),
    Array { elements: Vec<PatternId> },
    Record { elements: Vec<RecordElement> },
    Constructor { file_id: FileId, item_id: TermItemId, fields: Vec<PatternId> },
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
    types: FxHashMap<PatternId, TypeId>,
}

impl ExhaustivenessState {
    fn allocate(&mut self, pattern: Pattern, t: TypeId) -> PatternId {
        let id = self.interner.intern(pattern);
        self.types.insert(id, t);
        id
    }

    fn allocate_wildcard(&mut self, t: TypeId) -> PatternId {
        self.allocate(Pattern::Wildcard, t)
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
            Some(v) => exhaustiveness_state.allocate(Pattern::Integer(*v), t),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::Number { negative, value } => {
            if let Some(value) = value {
                let pattern = Pattern::Number(*negative, SmolStr::clone(value));
                exhaustiveness_state.allocate(pattern, t)
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
                let pattern = Pattern::String(SmolStr::clone(value));
                exhaustiveness_state.allocate(pattern, t)
            } else {
                exhaustiveness_state.allocate_wildcard(t)
            }
        }
        lowering::BinderKind::Char { value } => match value {
            Some(v) => exhaustiveness_state.allocate(Pattern::Char(*v), t),
            None => exhaustiveness_state.allocate_wildcard(t),
        },
        lowering::BinderKind::Boolean { boolean } => {
            exhaustiveness_state.allocate(Pattern::Boolean(*boolean), t)
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
    exhaustiveness_state.allocate(Pattern::Array { elements }, t)
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
    exhaustiveness_state.allocate(Pattern::Record { elements }, t)
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

    let pattern = Pattern::Constructor { file_id: *file_id, item_id: *item_id, fields };
    exhaustiveness_state.allocate(pattern, t)
}

fn lower_operator_chain_binder<Q>(
    _check_state: &mut CheckState,
    exhaustiveness_state: &mut ExhaustivenessState,
    _context: &CheckContext<Q>,
    _id: BinderId,
    t: TypeId,
) -> PatternId
where
    Q: ExternalQueries,
{
    // TODO: Lookup context.bracketed.binders.get(&id) and traverse tree to build Pattern::Constructor
    exhaustiveness_state.allocate_wildcard(t)
}
