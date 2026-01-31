use std::sync::Arc;

use files::FileId;
use indexing::TermItemId;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::algorithm::exhaustiveness::{
    Constructor, ExhaustivenessState, PatternId, PatternKind, RecordElement, WitnessVector,
};
use crate::algorithm::state::CheckContext;

pub fn render_witness<Q>(
    context: &CheckContext<Q>,
    state: &ExhaustivenessState,
    witness: &WitnessVector,
) -> String
where
    Q: ExternalQueries,
{
    witness.iter().map(|&id| render_pattern(context, state, id)).join(", ")
}

fn render_pattern<Q>(
    context: &CheckContext<Q>,
    state: &ExhaustivenessState,
    id: PatternId,
) -> String
where
    Q: ExternalQueries,
{
    let pattern = &state.interner[id];
    match &pattern.kind {
        PatternKind::Wildcard => "_".to_string(),
        PatternKind::Boolean(b) => b.to_string(),
        PatternKind::Char(c) => format!("'{c}'"),
        PatternKind::String(s) => format!("\"{s}\""),
        PatternKind::Integer(i) => i.to_string(),
        PatternKind::Number(negative, n) => {
            if *negative {
                format!("-{n}")
            } else {
                n.to_string()
            }
        }
        PatternKind::Array { elements } => {
            let mut elements = elements.iter().map(|&e| render_pattern(context, state, e));
            format!("[{}]", elements.join(", "))
        }
        PatternKind::Record { elements } => {
            let mut elements = elements.iter().map(|e| match e {
                RecordElement::Named(name, pattern) => {
                    let pattern = render_pattern(context, state, *pattern);
                    format!("{name}: {pattern}")
                }
                RecordElement::Pun(name) => name.to_string(),
            });
            format!("{{ {} }}", elements.join(", "))
        }
        PatternKind::Constructor { constructor } => render_constructor(context, state, constructor),
    }
}

fn render_constructor<Q>(
    context: &CheckContext<Q>,
    exhaustiveness_state: &ExhaustivenessState,
    constructor: &Constructor,
) -> String
where
    Q: ExternalQueries,
{
    let name = lookup_constructor_name(context, constructor.file_id, constructor.item_id)
        .unwrap_or_else(|| "<Unknown>".to_string());

    if constructor.fields.is_empty() {
        return name;
    }

    let mut fields = constructor.fields.iter().map(|&id| {
        let rendered = render_pattern(context, exhaustiveness_state, id);
        let pattern = &exhaustiveness_state.interner[id];
        if let PatternKind::Constructor { constructor } = &pattern.kind
            && !constructor.fields.is_empty()
        {
            format!("({rendered})")
        } else {
            rendered
        }
    });

    format!("{} {}", name, fields.join(" "))
}

fn lookup_constructor_name<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    term_id: TermItemId,
) -> Option<String>
where
    Q: ExternalQueries,
{
    let indexed = if file_id == context.id {
        Arc::clone(&context.indexed)
    } else {
        context.queries.indexed(file_id).ok()?
    };

    let item = &indexed.items[term_id];
    item.name.as_ref().map(|name| name.to_string())
}
