use std::sync::Arc;

use files::FileId;
use indexing::TermItemId;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::algorithm::exhaustiveness::{
    PatternConstructor, PatternId, PatternKind, RecordElement, WitnessVector,
};
use crate::algorithm::state::{CheckContext, CheckState};

pub fn pretty_witness<Q>(
    context: &CheckContext<Q>,
    state: &CheckState,
    witness: &WitnessVector,
) -> String
where
    Q: ExternalQueries,
{
    witness.iter().map(|&id| pretty_pattern(context, state, id)).join(", ")
}

fn pretty_pattern<Q>(context: &CheckContext<Q>, state: &CheckState, id: PatternId) -> String
where
    Q: ExternalQueries,
{
    let pattern = &state.patterns[id];
    match &pattern.kind {
        PatternKind::Wildcard => "_".to_string(),
        PatternKind::Array { elements } => {
            let mut elements = elements.iter().map(|&e| pretty_pattern(context, state, e));
            format!("[{}]", elements.join(", "))
        }
        PatternKind::Record { elements } => {
            let mut elements = elements.iter().map(|e| match e {
                RecordElement::Named(name, pattern) => {
                    let pattern = pretty_pattern(context, state, *pattern);
                    format!("{name}: {pattern}")
                }
                RecordElement::Pun(name) => name.to_string(),
            });
            format!("{{ {} }}", elements.join(", "))
        }
        PatternKind::Constructor { constructor } => pretty_constructor(context, state, constructor),
    }
}

fn pretty_constructor<Q>(
    context: &CheckContext<Q>,
    state: &CheckState,
    constructor: &PatternConstructor,
) -> String
where
    Q: ExternalQueries,
{
    match constructor {
        PatternConstructor::DataConstructor { file_id, item_id, fields } => {
            let name = lookup_constructor_name(context, *file_id, *item_id)
                .unwrap_or_else(|| "<Unknown>".to_string());

            if fields.is_empty() {
                return name;
            }

            let mut field_strings = fields.iter().map(|&id| {
                let rendered = pretty_pattern(context, state, id);
                let pattern = &state.patterns[id];
                if let PatternKind::Constructor { constructor } = &pattern.kind
                    && !constructor.fields().is_empty()
                {
                    format!("({rendered})")
                } else {
                    rendered
                }
            });

            format!("{} {}", name, field_strings.join(" "))
        }
        PatternConstructor::Boolean(b) => b.to_string(),
        PatternConstructor::Char(c) => format!("'{c}'"),
        PatternConstructor::String(s) => format!("\"{s}\""),
        PatternConstructor::Integer(i) => i.to_string(),
        PatternConstructor::Number(negative, n) => {
            if *negative {
                format!("-{n}")
            } else {
                n.to_string()
            }
        }
    }
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
