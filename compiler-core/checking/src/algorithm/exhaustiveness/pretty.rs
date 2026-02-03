use std::sync::Arc;

use files::FileId;
use indexing::TermItemId;
use smol_str::{SmolStr, SmolStrBuilder};

use crate::ExternalQueries;
use crate::algorithm::exhaustiveness::{PatternConstructor, PatternId, PatternKind, WitnessVector};
use crate::algorithm::state::{CheckContext, CheckState};

pub fn pretty_witness<Q>(
    context: &CheckContext<Q>,
    state: &CheckState,
    witness: &WitnessVector,
) -> SmolStr
where
    Q: ExternalQueries,
{
    join_smolstr(witness.iter().map(|&id| pretty_pattern(context, state, id)), ", ")
}

fn pretty_pattern<Q>(context: &CheckContext<Q>, state: &CheckState, id: PatternId) -> SmolStr
where
    Q: ExternalQueries,
{
    let pattern = &state.patterns[id];
    match &pattern.kind {
        PatternKind::Wildcard => SmolStr::new_inline("_"),
        PatternKind::Constructor { constructor } => pretty_constructor(context, state, constructor),
    }
}

fn join_smolstr(iterator: impl Iterator<Item = SmolStr>, separator: &str) -> SmolStr {
    let mut builder = SmolStrBuilder::default();
    join_with_sep(&mut builder, iterator, separator, |builder, item| builder.push_str(&item));
    builder.finish()
}

fn join_with_sep<T>(
    builder: &mut SmolStrBuilder,
    iter: impl Iterator<Item = T>,
    sep: &str,
    mut render: impl FnMut(&mut SmolStrBuilder, T),
) {
    let mut first = true;
    for item in iter {
        if !first {
            builder.push_str(sep);
        }
        first = false;
        render(builder, item);
    }
}

fn pretty_constructor<Q>(
    context: &CheckContext<Q>,
    state: &CheckState,
    constructor: &PatternConstructor,
) -> SmolStr
where
    Q: ExternalQueries,
{
    match constructor {
        PatternConstructor::DataConstructor { file_id, item_id, fields } => {
            let name = lookup_constructor_name(context, *file_id, *item_id)
                .unwrap_or_else(|| SmolStr::new_inline("<Unknown>"));

            if fields.is_empty() {
                return name;
            }

            let mut builder = SmolStrBuilder::default();
            builder.push_str(&name);

            for &id in fields.iter() {
                builder.push(' ');
                let rendered = pretty_pattern(context, state, id);
                let pattern = &state.patterns[id];
                if let PatternKind::Constructor { constructor } = &pattern.kind
                    && !constructor.fields().is_empty()
                {
                    builder.push('(');
                    builder.push_str(&rendered);
                    builder.push(')');
                } else {
                    builder.push_str(&rendered);
                }
            }

            builder.finish()
        }
        PatternConstructor::Record { labels, fields } => {
            if labels.len() != fields.len() {
                return SmolStr::new_inline("{ <invalid> }");
            }

            let mut builder = SmolStrBuilder::default();
            builder.push_str("{ ");
            join_with_sep(
                &mut builder,
                labels.iter().zip(fields.iter()),
                ", ",
                |b, (label, field_id)| {
                    let field = pretty_pattern(context, state, *field_id);
                    b.push_str(label);
                    b.push_str(": ");
                    b.push_str(&field);
                },
            );
            builder.push_str(" }");
            builder.finish()
        }
        PatternConstructor::Array { fields } => {
            let mut builder = SmolStrBuilder::default();
            builder.push('[');
            join_with_sep(&mut builder, fields.iter().copied(), ", ", |b, id| {
                let rendered = pretty_pattern(context, state, id);
                b.push_str(&rendered);
            });
            builder.push(']');
            builder.finish()
        }
        PatternConstructor::Boolean(b) => SmolStr::from(b.to_string()),
        PatternConstructor::Char(c) => {
            let mut builder = SmolStrBuilder::default();
            builder.push('\'');
            builder.push(*c);
            builder.push('\'');
            builder.finish()
        }
        PatternConstructor::String(s) => {
            let mut builder = SmolStrBuilder::default();
            builder.push('"');
            builder.push_str(s);
            builder.push('"');
            builder.finish()
        }
        PatternConstructor::Integer(i) => SmolStr::from(i.to_string()),
        PatternConstructor::Number(negative, n) => {
            let mut builder = SmolStrBuilder::default();
            if *negative {
                builder.push('-');
            }
            builder.push_str(n.as_ref());
            builder.finish()
        }
    }
}

fn lookup_constructor_name<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    term_id: TermItemId,
) -> Option<SmolStr>
where
    Q: ExternalQueries,
{
    let indexed = if file_id == context.id {
        Arc::clone(&context.indexed)
    } else {
        context.queries.indexed(file_id).ok()?
    };

    let item = &indexed.items[term_id];
    item.name.clone()
}
