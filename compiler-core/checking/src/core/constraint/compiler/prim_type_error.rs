use building_types::QueryResult;
use lowering::StringKind;
use smol_str::{SmolStr, format_smolstr};

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint::MatchInstance;
use crate::core::pretty::Pretty;
use crate::core::{Type, TypeId, normalise, toolkit, zonk};
use crate::error::ErrorKind;
use crate::state::CheckState;

fn is_stuck<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let id = normalise::expand(state, context, id)?;
    Ok(matches!(context.lookup_type(id), Type::Unification(_)))
}

fn extract_symbol_text<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<SmolStr>>
where
    Q: ExternalQueries,
{
    let id = normalise::expand(state, context, id)?;
    if let Type::String(_, smol_str_id) = context.lookup_type(id) {
        Ok(Some(context.queries.lookup_smol_str(smol_str_id)))
    } else {
        Ok(None)
    }
}

fn extract_symbol_with_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<Option<(StringKind, SmolStr)>>
where
    Q: ExternalQueries,
{
    let id = normalise::expand(state, context, id)?;
    if let Type::String(kind, smol_id) = context.lookup_type(id) {
        Ok(Some((kind, context.queries.lookup_smol_str(smol_id))))
    } else {
        Ok(None)
    }
}

fn is_valid_label(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_lowercase() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_' || c == '\'')
}

fn render_label(kind: StringKind, text: &str) -> SmolStr {
    if is_valid_label(text) {
        SmolStr::new(text)
    } else {
        match kind {
            StringKind::String => SmolStr::new(format!(r#""{text}""#)),
            StringKind::RawString => SmolStr::new(format!(r#""""{text}""""#)),
        }
    }
}

/// Renders a `Doc` type into a string for custom type error messages.
///
/// Returns `None` if the doc is stuck on an unsolved unification variable,
/// signalling that the constraint solver should return `Stuck`.
fn render_doc<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    doc: TypeId,
) -> QueryResult<Option<SmolStr>>
where
    Q: ExternalQueries,
{
    let doc = normalise::expand(state, context, doc)?;

    if matches!(context.lookup_type(doc), Type::Unification(_)) {
        return Ok(None);
    }

    let (constructor, arguments) = toolkit::extract_type_application(state, context, doc)?;
    let prim = &context.prim_type_error;

    if constructor == prim.text {
        let &[symbol] = arguments.as_slice() else { return Ok(None) };
        if is_stuck(state, context, symbol)? {
            return Ok(None);
        }
        extract_symbol_text(state, context, symbol)
    } else if constructor == prim.quote {
        let &[quoted_type] = arguments.as_slice() else { return Ok(None) };
        if is_stuck(state, context, quoted_type)? {
            return Ok(None);
        }
        let quoted_type = zonk::zonk(state, context, quoted_type)?;
        let rendered = Pretty::new(context.queries, &state.checked).render(quoted_type);
        Ok(Some(rendered))
    } else if constructor == prim.quote_label {
        let &[symbol] = arguments.as_slice() else { return Ok(None) };
        if is_stuck(state, context, symbol)? {
            return Ok(None);
        }
        Ok(extract_symbol_with_kind(state, context, symbol)?
            .map(|(kind, text)| render_label(kind, &text)))
    } else if constructor == prim.beside {
        let &[left, right] = arguments.as_slice() else { return Ok(None) };
        let Some(left_rendered) = render_doc(state, context, left)? else {
            return Ok(None);
        };
        let Some(right_rendered) = render_doc(state, context, right)? else {
            return Ok(None);
        };
        Ok(Some(format_smolstr!("{left_rendered}{right_rendered}")))
    } else if constructor == prim.above {
        let &[upper, lower] = arguments.as_slice() else { return Ok(None) };
        let Some(upper_rendered) = render_doc(state, context, upper)? else {
            return Ok(None);
        };
        let Some(lower_rendered) = render_doc(state, context, lower)? else {
            return Ok(None);
        };
        Ok(Some(format_smolstr!("{upper_rendered}\n{lower_rendered}")))
    } else {
        Ok(None)
    }
}

pub fn match_warn<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[doc] = arguments else { return Ok(None) };

    let Some(message) = render_doc(state, context, doc)? else {
        return Ok(Some(MatchInstance::Stuck));
    };

    let message_id = context.queries.intern_smol_str(message);
    state.insert_error(ErrorKind::CustomWarning { message_id });

    Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![] }))
}

pub fn match_fail<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[doc] = arguments else { return Ok(None) };

    let Some(message) = render_doc(state, context, doc)? else {
        return Ok(Some(MatchInstance::Stuck));
    };

    let message_id = context.queries.intern_smol_str(message);
    state.insert_error(ErrorKind::CustomFailure { message_id });

    Ok(Some(MatchInstance::Match { constraints: vec![], equalities: vec![] }))
}
