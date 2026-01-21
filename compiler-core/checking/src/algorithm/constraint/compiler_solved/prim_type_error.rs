use lowering::StringKind;
use smol_str::SmolStr;

use crate::algorithm::constraint::MatchInstance;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::toolkit;
use crate::core::pretty;
use crate::error::ErrorKind;
use crate::{ExternalQueries, Type, TypeId};

/// Checks if a type is stuck on a unification variable at its head.
fn is_stuck(state: &mut CheckState, type_id: TypeId) -> bool {
    let type_id = state.normalize_type(type_id);
    matches!(state.storage[type_id], Type::Unification(_))
}

/// Extracts a symbol from a type, returning `None` if stuck on a unification variable.
fn extract_symbol_or_stuck(state: &mut CheckState, id: TypeId) -> Option<String> {
    let id = state.normalize_type(id);

    if matches!(state.storage[id], Type::Unification(_)) {
        return None;
    }

    if let Type::String(_, value) = &state.storage[id] { Some(value.to_string()) } else { None }
}

/// Extracts a symbol with its kind from a type, returning `None` if stuck.
fn extract_symbol_with_kind(state: &mut CheckState, id: TypeId) -> Option<(StringKind, SmolStr)> {
    let id = state.normalize_type(id);

    if matches!(state.storage[id], Type::Unification(_)) {
        return None;
    }

    match &state.storage[id] {
        Type::String(kind, value) => Some((*kind, SmolStr::clone(value))),
        _ => None,
    }
}

/// Checks if a string is a valid PureScript label.
///
/// Matches the lexer rules: starts with lowercase letter or `_`,
/// continues with alphanumeric, `_`, or `'`.
fn is_valid_label(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_lowercase() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_' || c == '\'')
}

/// Renders a label, quoting it if necessary.
///
/// Preserves the original string kind (regular vs raw) when quoting.
fn render_label(kind: StringKind, s: &str) -> String {
    if is_valid_label(s) {
        s.to_string()
    } else {
        match kind {
            StringKind::String => format!(r#""{s}""#),
            StringKind::RawString => format!(r#""""{s}""""#),
        }
    }
}

/// Renders a `Doc` type into a string for custom type error messages.
///
/// Returns `None` if the doc is stuck on a unification variable.
fn render_doc<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    type_id: TypeId,
) -> Option<String>
where
    Q: ExternalQueries,
{
    let type_id = state.normalize_type(type_id);

    if matches!(state.storage[type_id], Type::Unification(_)) {
        return None;
    }

    let (constructor, arguments) = toolkit::extract_type_application(state, type_id);
    let prim = &context.prim_type_error;

    if constructor == prim.text {
        let &[symbol] = arguments.as_slice() else { return None };
        extract_symbol_or_stuck(state, symbol)
    } else if constructor == prim.quote {
        let &[t] = arguments.as_slice() else { return None };
        if is_stuck(state, t) {
            return None;
        }
        Some(pretty::print_local(state, context, t))
    } else if constructor == prim.quote_label {
        let &[symbol] = arguments.as_slice() else { return None };
        extract_symbol_with_kind(state, symbol).map(|(kind, s)| render_label(kind, &s))
    } else if constructor == prim.beside {
        let &[left, right] = arguments.as_slice() else { return None };
        let l = render_doc(state, context, left)?;
        let r = render_doc(state, context, right)?;
        Some(format!("{}{}", l, r))
    } else if constructor == prim.above {
        let &[upper, lower] = arguments.as_slice() else { return None };
        let u = render_doc(state, context, upper)?;
        let d = render_doc(state, context, lower)?;
        Some(format!("{}\n{}", u, d))
    } else {
        None
    }
}

/// Solver for `Prim.TypeError.Warn`.
///
/// Emits a custom warning message and satisfies the constraint.
pub fn prim_warn<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[doc] = arguments else { return None };

    let Some(message) = render_doc(state, context, doc) else {
        return Some(MatchInstance::Stuck);
    };

    let message_id = state.intern_error_message(message);
    state.insert_error(ErrorKind::CustomWarning { message_id });

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![] })
}

/// Solver for `Prim.TypeError.Fail`.
///
/// Emits a custom error message and satisfies the constraint.
pub fn prim_fail<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> Option<MatchInstance>
where
    Q: ExternalQueries,
{
    let &[doc] = arguments else { return None };

    let Some(message) = render_doc(state, context, doc) else {
        return Some(MatchInstance::Stuck);
    };

    let message_id = state.intern_error_message(message);
    state.insert_error(ErrorKind::CustomFailure { message_id });

    Some(MatchInstance::Match { constraints: vec![], equalities: vec![] })
}
