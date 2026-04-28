use std::cmp::Ordering;

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint::matching::{self, InstanceMatch, MatchInstance};
use crate::core::{Type, TypeId, normalise};
use crate::state::CheckState;

use super::{extract_symbol, intern_symbol, match_equality};

pub fn match_append<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[left, right, appended] = arguments else {
        return Ok(None);
    };

    let left_symbol = extract_symbol(state, context, left)?;
    let right_symbol = extract_symbol(state, context, right)?;
    let appended_symbol = extract_symbol(state, context, appended)?;

    let matched = match (left_symbol, right_symbol, appended_symbol) {
        (Some(left_value), Some(right_value), _) => {
            let result = intern_symbol(context, &format!("{left_value}{right_value}"));
            match_equality(state, context, appended, result)?
        }
        (_, Some(right_value), Some(appended_value)) => {
            let Some(left_value) = appended_value.strip_suffix(right_value.as_str()) else {
                return Ok(Some(MatchInstance::Apart));
            };

            let result = intern_symbol(context, left_value);
            match_equality(state, context, left, result)?
        }
        (Some(left_value), _, Some(appended_value)) => {
            let Some(right_value) = appended_value.strip_prefix(left_value.as_str()) else {
                return Ok(Some(MatchInstance::Apart));
            };

            let result = intern_symbol(context, right_value);
            match_equality(state, context, right, result)?
        }
        _ => matching::blocking_constraint(state, context, &[left, right, appended])?,
    };

    Ok(Some(matched))
}

pub fn match_compare<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[left, right, ordering] = arguments else {
        return Ok(None);
    };

    let Some(left_symbol) = extract_symbol(state, context, left)? else {
        return Ok(Some(matching::blocking_constraint(state, context, &[left])?));
    };
    let Some(right_symbol) = extract_symbol(state, context, right)? else {
        return Ok(Some(matching::blocking_constraint(state, context, &[right])?));
    };

    let result = match left_symbol.cmp(&right_symbol) {
        Ordering::Less => context.prim_ordering.lt,
        Ordering::Equal => context.prim_ordering.eq,
        Ordering::Greater => context.prim_ordering.gt,
    };

    Ok(Some(match_equality(state, context, ordering, result)?))
}

pub fn match_cons<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[head, tail, symbol] = arguments else {
        return Ok(None);
    };

    let head_symbol = extract_symbol(state, context, head)?;
    let tail_symbol = extract_symbol(state, context, tail)?;
    let symbol_symbol = extract_symbol(state, context, symbol)?;

    let matched = match (&head_symbol, &tail_symbol, &symbol_symbol) {
        (Some(head_value), Some(tail_value), _) => {
            let mut chars = head_value.chars();
            let (Some(ch), None) = (chars.next(), chars.next()) else {
                return Ok(Some(MatchInstance::Apart));
            };

            let result = intern_symbol(context, &format!("{ch}{tail_value}"));
            match_equality(state, context, symbol, result)?
        }
        (_, _, Some(symbol_value)) => {
            let mut chars = symbol_value.chars();
            let Some(head_char) = chars.next() else {
                return Ok(Some(MatchInstance::Apart));
            };

            if let Some(head_value) = head_symbol {
                let mut head_chars = head_value.chars();
                if head_chars.next() != Some(head_char) || head_chars.next().is_some() {
                    return Ok(Some(MatchInstance::Apart));
                }
            }

            let head_result = intern_symbol(context, &head_char.to_string());
            let tail_result = intern_symbol(context, chars.as_str());
            MatchInstance::Match(InstanceMatch::from_unifications(vec![
                (head, head_result),
                (tail, tail_result),
            ]))
        }
        _ => matching::blocking_constraint(state, context, &[head, tail, symbol])?,
    };

    Ok(Some(matched))
}

pub fn match_is_symbol<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[symbol] = arguments else {
        return Ok(None);
    };

    let symbol = normalise::expand(state, context, symbol)?;

    let matched = if extract_symbol(state, context, symbol)?.is_some() {
        MatchInstance::Match(InstanceMatch::empty())
    } else if let Type::Unification(id) = context.lookup_type(symbol) {
        MatchInstance::Stuck(vec![id])
    } else {
        MatchInstance::Apart
    };

    Ok(Some(matched))
}
