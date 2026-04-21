use std::cmp::Ordering;

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint2::matching::{self, MatchInstance};
use crate::core::{TypeId, normalise};
use crate::state::CheckState;

use super::{extract_integer, intern_symbol, match_equality};

pub fn match_add<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[left, right, sum] = arguments else {
        return Ok(None);
    };

    let left_int = extract_integer(state, context, left)?;
    let right_int = extract_integer(state, context, right)?;
    let sum_int = extract_integer(state, context, sum)?;

    let matched = match (left_int, right_int, sum_int) {
        (Some(left), Some(right), _) => {
            let result = super::intern_integer(context, left + right);
            match_equality(state, context, sum, result)?
        }
        (Some(left), _, Some(sum_value)) => {
            let result = super::intern_integer(context, sum_value - left);
            match_equality(state, context, right, result)?
        }
        (_, Some(right), Some(sum_value)) => {
            let result = super::intern_integer(context, sum_value - right);
            match_equality(state, context, left, result)?
        }
        _ => matching::blocking_constraint(state, context, &[left, right, sum])?,
    };

    Ok(Some(matched))
}

pub fn match_mul<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[left, right, product] = arguments else {
        return Ok(None);
    };

    let Some(left_int) = extract_integer(state, context, left)? else {
        return Ok(Some(matching::blocking_constraint(state, context, &[left])?));
    };
    let Some(right_int) = extract_integer(state, context, right)? else {
        return Ok(Some(matching::blocking_constraint(state, context, &[right])?));
    };

    let result = super::intern_integer(context, left_int * right_int);
    Ok(Some(match_equality(state, context, product, result)?))
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

    let left = normalise::expand(state, context, left)?;
    let right = normalise::expand(state, context, right)?;
    let ordering = normalise::expand(state, context, ordering)?;

    let left_int = extract_integer(state, context, left)?;
    let right_int = extract_integer(state, context, right)?;

    if let (Some(left_int), Some(right_int)) = (left_int, right_int) {
        let result = match left_int.cmp(&right_int) {
            Ordering::Less => context.prim_ordering.lt,
            Ordering::Equal => context.prim_ordering.eq,
            Ordering::Greater => context.prim_ordering.gt,
        };

        return Ok(Some(match_equality(state, context, ordering, result)?));
    }

    Ok(Some(matching::blocking_constraint(state, context, &[left, right])?))
}

pub fn match_to_string<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    arguments: &[TypeId],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    let &[int, symbol] = arguments else {
        return Ok(None);
    };

    let Some(value) = extract_integer(state, context, int)? else {
        return Ok(Some(matching::blocking_constraint(state, context, &[int])?));
    };

    let result = intern_symbol(context, &value.to_string());
    Ok(Some(match_equality(state, context, symbol, result)?))
}
