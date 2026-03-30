use std::iter;

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{KindOrType, Type, TypeId, normalise};
use crate::state::CheckState;

use super::instances::can_determine_stuck;
use super::{ConstraintApplication, MatchInstance, MatchType};

/// Matches a wanted constraint to given constraints.
pub fn match_given_instances<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: &ConstraintApplication,
    given: &[ConstraintApplication],
) -> QueryResult<Option<MatchInstance>>
where
    Q: ExternalQueries,
{
    'given: for given in given {
        if wanted.file_id != given.file_id || wanted.item_id != given.item_id {
            continue;
        }

        if wanted.arguments.len() != given.arguments.len() {
            continue;
        }

        let mut equalities = vec![];
        let mut type_match_results = vec![];

        let mut stuck_positions = vec![];
        let mut stuck_type_positions = vec![];

        for (index, (&wanted_argument, &given_argument)) in
            iter::zip(wanted.arguments.iter(), given.arguments.iter()).enumerate()
        {
            let match_result = match (wanted_argument, given_argument) {
                (KindOrType::Kind(wanted_argument), KindOrType::Kind(given_argument))
                | (KindOrType::Type(wanted_argument), KindOrType::Type(given_argument)) => {
                    match_given_type(state, context, wanted_argument, given_argument)?
                }
                _ => continue 'given,
            };

            if matches!(match_result, MatchType::Apart) {
                continue 'given;
            }

            if matches!(match_result, MatchType::Stuck) {
                stuck_positions.push(index);
            }

            if matches!(wanted_argument, KindOrType::Type(_)) {
                let type_index = type_match_results.len();
                if matches!(match_result, MatchType::Stuck) {
                    stuck_type_positions.push(type_index);
                }
                type_match_results.push(match_result);
            }
        }

        if !stuck_type_positions.is_empty()
            && !can_determine_stuck(
                context,
                wanted.file_id,
                wanted.item_id,
                &type_match_results,
                &stuck_type_positions,
            )?
        {
            return Ok(Some(MatchInstance::Stuck));
        }

        for &index in &stuck_positions {
            let wanted_argument = match wanted.arguments[index] {
                KindOrType::Kind(argument) | KindOrType::Type(argument) => argument,
            };
            let given_argument = match given.arguments[index] {
                KindOrType::Kind(argument) | KindOrType::Type(argument) => argument,
            };
            equalities.push((wanted_argument, given_argument));
        }

        return Ok(Some(MatchInstance::Match { constraints: vec![], equalities }));
    }

    Ok(None)
}

/// Matches an argument from a wanted constraint to one from a given constraint.
///
/// This function is specialised for matching given constraints, like those
/// found in value signatures rather than top-level instance declarations;
/// unlike [`match_type`], this function does not build bindings or equalities
/// for rigid variables.
///
/// [`match_type`]: super::match_type
fn match_given_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    wanted: TypeId,
    given: TypeId,
) -> QueryResult<MatchType>
where
    Q: ExternalQueries,
{
    let wanted = normalise::expand(state, context, wanted)?;
    let given = normalise::expand(state, context, given)?;

    if wanted == given {
        return Ok(MatchType::Match);
    }

    let wanted_t = context.lookup_type(wanted);
    let given_t = context.lookup_type(given);

    match (wanted_t, given_t) {
        (Type::Unification(_), _) => Ok(MatchType::Stuck),

        (Type::Rigid(wanted_name, _, wanted_kind), Type::Rigid(given_name, _, given_kind)) => {
            if wanted_name == given_name {
                match_given_type(state, context, wanted_kind, given_kind)
            } else {
                Ok(MatchType::Apart)
            }
        }

        (
            Type::Application(wanted_function, wanted_argument),
            Type::Application(given_function, given_argument),
        ) => match_given_type(state, context, wanted_function, given_function)?
            .and_then(|| match_given_type(state, context, wanted_argument, given_argument)),

        (
            Type::Function(wanted_argument, wanted_result),
            Type::Function(given_argument, given_result),
        ) => match_given_type(state, context, wanted_argument, given_argument)?
            .and_then(|| match_given_type(state, context, wanted_result, given_result)),

        (Type::Function(wanted_argument, wanted_result), Type::Application(_, _)) => {
            let wanted = context.intern_function_application(wanted_argument, wanted_result);
            match_given_type(state, context, wanted, given)
        }

        (Type::Application(_, _), Type::Function(given_argument, given_result)) => {
            let given = context.intern_function_application(given_argument, given_result);
            match_given_type(state, context, wanted, given)
        }

        (Type::Row(wanted_row_id), Type::Row(given_row_id)) => {
            let wanted_row = context.lookup_row_type(wanted_row_id);
            let given_row = context.lookup_row_type(given_row_id);

            if wanted_row.fields.len() != given_row.fields.len() {
                return Ok(MatchType::Apart);
            }

            let mut result = MatchType::Match;
            for (wanted_field, given_field) in
                iter::zip(wanted_row.fields.iter(), given_row.fields.iter())
            {
                if wanted_field.label != given_field.label {
                    return Ok(MatchType::Apart);
                }
                result = result.and_then(|| {
                    match_given_type(state, context, wanted_field.id, given_field.id)
                })?;
            }

            match (wanted_row.tail, given_row.tail) {
                (Some(wanted_tail), Some(given_tail)) => {
                    result.and_then(|| match_given_type(state, context, wanted_tail, given_tail))
                }
                (Some(wanted_tail), None) => {
                    let wanted_tail = normalise::expand(state, context, wanted_tail)?;
                    if matches!(context.lookup_type(wanted_tail), Type::Unification(_)) {
                        Ok(MatchType::Stuck)
                    } else {
                        Ok(MatchType::Apart)
                    }
                }
                (None, Some(given_tail)) => {
                    let given_tail = normalise::expand(state, context, given_tail)?;
                    if matches!(context.lookup_type(given_tail), Type::Unification(_)) {
                        Ok(MatchType::Stuck)
                    } else {
                        Ok(MatchType::Apart)
                    }
                }
                (None, None) => Ok(result),
            }
        }

        (
            Type::KindApplication(wanted_function, wanted_argument),
            Type::KindApplication(given_function, given_argument),
        ) => match_given_type(state, context, wanted_function, given_function)?
            .and_then(|| match_given_type(state, context, wanted_argument, given_argument)),

        (Type::Kinded(wanted_inner, wanted_kind), Type::Kinded(given_inner, given_kind)) => {
            match_given_type(state, context, wanted_inner, given_inner)?
                .and_then(|| match_given_type(state, context, wanted_kind, given_kind))
        }

        _ => Ok(MatchType::Apart),
    }
}
