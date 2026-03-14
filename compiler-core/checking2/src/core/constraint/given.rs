use std::iter;

use building_types::QueryResult;
use itertools::Itertools;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{KindOrType, Type, TypeId, normalise};
use crate::state::CheckState;

use super::{
    ConstraintApplication, MatchInstance, MatchType, constraint_application, elaborate_superclasses,
};

pub fn elaborate_given<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    given: &[TypeId],
) -> QueryResult<Vec<ConstraintApplication>>
where
    Q: ExternalQueries,
{
    let mut elaborated = vec![];

    for &constraint in given {
        elaborated.push(constraint);
        elaborate_superclasses(state, context, constraint, &mut elaborated)?;
    }

    let mut applications = elaborated
        .into_iter()
        .map(|constraint| constraint_application(state, context, constraint))
        .filter_map_ok(|constraint| constraint)
        .collect::<QueryResult<Vec<_>>>()?;

    let symmetric = applications.iter().filter_map(|application| {
        let is_coercible = application.file_id == context.prim_coerce.file_id
            && application.item_id == context.prim_coerce.coercible;

        let type_positions = application
            .arguments
            .iter()
            .enumerate()
            .filter_map(|(index, argument)| {
                matches!(argument, KindOrType::Type(_)).then_some(index)
            })
            .collect_vec();

        let &[left, right] = type_positions.as_slice() else {
            return None;
        };

        let mut arguments = application.arguments.clone();
        arguments.swap(left, right);

        is_coercible.then(|| ConstraintApplication {
            file_id: application.file_id,
            item_id: application.item_id,
            arguments,
        })
    });

    let symmetric = symmetric.collect_vec();
    applications.extend(symmetric);

    Ok(applications)
}

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

        let mut stuck_positions = vec![];

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
        }

        // Given constraints are valid by construction. When a unification
        // variable makes a position stuck, it's safe to emit an equality
        // rather than require functional dependencies to cover it.
        let equalities = stuck_positions
            .into_iter()
            .map(|index| match (wanted.arguments[index], given.arguments[index]) {
                (KindOrType::Kind(wanted_argument), KindOrType::Kind(given_argument))
                | (KindOrType::Type(wanted_argument), KindOrType::Type(given_argument)) => {
                    (wanted_argument, given_argument)
                }
                _ => unreachable!("kind/type mismatch after positional argument matching"),
            })
            .collect();

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

        _ => Ok(MatchType::Apart),
    }
}
