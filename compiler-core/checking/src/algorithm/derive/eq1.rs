//! Implements derive for Eq1 and Ord1.
//!
//! Eq1 and Ord1 derivation is simple: it delegates to the base class.
//! The derived implementations are `eq1 = eq` and `compare1 = compare`.
//!
//! For `derive instance Eq1 Identity` to work, there must be an instance
//! available for `Eq (Identity a)`. The derivation algorithm:
//!
//! 1. Creates a fresh skolem `a` and the given constraint `Eq a`
//! 2. Creates a wanted constraint for `Eq (Identity a)`
//! 3. Solves the constraints to determine if it's satisfiable

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::algorithm::derive::{self, DeriveStrategy, tools};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::error::ErrorKind;

/// Checks a derive instance head for Eq1.
pub fn check_derive_eq1<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let Some(eq) = context.known_types.eq else {
        state.insert_error(ErrorKind::CannotDeriveClass {
            class_file: input.class_file,
            class_id: input.class_id,
        });
        return Ok(None);
    };

    check_derive_class1(state, context, input, eq)
}

/// Checks a derive instance head for Ord1.
pub fn check_derive_ord1<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let Some(ord) = context.known_types.ord else {
        state.insert_error(ErrorKind::CannotDeriveClass {
            class_file: input.class_file,
            class_id: input.class_id,
        });
        return Ok(None);
    };

    check_derive_class1(state, context, input, ord)
}

/// Shared implementation for Eq1 and Ord1 head phase.
fn check_derive_class1<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
    class: (files::FileId, indexing::TypeItemId),
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let [(derived_type, _)] = input.arguments[..] else {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file: input.class_file,
            class_id: input.class_id,
            expected: 1,
            actual: input.arguments.len(),
        });
        return Ok(None);
    };

    if derive::extract_type_constructor(state, derived_type).is_none() {
        let type_message = state.render_local_type(context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    tools::register_derived_instance(state, context, input)?;

    Ok(Some(DeriveStrategy::DelegateConstraint { derived_type, class }))
}
