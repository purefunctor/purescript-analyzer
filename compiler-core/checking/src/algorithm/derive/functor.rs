//! Implements derive for Functor and Bifunctor.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::algorithm::derive::variance::{Variance, VarianceConfig};
use crate::algorithm::derive::{self, DeriveStrategy, tools};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::error::ErrorKind;

/// Checks a derive instance head for Functor.
pub fn check_derive_functor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
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

    let Some((data_file, data_id)) = derive::extract_type_constructor(state, derived_type) else {
        let type_message = state.render_local_type(context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    let functor = Some((input.class_file, input.class_id));
    tools::register_derived_instance(state, context, input)?;

    let config = VarianceConfig::Single((Variance::Covariant, functor));
    Ok(Some(DeriveStrategy::VarianceConstraints { data_file, data_id, derived_type, config }))
}

/// Checks a derive instance head for Bifunctor.
pub fn check_derive_bifunctor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
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

    let Some((data_file, data_id)) = derive::extract_type_constructor(state, derived_type) else {
        let type_message = state.render_local_type(context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    // Bifunctor derivation emits Functor constraints for wrapped parameters.
    let functor = context.known_types.functor;
    tools::register_derived_instance(state, context, input)?;

    let config =
        VarianceConfig::Pair((Variance::Covariant, functor), (Variance::Covariant, functor));

    Ok(Some(DeriveStrategy::VarianceConstraints { data_file, data_id, derived_type, config }))
}
