//! Implements derive for Foldable and Bifoldable.

use building_types::QueryResult;

use super::variance::{Variance, VarianceConfig, generate_variance_constraints};
use super::tools;
use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::transfer;
use crate::error::ErrorKind;

/// Checks a derive instance for Foldable.
pub fn check_derive_foldable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
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
        return Ok(());
    };

    let Some((data_file, data_id)) = super::extract_type_constructor(state, derived_type) else {
        let global_type = transfer::globalize(state, context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
        return Ok(());
    };

    let foldable = Some((input.class_file, input.class_id));
    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    let config = VarianceConfig::Single((Variance::Covariant, foldable));
    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}

/// Checks a derive instance for Bifoldable.
pub fn check_derive_bifoldable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
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
        return Ok(());
    };

    let Some((data_file, data_id)) = super::extract_type_constructor(state, derived_type) else {
        let global_type = transfer::globalize(state, context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
        return Ok(());
    };

    // Bifoldable derivation emits Foldable constraints for wrapped parameters.
    let foldable = context.known_types.foldable;
    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    let config =
        VarianceConfig::Pair((Variance::Covariant, foldable), (Variance::Covariant, foldable));

    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}
