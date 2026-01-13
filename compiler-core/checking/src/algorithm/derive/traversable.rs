//! Implements derive for Traversable and Bitraversable.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::algorithm::derive::tools;
use crate::algorithm::derive::variance::{Variance, VarianceConfig, generate_variance_constraints};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::transfer;
use crate::error::ErrorKind;

/// Checks a derive instance for Traversable.
pub fn check_derive_traversable<Q>(
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

    let traversable = Some((input.class_file, input.class_id));
    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    let config = VarianceConfig::Single((Variance::Covariant, traversable));
    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}

/// Checks a derive instance for Bitraversable.
pub fn check_derive_bitraversable<Q>(
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

    // Bitraversable derivation emits Traversable constraints for wrapped parameters.
    let traversable = context.known_types.traversable;
    tools::push_given_constraints(state, &input.constraints);
    tools::register_derived_instance(state, context, input);

    let config = VarianceConfig::Pair(
        (Variance::Covariant, traversable),
        (Variance::Covariant, traversable),
    );

    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}
