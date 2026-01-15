//! Implements derive for Functor and Bifunctor.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::algorithm::derive::variance::{Variance, VarianceConfig, generate_variance_constraints};
use crate::algorithm::derive::{self, tools};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::transfer;
use crate::error::ErrorKind;

/// Checks a derive instance for Functor.
pub fn check_derive_functor<Q>(
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

    let Some((data_file, data_id)) = derive::extract_type_constructor(state, derived_type) else {
        let global_type = transfer::globalize(state, context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
        return Ok(());
    };

    let functor = Some((input.class_file, input.class_id));
    tools::push_given_constraints(state, &input.constraints);
    tools::emit_superclass_constraints(state, context, &input)?;
    tools::register_derived_instance(state, context, input);

    let config = VarianceConfig::Single((Variance::Covariant, functor));
    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}

/// Checks a derive instance for Bifunctor.
pub fn check_derive_bifunctor<Q>(
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

    let Some((data_file, data_id)) = derive::extract_type_constructor(state, derived_type) else {
        let global_type = transfer::globalize(state, context, derived_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_id: global_type });
        return Ok(());
    };

    // Bifunctor derivation emits Functor constraints for wrapped parameters.
    let functor = context.known_types.functor;
    tools::push_given_constraints(state, &input.constraints);
    tools::emit_superclass_constraints(state, context, &input)?;
    tools::register_derived_instance(state, context, input);

    let config =
        VarianceConfig::Pair((Variance::Covariant, functor), (Variance::Covariant, functor));

    generate_variance_constraints(state, context, data_file, data_id, derived_type, config)?;

    tools::solve_and_report_constraints(state, context)
}
