use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::toolkit;
use crate::error::ErrorKind;
use crate::state::CheckState;

use super::DeriveStrategy;
use super::variance::{Variance, VarianceConfig};

pub(super) fn check_derive_traversable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: &[crate::core::TypeId],
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let [derived_type] = arguments else {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file,
            class_id,
            expected: 1,
            actual: arguments.len(),
        });
        return Ok(None);
    };

    let Some((data_file, data_id)) =
        toolkit::extract_type_constructor(state, context, *derived_type)?
    else {
        let type_message = state.pretty_id(context, *derived_type)?;
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    let traversable = Some((class_file, class_id));
    let config = VarianceConfig::Single((Variance::Covariant, traversable));

    Ok(Some(DeriveStrategy::VarianceConstraints {
        data_file,
        data_id,
        derived_type: *derived_type,
        config,
    }))
}

pub(super) fn check_derive_bitraversable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: &[crate::core::TypeId],
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let [derived_type] = arguments else {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file,
            class_id,
            expected: 1,
            actual: arguments.len(),
        });
        return Ok(None);
    };

    let Some((data_file, data_id)) =
        toolkit::extract_type_constructor(state, context, *derived_type)?
    else {
        let type_message = state.pretty_id(context, *derived_type)?;
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    let wrapper = context.known_types.traversable;
    let config =
        VarianceConfig::Pair((Variance::Covariant, wrapper), (Variance::Covariant, wrapper));

    Ok(Some(DeriveStrategy::VarianceConstraints {
        data_file,
        data_id,
        derived_type: *derived_type,
        config,
    }))
}
