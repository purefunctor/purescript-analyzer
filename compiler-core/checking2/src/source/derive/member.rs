use building_types::QueryResult;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::error::{ErrorCrumb, ErrorKind};
use crate::state::CheckState;

use super::{DeriveHeadResult, DeriveStrategy, field, tools};

pub fn check_derive_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    derives: &[DeriveHeadResult],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for result in derives {
        state.with_error_crumb(ErrorCrumb::TermDeclaration(result.item_id), |state| {
            state.with_implication(|state| check_derive_member(state, context, result))
        })?;
    }
    Ok(())
}

fn check_derive_member<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    result: &DeriveHeadResult,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for &constraint in &result.constraints {
        state.push_given(constraint);
    }

    match result.strategy {
        DeriveStrategy::FieldConstraints { data_file, data_id, derived_type, class } => {
            tools::emit_superclass_constraints(
                state,
                context,
                result.class_file,
                result.class_id,
                &result.arguments,
            )?;
            field::generate_field_constraints(
                state,
                context,
                data_file,
                data_id,
                derived_type,
                class,
            )?;
            tools::solve_and_report_constraints(state, context)?;
        }
        DeriveStrategy::Unsupported => {
            state.insert_error(ErrorKind::DeriveNotSupportedYet {
                class_file: result.class_file,
                class_id: result.class_id,
            });
        }
    }
    Ok(())
}
