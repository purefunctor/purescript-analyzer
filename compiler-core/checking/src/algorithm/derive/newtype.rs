//! Implements derive for `Data.Newtype.Newtype`.

use building_types::QueryResult;

use crate::ExternalQueries;
use crate::algorithm::derive::{self, tools};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::unification;
use crate::error::ErrorKind;

pub fn check_derive_newtype<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: tools::ElaboratedDerive,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let [(newtype_type, _), (wildcard_type, _)] = input.arguments[..] else {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file: input.class_file,
            class_id: input.class_id,
            expected: 2,
            actual: input.arguments.len(),
        });
        return Ok(());
    };

    let Some((newtype_file, newtype_id)) = derive::extract_type_constructor(state, newtype_type)
    else {
        let type_message = state.render_local_type(context, newtype_type);
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(());
    };

    if !derive::is_newtype(context, newtype_file, newtype_id)? {
        let type_message = state.render_local_type(context, newtype_type);
        state.insert_error(ErrorKind::ExpectedNewtype { type_message });
        return Ok(());
    }

    let (inner_type, _) =
        derive::get_newtype_inner(state, context, newtype_file, newtype_id, newtype_type)?;

    let _ = unification::unify(state, context, wildcard_type, inner_type);

    tools::push_given_constraints(state, &input.constraints);
    tools::emit_superclass_constraints(state, context, &input)?;
    tools::register_derived_instance(state, context, input);
    tools::solve_and_report_constraints(state, context)
}
