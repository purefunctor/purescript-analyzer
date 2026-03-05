use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::toolkit;
use crate::error::ErrorKind;
use crate::state::CheckState;

use super::DeriveStrategy;

pub fn check_derive_eq1<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: &[crate::core::TypeId],
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let Some(eq) = context.known_types.eq else {
        state.insert_error(ErrorKind::CannotDeriveClass { class_file, class_id });
        return Ok(None);
    };

    check_derive_delegate_constraint(state, context, class_file, class_id, arguments, eq)
}

pub fn check_derive_ord1<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: &[crate::core::TypeId],
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let Some(ord) = context.known_types.ord else {
        state.insert_error(ErrorKind::CannotDeriveClass { class_file, class_id });
        return Ok(None);
    };

    check_derive_delegate_constraint(state, context, class_file, class_id, arguments, ord)
}

fn check_derive_delegate_constraint<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: &[crate::core::TypeId],
    class: (FileId, TypeItemId),
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

    if toolkit::extract_type_constructor(state, context, *derived_type)?.is_none() {
        let type_message = state.pretty_id(context, *derived_type)?;
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    }

    Ok(Some(DeriveStrategy::DelegateConstraint { derived_type: *derived_type, class }))
}
