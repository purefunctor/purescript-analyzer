use building_types::QueryResult;
use files::FileId;
use indexing::TypeItemId;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{Type, TypeId, toolkit, unification};
use crate::error::ErrorKind;
use crate::state::CheckState;

use super::DeriveStrategy;

pub fn check_derive_newtype<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_file: FileId,
    class_id: TypeItemId,
    arguments: &[TypeId],
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let [preceding_arguments @ .., newtype_type] = arguments else {
        return Ok(None);
    };

    let Some((newtype_file, newtype_id)) =
        toolkit::extract_type_constructor(state, context, *newtype_type)?
    else {
        let type_message = state.pretty_id(context, *newtype_type)?;
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    if newtype_file != context.id || !toolkit::is_newtype(context, newtype_file, newtype_id)? {
        let type_message = state.pretty_id(context, *newtype_type)?;
        state.insert_error(ErrorKind::ExpectedNewtype { type_message });
        return Ok(None);
    }

    let Some(inner_type) =
        toolkit::get_newtype_inner(state, context, newtype_file, newtype_id, *newtype_type)?
    else {
        let type_message = state.pretty_id(context, *newtype_type)?;
        state.insert_error(ErrorKind::ExpectedNewtype { type_message });
        return Ok(None);
    };

    let class_type = context.queries.intern_type(Type::Constructor(class_file, class_id));
    let mut delegate_constraint = preceding_arguments
        .iter()
        .copied()
        .fold(class_type, |function, argument| context.intern_application(function, argument));
    delegate_constraint = context.intern_application(delegate_constraint, inner_type);

    Ok(Some(DeriveStrategy::NewtypeDeriveConstraint { delegate_constraint }))
}

pub fn check_derive_newtype_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    _class_file: FileId,
    _class_id: TypeItemId,
    arguments: &[TypeId],
) -> QueryResult<Option<DeriveStrategy>>
where
    Q: ExternalQueries,
{
    let [newtype_type, inner_type] = arguments else {
        return Ok(None);
    };

    let Some((newtype_file, newtype_id)) =
        toolkit::extract_type_constructor(state, context, *newtype_type)?
    else {
        let type_message = state.pretty_id(context, *newtype_type)?;
        state.insert_error(ErrorKind::CannotDeriveForType { type_message });
        return Ok(None);
    };

    if newtype_file != context.id || !toolkit::is_newtype(context, newtype_file, newtype_id)? {
        let type_message = state.pretty_id(context, *newtype_type)?;
        state.insert_error(ErrorKind::ExpectedNewtype { type_message });
        return Ok(None);
    }

    let Some(extracted_inner) =
        toolkit::get_newtype_inner(state, context, newtype_file, newtype_id, *newtype_type)?
    else {
        let type_message = state.pretty_id(context, *newtype_type)?;
        state.insert_error(ErrorKind::ExpectedNewtype { type_message });
        return Ok(None);
    };

    unification::unify(state, context, *inner_type, extracted_inner)?;

    Ok(Some(DeriveStrategy::HeadOnly))
}
