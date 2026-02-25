//! Implements shared utilities for core type operations.

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TypeItemId};

use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{CheckedSynonym, ForallBinder, Type, TypeId, normalise};
use crate::state::CheckState;
use crate::{ExternalQueries, safe_loop};

pub struct InspectQuantified {
    pub binders: Vec<ForallBinder>,
    pub quantified: TypeId,
}

pub struct InspectFunction {
    pub arguments: Vec<TypeId>,
    pub result: TypeId,
}

pub fn lookup_file_type<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let kind = if file_id == context.id {
        state.checked.lookup_type(type_id)
    } else {
        let checked = context.queries.checked2(file_id)?;
        checked.lookup_type(type_id)
    };

    if let Some(kind) = kind { Ok(kind) } else { Ok(context.unknown("invalid type item")) }
}

pub fn lookup_file_term<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    term_id: TermItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let term = if file_id == context.id {
        state.checked.lookup_term(term_id)
    } else {
        let checked = context.queries.checked2(file_id)?;
        checked.lookup_term(term_id)
    };

    if let Some(term) = term { Ok(term) } else { Ok(context.unknown("invalid term item")) }
}

pub fn lookup_file_synonym<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<Option<CheckedSynonym>>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        Ok(state.checked.lookup_synonym(type_id))
    } else {
        let checked = context.queries.checked2(file_id)?;
        Ok(checked.lookup_synonym(type_id))
    }
}

pub fn lookup_file_type_operator<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let operator_kind = if file_id == context.id {
        state.checked.lookup_type(type_id)
    } else {
        let checked = context.queries.checked2(file_id)?;
        checked.lookup_type(type_id)
    };

    if let Some(operator_kind) = operator_kind {
        Ok(operator_kind)
    } else {
        Ok(context.unknown("invalid operator item"))
    }
}

pub fn lookup_file_term_operator<Q>(
    state: &CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    term_id: TermItemId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let operator_type = if file_id == context.id {
        state.checked.lookup_term(term_id)
    } else {
        let checked = context.queries.checked2(file_id)?;
        checked.lookup_term(term_id)
    };

    if let Some(operator_type) = operator_type {
        Ok(operator_type)
    } else {
        Ok(context.unknown("invalid operator item"))
    }
}

pub fn inspect_quantified<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<InspectQuantified>
where
    Q: ExternalQueries,
{
    let mut binders = vec![];
    let mut current = id;

    safe_loop! {
        current = normalise::normalise(state, context, current)?;

        let Type::Forall(binder_id, inner) = context.lookup_type(current) else {
            break;
        };

        binders.push(context.lookup_forall_binder(binder_id));
        current = inner;
    }

    Ok(InspectQuantified { binders, quantified: current })
}

pub fn inspect_function<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<InspectFunction>
where
    Q: ExternalQueries,
{
    let mut arguments = vec![];
    let mut current = id;

    safe_loop! {
        current = normalise::normalise(state, context, current)?;

        match context.lookup_type(current) {
            Type::Function(argument, result) => {
                arguments.push(argument);
                current = result;
            }
            Type::Application(function_argument, result) => {
                let function_argument = normalise::normalise(state, context, function_argument)?;
                let Type::Application(function, argument) = context.lookup_type(function_argument) else {
                    break;
                };
                let function = normalise::normalise(state, context, function)?;
                if function == context.prim.function {
                    arguments.push(argument);
                    current = result;
                } else {
                    break;
                }
            }
            _ => break,
        }
    }

    Ok(InspectFunction { arguments, result: current })
}

pub fn instantiate_unifications<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    safe_loop! {
        id = normalise::normalise(state, context, id)?;

        let Type::Forall(binder_id, inner) = context.lookup_type(id) else {
            break;
        };

        let binder = context.lookup_forall_binder(binder_id);
        let binder_kind = normalise::normalise(state, context, binder.kind)?;

        let replacement = state.fresh_unification(context.queries, binder_kind);
        id = SubstituteName::one(state, context, binder.name, replacement, inner)?;
    }

    Ok(id)
}
