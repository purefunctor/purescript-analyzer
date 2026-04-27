//! Implements shared utilities for core type operations.

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TypeItemId};
use lowering::TypeItemIr;

use crate::context::CheckContext;
use crate::core::substitute::SubstituteName;
use crate::core::{
    CheckedClass, CheckedInstance, CheckedSynonym, ForallBinder, KindOrType, Role, Type, TypeId,
    constraint, normalise, unification,
};
use crate::error::ErrorKind;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InspectMode {
    Some(usize),
    Full,
}

pub struct DecomposedInstance {
    pub binders: Vec<ForallBinder>,
    pub constraints: Vec<TypeId>,
    pub arguments: Vec<KindOrType>,
}

pub struct NewtypeInner {
    pub inner: TypeId,
    pub rigids: Vec<TypeId>,
}

pub fn report_invalid_type_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    function_type: TypeId,
    function_kind: TypeId,
    argument_type: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let function_type = state.pretty_id(context, function_type)?;
    let function_kind = state.pretty_id(context, function_kind)?;
    let argument_type = state.pretty_id(context, argument_type)?;

    state.insert_error(ErrorKind::InvalidTypeApplication {
        function_type,
        function_kind,
        argument_type,
    });

    Ok(())
}

pub fn extract_type_application<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<(TypeId, Vec<TypeId>)>
where
    Q: ExternalQueries,
{
    let mut arguments = vec![];

    safe_loop! {
        id = normalise::expand(state, context, id)?;
        match context.lookup_type(id) {
            Type::Application(function, argument) => {
                arguments.push(argument);
                id = function;
            }
            Type::KindApplication(function, _) => {
                id = function;
            }
            _ => break,
        }
    }

    arguments.reverse();
    Ok((id, arguments))
}

pub fn extract_all_applications<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<(TypeId, Vec<KindOrType>)>
where
    Q: ExternalQueries,
{
    let mut arguments = vec![];

    safe_loop! {
        id = normalise::expand(state, context, id)?;
        match context.lookup_type(id) {
            Type::Application(function, argument) => {
                arguments.push(crate::core::KindOrType::Type(argument));
                id = function;
            }
            Type::KindApplication(function, argument) => {
                arguments.push(crate::core::KindOrType::Kind(argument));
                id = function;
            }
            _ => break,
        }
    }

    arguments.reverse();
    Ok((id, arguments))
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
        let checked = context.queries.checked(file_id)?;
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
        let checked = context.queries.checked(file_id)?;
        checked.lookup_term(term_id)
    };

    if let Some(term) = term { Ok(term) } else { Ok(context.unknown("invalid term item")) }
}

pub fn lookup_term_variable<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: lowering::TermVariableResolution,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match resolution {
        lowering::TermVariableResolution::Binder(binder_id) => Ok(state
            .checked
            .nodes
            .lookup_binder(binder_id)
            .unwrap_or_else(|| context.unknown("unresolved binder"))),
        lowering::TermVariableResolution::Let(let_binding_id) => Ok(state
            .checked
            .nodes
            .lookup_let(let_binding_id)
            .unwrap_or_else(|| context.unknown("unresolved let"))),
        lowering::TermVariableResolution::RecordPun(pun_id) => Ok(state
            .checked
            .nodes
            .lookup_pun(pun_id)
            .unwrap_or_else(|| context.unknown("unresolved pun"))),
        lowering::TermVariableResolution::Reference(file_id, term_id) => {
            lookup_file_term(state, context, file_id, term_id)
        }
    }
}

pub fn lookup_file_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<Option<CheckedClass>>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        Ok(state.checked.lookup_class(item_id))
    } else {
        let checked = context.queries.checked(file_id)?;
        Ok(checked.lookup_class(item_id))
    }
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
        let checked = context.queries.checked(file_id)?;
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
        let checked = context.queries.checked(file_id)?;
        checked.lookup_type(type_id)
    };

    if let Some(operator_kind) = operator_kind {
        Ok(operator_kind)
    } else {
        Ok(context.unknown("invalid operator item"))
    }
}

pub fn resolve_type_operator_target<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> QueryResult<Option<(FileId, TypeItemId)>>
where
    Q: ExternalQueries,
{
    let resolve = |lowered: &lowering::LoweredModule| {
        lowered.info.get_type_item(type_id).and_then(|item| match item {
            TypeItemIr::Operator { resolution, .. } => *resolution,
            _ => None,
        })
    };

    if file_id == context.id {
        Ok(resolve(&context.lowered))
    } else {
        let lowered = context.queries.lowered(file_id)?;
        Ok(resolve(&lowered))
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
        let checked = context.queries.checked(file_id)?;
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
        current = normalise::expand(state, context, current)?;

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
    inspect_function_with(state, context, id, InspectMode::Full)
}

pub fn inspect_function_with<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
    mode: InspectMode,
) -> QueryResult<InspectFunction>
where
    Q: ExternalQueries,
{
    let mut arguments = vec![];
    let mut current = id;

    safe_loop! {
        current = normalise::expand(state, context, current)?;

        if let InspectMode::Some(required) = mode
            && arguments.len() >= required
        {
            return Ok(InspectFunction { arguments, result: current });
        }

        match context.lookup_type(current) {
            Type::Function(argument, result) => {
                arguments.push(argument);
                current = result;
            }
            Type::Application(function_argument, result) => {
                let function_argument = normalise::expand(state, context, function_argument)?;

                let Type::Application(function, argument) = context.lookup_type(function_argument)
                else {
                    return Ok(InspectFunction { arguments, result: current });
                };

                let function = normalise::expand(state, context, function)?;
                if function == context.prim.function {
                    arguments.push(argument);
                    current = result;
                } else {
                    return Ok(InspectFunction { arguments, result: current });
                }
            }
            _ => return Ok(InspectFunction { arguments, result: current }),
        }
    }
}

pub fn decompose_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    instance: &CheckedInstance,
) -> QueryResult<Option<DecomposedInstance>>
where
    Q: ExternalQueries,
{
    let InspectQuantified { binders, quantified } =
        inspect_quantified(state, context, instance.canonical)?;

    let mut current = quantified;
    let mut constraints = vec![];

    safe_loop! {
        current = normalise::expand(state, context, current)?;
        match context.lookup_type(current) {
            Type::Constrained(constraint, constrained) => {
                constraints.push(constraint);
                current = constrained;
            }
            _ => break,
        }
    }

    let Some(current) = constraint::canonical::canonicalise(state, context, current)? else {
        return Ok(None);
    };

    let current = state.canonicals[current].clone();
    let file_id = current.file_id;
    let item_id = current.type_id;
    let arguments = current.arguments.to_vec();

    if (file_id, item_id) != instance.resolution {
        return Ok(None);
    }

    Ok(Some(DecomposedInstance { binders, constraints, arguments }))
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
        id = normalise::expand(state, context, id)?;

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

/// Replaces forall binders with rigid (skolem) variables.
pub fn skolemise_forall<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    safe_loop! {
        id = normalise::expand(state, context, id)?;

        let Type::Forall(binder_id, inner) = context.lookup_type(id) else {
            break;
        };

        let binder = context.lookup_forall_binder(binder_id);
        let binder_kind = normalise::normalise(state, context, binder.kind)?;

        let rigid = state.fresh_rigid(context.queries, binder_kind);
        id = SubstituteName::one(state, context, binder.name, rigid, inner)?;
    }

    Ok(id)
}

/// Peels constraint layers, pushing each as a wanted.
pub fn collect_wanteds<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    safe_loop! {
        id = normalise::expand(state, context, id)?;
        match context.lookup_type(id) {
            Type::Constrained(constraint, constrained) => {
                state.push_wanted(constraint);
                id = constrained;
            }
            _ => return Ok(id),
        }
    }
}

/// Peels constraint layers, pushing each as a given.
pub fn collect_givens<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    safe_loop! {
        id = normalise::expand(state, context, id)?;
        match context.lookup_type(id) {
            Type::Constrained(constraint, constrained) => {
                state.push_given(constraint);
                id = constrained;
            }
            _ => return Ok(id),
        }
    }
}

/// Peels constraint layers without introducing givens or wanteds.
pub fn without_constraints<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    safe_loop! {
        id = normalise::expand(state, context, id)?;
        match context.lookup_type(id) {
            Type::Constrained(_, constrained) => {
                id = constrained;
            }
            _ => return Ok(id),
        }
    }
}

/// Instantiates forall binders and collects wanted constraints.
pub fn instantiate_constrained<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let id = instantiate_unifications(state, context, id)?;
    collect_wanteds(state, context, id)
}

pub fn decompose_function<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    t: TypeId,
) -> QueryResult<Option<(TypeId, TypeId)>>
where
    Q: ExternalQueries,
{
    let t = normalise::expand(state, context, t)?;

    match context.lookup_type(t) {
        Type::Function(argument, result) => Ok(Some((argument, result))),

        Type::Unification(unification_id) => {
            let argument = state.fresh_unification(context.queries, context.prim.t);
            let result = state.fresh_unification(context.queries, context.prim.t);

            let function = context.intern_function(argument, result);
            unification::solve(state, context, t, unification_id, function)?;

            Ok(Some((argument, result)))
        }

        Type::Application(partial, result) => {
            let partial = normalise::expand(state, context, partial)?;
            if let Type::Application(constructor, argument) = context.lookup_type(partial) {
                let constructor = normalise::expand(state, context, constructor)?;
                if constructor == context.prim.function {
                    return Ok(Some((argument, result)));
                }
                if let Type::Unification(unification_id) = context.lookup_type(constructor) {
                    unification::solve(
                        state,
                        context,
                        constructor,
                        unification_id,
                        context.prim.function,
                    )?;
                    return Ok(Some((argument, result)));
                }
            }
            Ok(None)
        }

        _ => Ok(None),
    }
}

pub fn lookup_file_roles<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<Option<Arc<[Role]>>>
where
    Q: ExternalQueries,
{
    if file_id == context.id {
        Ok(state.checked.lookup_roles(item_id))
    } else {
        let checked = context.queries.checked(file_id)?;
        Ok(checked.lookup_roles(item_id))
    }
}

pub fn is_newtype<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let type_item = if file_id == context.id {
        context.lowered.info.get_type_item(item_id)
    } else {
        let lowered = context.queries.lowered(file_id)?;
        return Ok(matches!(
            lowered.info.get_type_item(item_id),
            Some(lowering::TypeItemIr::NewtypeGroup { .. })
        ));
    };
    Ok(matches!(type_item, Some(lowering::TypeItemIr::NewtypeGroup { .. })))
}

pub fn extract_type_constructor<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    mut id: TypeId,
) -> QueryResult<Option<(FileId, TypeItemId)>>
where
    Q: ExternalQueries,
{
    safe_loop! {
        id = normalise::expand(state, context, id)?;
        match context.lookup_type(id) {
            Type::Constructor(file_id, item_id) => return Ok(Some((file_id, item_id))),
            Type::Application(function, _) | Type::KindApplication(function, _) => {
                id = function;
            }
            _ => return Ok(None),
        }
    }
}

pub fn is_constructor_in_scope<Q>(
    context: &CheckContext<Q>,
    file_id: FileId,
    item_id: TypeItemId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let constructor_term_id = if file_id == context.id {
        context.indexed.pairs.data_constructors(item_id).next()
    } else {
        let indexed = context.queries.indexed(file_id)?;
        indexed.pairs.data_constructors(item_id).next()
    };

    let Some(constructor_term_id) = constructor_term_id else {
        return Ok(false);
    };

    Ok(context.resolved.is_term_in_scope(&context.prim_resolved, file_id, constructor_term_id))
}

pub fn get_newtype_inner<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    newtype_file: FileId,
    newtype_id: TypeItemId,
    newtype_type: TypeId,
) -> QueryResult<Option<NewtypeInner>>
where
    Q: ExternalQueries,
{
    if !is_newtype(context, newtype_file, newtype_id)? {
        return Ok(None);
    }

    let constructor_term_id = if newtype_file == context.id {
        context.indexed.pairs.data_constructors(newtype_id).next()
    } else {
        let indexed = context.queries.indexed(newtype_file)?;
        indexed.pairs.data_constructors(newtype_id).next()
    };

    let Some(constructor_term_id) = constructor_term_id else {
        return Ok(None);
    };

    let constructor_type = lookup_file_term(state, context, newtype_file, constructor_term_id)?;

    let (_, arguments) = extract_all_applications(state, context, newtype_type)?;

    let mut current = constructor_type;
    let mut arguments = arguments.iter().copied();
    let mut rigids = vec![];

    safe_loop! {
        current = normalise::expand(state, context, current)?;
        let Type::Forall(binder_id, inner) = context.lookup_type(current) else {
            break;
        };

        let binder = context.lookup_forall_binder(binder_id);
        let replacement = arguments
            .next()
            .map(|argument| match argument {
                KindOrType::Kind(argument) | KindOrType::Type(argument) => argument,
            })
            .unwrap_or_else(|| {
                let rigid = state.fresh_rigid(context.queries, binder.kind);
                rigids.push(rigid);
                rigid
            });

        current = SubstituteName::one(state, context, binder.name, replacement, inner)?;
    }

    current = normalise::normalise(state, context, current)?;

    let InspectFunction { arguments, .. } = inspect_function(state, context, current)?;
    let [inner] = arguments[..] else { return Ok(None) };

    Ok(Some(NewtypeInner { inner, rigids }))
}
