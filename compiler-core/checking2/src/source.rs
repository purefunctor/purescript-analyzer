//! Implements syntax-driven checking rules for source files.

pub mod synonym;
pub mod terms;
pub mod types;

use building_types::QueryResult;
use indexing::TypeItemId;
use lowering::{DataIr, NewtypeIr};

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, generalise, toolkit, unification, zonk};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::state::CheckState;

/// Checks all type items in topological order.
pub fn check_type_items<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.type_scc {
        let items = filter_type_items(context, scc);

        for &item in &items {
            check_type_signature(state, context, item)?;
        }

        if scc.is_recursive() {
            prepare_binding_group(state, context, &items);
        }

        for &item in &items {
            check_type_equation(state, context, item)?;
        }

        finalise_binding_group(state, context, &items)?;
    }
    Ok(())
}

fn prepare_binding_group<Q>(state: &mut CheckState, context: &CheckContext<Q>, items: &[TypeItemId])
where
    Q: ExternalQueries,
{
    for &item_id in items {
        if state.checked.types.contains_key(&item_id) {
            continue;
        }
        let kind = state.fresh_unification(context.queries, context.prim.t);
        state.checked.types.insert(item_id, kind);
    }
}

fn finalise_binding_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    items: &[TypeItemId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for &item_id in items {
        let Some(kind) = state.checked.types.get(&item_id).copied() else {
            continue;
        };

        let kind = zonk::Zonk::on(state, context, kind)?;
        let kind = generalise::generalise(state, context, kind)?;
        state.checked.types.insert(item_id, kind);
    }

    Ok(())
}

fn filter_type_items<Q>(
    context: &CheckContext<Q>,
    scc: &lowering::Scc<TypeItemId>,
) -> Vec<TypeItemId>
where
    Q: ExternalQueries,
{
    scc.as_slice()
        .iter()
        .copied()
        .filter(|&item_id| {
            if has_recursive_kind_error(context, item_id) {
                return false;
            }

            // Recursive groups should only contain proper equations.
            !(scc.is_recursive() && is_foreign_item(context, item_id))
        })
        .collect()
}

fn has_recursive_kind_error<Q>(context: &CheckContext<Q>, item_id: TypeItemId) -> bool
where
    Q: ExternalQueries,
{
    context.grouped.cycle_errors.iter().any(|error| {
        if let lowering::LoweringError::RecursiveKinds(recursive) = error {
            recursive.group.contains(&item_id)
        } else {
            false
        }
    })
}

fn is_foreign_item<Q>(context: &CheckContext<Q>, item_id: TypeItemId) -> bool
where
    Q: ExternalQueries,
{
    matches!(
        context.lowered.info.get_type_item(item_id),
        Some(lowering::TypeItemIr::Foreign { .. })
    )
}

fn check_type_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(item) = context.lowered.info.get_type_item(item_id) else {
        return Ok(());
    };

    match item {
        lowering::TypeItemIr::DataGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_data_signature(state, context, item_id, *signature)?;
        }
        lowering::TypeItemIr::NewtypeGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_data_signature(state, context, item_id, *signature)?;
        }
        lowering::TypeItemIr::SynonymGroup { .. } => todo!(),
        lowering::TypeItemIr::ClassGroup { .. } => todo!(),
        lowering::TypeItemIr::Foreign { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_foreign_signature(state, context, item_id, *signature)?;
        }
        lowering::TypeItemIr::Operator { .. } => todo!(),
    }

    Ok(())
}

fn check_data_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let (inferred_type, _) = types::check_kind(state, context, signature, context.prim.t)?;
    state.checked.types.insert(item_id, inferred_type);
    Ok(())
}

fn check_foreign_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let (inferred_type, _) = types::check_kind(state, context, signature, context.prim.t)?;
    state.checked.types.insert(item_id, inferred_type);
    Ok(())
}

fn check_type_equation<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(item) = context.lowered.info.get_type_item(item_id) else {
        return Ok(());
    };

    match item {
        lowering::TypeItemIr::DataGroup { signature, data, .. } => {
            let Some(DataIr { variables }) = data else { return Ok(()) };
            check_data_equation(state, context, item_id, *signature, &variables)?;
        }
        lowering::TypeItemIr::NewtypeGroup { signature, newtype, .. } => {
            let Some(NewtypeIr { variables }) = newtype else { return Ok(()) };
            check_data_equation(state, context, item_id, *signature, &variables)?;
        }
        lowering::TypeItemIr::SynonymGroup { .. } => todo!(),
        lowering::TypeItemIr::ClassGroup { .. } => todo!(),
        lowering::TypeItemIr::Foreign { .. } => {}
        lowering::TypeItemIr::Operator { .. } => todo!(),
    }

    Ok(())
}

fn check_data_equation<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[lowering::TypeVariableBinding],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    if let Some(signature_id) = signature {
        let (matched_arguments, result) =
            check_data_equation_variables(state, context, signature_id, item_id, variables)?;
        let _ = build_data_equation_kind(state, context, variables, &matched_arguments, result)?;
    } else {
        let inferred_kind =
            build_data_equation_kind(state, context, variables, &[], context.prim.t)?;

        if let Some(known_kind) = state.checked.lookup_type(item_id) {
            unification::subtype(state, context, inferred_kind, known_kind)?;
        } else {
            state.checked.types.insert(item_id, inferred_kind);
        }
    }

    check_data_constructor_arguments(state, context, item_id)?;

    Ok(())
}

fn build_data_equation_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    variables: &[lowering::TypeVariableBinding],
    matched_arguments: &[TypeId],
    result: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let mut variable_kinds = Vec::with_capacity(variables.len());

    for (index, binding) in variables.iter().enumerate() {
        let expected_kind = matched_arguments.get(index).copied();

        let kind = if let Some(expected_kind) = expected_kind {
            if let Some(kind_id) = binding.kind {
                let (kind, _) = types::infer_kind(state, context, kind_id)?;
                let valid = unification::subtype(state, context, expected_kind, kind)?;
                if valid { kind } else { context.unknown("invalid data equation variable kind") }
            } else {
                expected_kind
            }
        } else {
            if let Some(kind_id) = binding.kind {
                let (kind, _) = types::check_kind(state, context, kind_id, context.prim.t)?;
                kind
            } else {
                state.fresh_unification(context.queries, context.prim.t)
            }
        };

        let name = state.names.fresh();
        state.kind_scope.bind_forall(binding.id, name, kind);
        variable_kinds.push(kind);
    }

    let item_kind = variable_kinds
        .iter()
        .rfold(result, |result, &argument| context.intern_function(argument, result));

    Ok(item_kind)
}

fn check_data_equation_variables<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_id: lowering::TypeId,
    item_id: TypeItemId,
    variables: &[lowering::TypeVariableBinding],
) -> QueryResult<(Vec<TypeId>, TypeId)>
where
    Q: ExternalQueries,
{
    let Some(stored_kind) = state.checked.lookup_type(item_id) else {
        return Ok((vec![], context.unknown("missing checked kind for type item")));
    };

    let (arguments, result) = toolkit::function_components(state, context, stored_kind)?;

    if variables.len() > arguments.len() {
        state.insert_error(ErrorKind::TypeSignatureVariableMismatch {
            id: signature_id,
            expected: arguments.len() as u32,
            actual: variables.len() as u32,
        });
    }

    let matched_count = variables.len().min(arguments.len());
    let matched_arguments = arguments.iter().take(matched_count).copied().collect::<Vec<_>>();

    let final_kind = arguments
        .iter()
        .skip(matched_count)
        .rfold(result, |result, &argument| context.intern_function(argument, result));

    Ok((matched_arguments, final_kind))
}

fn check_data_constructor_arguments<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for constructor_id in context.indexed.pairs.data_constructors(item_id) {
        let Some(lowering::TermItemIr::Constructor { arguments }) =
            context.lowered.info.get_term_item(constructor_id)
        else {
            continue;
        };

        for &argument in arguments.iter() {
            state.with_error_crumb(ErrorCrumb::ConstructorArgument(argument), |state| {
                let (_, _) = types::check_kind(state, context, argument, context.prim.t)?;
                Ok(())
            })?;
        }
    }

    Ok(())
}
