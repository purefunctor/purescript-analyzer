//! Implements syntax-driven checking rules for source files.

pub mod signature;
pub mod synonym;
pub mod terms;
pub mod types;

use building_types::QueryResult;
use indexing::TypeItemId;
use itertools::Itertools;
use lowering::{
    DataIr, LoweringError, NewtypeIr, RecursiveGroup, Scc, TermItemIr, TypeItemIr,
    TypeVariableBinding,
};

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{TypeId, generalise, unification, zonk};
use crate::error::ErrorCrumb;
use crate::state::CheckState;

/// Checks all type items in topological order.
///
/// The order is determined by [`GroupedModule::type_scc`] in [`lowering`].
/// The algorithm accounts for items that appear in [`RecursiveKinds`] by
/// filtering and populating these items with [`Type::Unknown`]. This
/// enables checking of other binding groups, which may be unaffected.
///
/// [`GroupedModule::type_scc`]: lowering::GroupedModule::type_scc
/// [`RecursiveKinds`]: LoweringError::RecursiveKinds
/// [`Type::Unknown`]: crate::core::Type::Unknown
pub fn check_type_items<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.type_scc {
        let (items, skipped) = partition_type_items(context, scc);
        populate_skipped_items(state, context, &skipped);

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

        let kind = zonk::zonk(state, context, kind)?;
        let kind = generalise::generalise(state, context, kind)?;
        state.checked.types.insert(item_id, kind);
    }

    Ok(())
}

fn partition_type_items<Q>(
    context: &CheckContext<Q>,
    scc: &Scc<TypeItemId>,
) -> (Vec<TypeItemId>, Vec<TypeItemId>)
where
    Q: ExternalQueries,
{
    let mut checked = vec![];
    let mut skipped = vec![];

    for &item_id in scc.as_slice() {
        if is_recursive_kind(context, item_id) {
            skipped.push(item_id);
        } else {
            checked.push(item_id);
        }
    }

    (checked, skipped)
}

fn is_recursive_kind<Q>(context: &CheckContext<Q>, item_id: TypeItemId) -> bool
where
    Q: ExternalQueries,
{
    context.grouped.cycle_errors.iter().any(|error| {
        let LoweringError::RecursiveKinds(RecursiveGroup { group }) = error else {
            return false;
        };
        group.contains(&item_id)
    })
}

fn populate_skipped_items<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    items: &[TypeItemId],
) where
    Q: ExternalQueries,
{
    let unknown = context.unknown("invalid recursive type");
    let skipped = items.iter().map(|item| (*item, unknown));
    state.checked.types.extend(skipped);
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
        TypeItemIr::DataGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_data_signature(state, context, item_id, *signature)?;
        }
        TypeItemIr::NewtypeGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_data_signature(state, context, item_id, *signature)?;
        }
        TypeItemIr::SynonymGroup { .. } => todo!(),
        TypeItemIr::ClassGroup { .. } => todo!(),
        TypeItemIr::Foreign { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_foreign_signature(state, context, item_id, *signature)?;
        }
        TypeItemIr::Operator { .. } => todo!(),
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
        TypeItemIr::DataGroup { signature, data, .. } => {
            let Some(DataIr { variables }) = data else { return Ok(()) };
            check_data_equation(state, context, item_id, *signature, &variables)?;
        }
        TypeItemIr::NewtypeGroup { signature, newtype, .. } => {
            let Some(NewtypeIr { variables }) = newtype else { return Ok(()) };
            check_data_equation(state, context, item_id, *signature, &variables)?;
        }
        TypeItemIr::SynonymGroup { .. } => todo!(),
        TypeItemIr::ClassGroup { .. } => todo!(),
        TypeItemIr::Foreign { .. } => {}
        TypeItemIr::Operator { .. } => todo!(),
    }

    Ok(())
}

fn check_data_equation<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    if let Some(signature_id) = signature
        && let Some(signature_kind) = state.checked.lookup_type(item_id)
    {
        check_data_equation_check(state, context, (signature_id, signature_kind), variables)?;
    } else {
        check_data_equation_infer(state, context, item_id, variables)?;
    }

    check_data_constructors(state, context, item_id)?;

    Ok(())
}

fn check_data_equation_check<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: (lowering::TypeId, TypeId),
    bindings: &[TypeVariableBinding],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let signature = signature::inspect_signature(state, context, signature, &bindings)?;
    let _ = check_type_variable_bindings(state, context, bindings, &signature.arguments)?;
    Ok(())
}

fn check_type_variable_bindings<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &[TypeVariableBinding],
    signature: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let mut binding_kinds = Vec::with_capacity(bindings.len());

    for (index, equation_binding) in bindings.iter().enumerate() {
        let signature_kind = signature.get(index).copied();

        let resolved_kind =
            resolve_type_variable_binding(state, context, signature_kind, equation_binding)?;

        let name = state.names.fresh();
        state.kind_scope.bind_forall(equation_binding.id, name, resolved_kind);

        binding_kinds.push(resolved_kind);
    }

    Ok(binding_kinds)
}

fn resolve_type_variable_binding<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: Option<TypeId>,
    binding: &TypeVariableBinding,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    match (signature, binding.kind) {
        (Some(signature_kind), Some(binding_kind)) => {
            let (binding_kind, _) = types::infer_kind(state, context, binding_kind)?;
            let valid = unification::subtype(state, context, signature_kind, binding_kind)?;
            if valid { Ok(binding_kind) } else { Ok(context.unknown("invalid variable kind")) }
        }
        (Some(signature_kind), None) => {
            // Pure checking
            Ok(signature_kind)
        }
        (None, Some(binding_kind)) => {
            let (binding_kind, _) =
                types::check_kind(state, context, binding_kind, context.prim.t)?;
            Ok(binding_kind)
        }
        (None, None) => {
            // Pure inference
            Ok(state.fresh_unification(context.queries, context.prim.t))
        }
    }
}

fn check_data_equation_infer<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    bindings: &[TypeVariableBinding],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let variable_kinds = check_type_variable_bindings(state, context, bindings, &[])?;
    let inferred_kind = context.intern_function_chain(&variable_kinds, context.prim.t);

    if let Some(known_kind) = state.checked.lookup_type(item_id) {
        unification::subtype(state, context, inferred_kind, known_kind)?;
    } else {
        state.checked.types.insert(item_id, inferred_kind);
    }

    Ok(())
}

fn check_data_constructors<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for constructor_id in context.indexed.pairs.data_constructors(item_id) {
        let Some(TermItemIr::Constructor { arguments }) =
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
