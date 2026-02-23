//! Implements syntax-driven checking rules for source files.

pub mod signature;
pub mod synonym;
pub mod terms;
pub mod types;

use std::mem;

use building_types::QueryResult;
use indexing::{TermItemId, TypeItemId};
use lowering::{
    DataIr, LoweringError, NewtypeIr, RecursiveGroup, Scc, SynonymIr, TermItemIr, TypeItemIr,
    TypeVariableBinding,
};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{
    CheckedSynonym, ForallBinder, Type, TypeId, generalise, toolkit, unification, zonk,
};
use crate::error::ErrorCrumb;
use crate::state::CheckState;

struct PendingDataType {
    parameters: Vec<ForallBinder>,
    constructors: Vec<(TermItemId, Vec<TypeId>)>,
}

struct PendingSynonymType {
    parameters: Vec<ForallBinder>,
    replacement: TypeId,
}

#[derive(Default)]
struct TypeSccState {
    data: Vec<(TypeItemId, PendingDataType)>,
    synonym: Vec<(TypeItemId, PendingSynonymType)>,
}

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

        let mut scc_state = TypeSccState::default();

        for &item in &items {
            check_type_equation(state, context, &mut scc_state, item)?;
        }

        finalise_binding_group(state, context, &items)?;
        finalise_data_constructors(state, context, &mut scc_state)?;
        finalise_synonym_replacements(state, context, &mut scc_state)?;
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
            check_signature_type(state, context, item_id, *signature)?;
        }
        TypeItemIr::NewtypeGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_type(state, context, item_id, *signature)?;
        }
        TypeItemIr::SynonymGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_type(state, context, item_id, *signature)?;
        }
        TypeItemIr::ClassGroup { .. } => todo!(),
        TypeItemIr::Foreign { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_type(state, context, item_id, *signature)?;
        }
        TypeItemIr::Operator { .. } => todo!(),
    }

    Ok(())
}

fn check_signature_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let (checked_type, _) = types::check_kind(state, context, signature, context.prim.t)?;
    state.checked.types.insert(item_id, checked_type);
    Ok(())
}

fn check_type_equation<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
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
            check_data_equation(state, context, scc, item_id, *signature, variables)?;
        }
        TypeItemIr::NewtypeGroup { signature, newtype, .. } => {
            let Some(NewtypeIr { variables }) = newtype else { return Ok(()) };
            check_data_equation(state, context, scc, item_id, *signature, variables)?;
        }
        TypeItemIr::SynonymGroup { signature, synonym, .. } => {
            let Some(SynonymIr { variables, synonym }) = synonym else { return Ok(()) };
            check_synonym_equation(state, context, scc, item_id, *signature, variables, *synonym)?;
        }
        TypeItemIr::ClassGroup { .. } => todo!(),
        TypeItemIr::Foreign { .. } => {}
        TypeItemIr::Operator { .. } => todo!(),
    }

    Ok(())
}

fn check_data_equation<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let parameters = if let Some(signature_id) = signature
        && let Some(signature_kind) = state.checked.lookup_type(item_id)
    {
        check_data_equation_check(state, context, (signature_id, signature_kind), variables)?
    } else {
        check_data_equation_infer(state, context, item_id, variables)?
    };

    let constructors = check_data_constructors(state, context, item_id)?;
    scc.data.push((item_id, PendingDataType { parameters, constructors }));

    Ok(())
}

fn check_data_equation_check<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature: (lowering::TypeId, TypeId),
    bindings: &[TypeVariableBinding],
) -> QueryResult<Vec<ForallBinder>>
where
    Q: ExternalQueries,
{
    let signature = signature::inspect_signature(state, context, signature, bindings)?;
    check_type_variable_bindings(state, context, bindings, &signature.arguments)
}

fn check_type_variable_bindings<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &[TypeVariableBinding],
    signature: &[TypeId],
) -> QueryResult<Vec<ForallBinder>>
where
    Q: ExternalQueries,
{
    let mut binders = vec![];

    for (index, equation_binding) in bindings.iter().enumerate() {
        let signature_kind = signature.get(index).copied();

        let kind = resolve_type_variable_binding(state, context, signature_kind, equation_binding)?;

        let name = state.names.fresh();
        state.kind_scope.bind_forall(equation_binding.id, name, kind);

        const MISSING: SmolStr = SmolStr::new_static("<MissingName>");
        let text = equation_binding.name.clone().unwrap_or(MISSING);
        let visible = equation_binding.visible;

        binders.push(ForallBinder { visible, name, text, kind });
    }

    Ok(binders)
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
) -> QueryResult<Vec<ForallBinder>>
where
    Q: ExternalQueries,
{
    let bindings = check_type_variable_bindings(state, context, bindings, &[])?;
    let kinds = bindings.iter().map(|binder| binder.kind);
    let inferred = context.intern_function_chain(kinds, context.prim.t);

    if let Some(expected) = state.checked.lookup_type(item_id) {
        unification::subtype(state, context, inferred, expected)?;
    } else {
        state.checked.types.insert(item_id, inferred);
    }

    Ok(bindings)
}

fn check_data_constructors<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<Vec<(TermItemId, Vec<TypeId>)>>
where
    Q: ExternalQueries,
{
    let mut constructors = vec![];

    for constructor_id in context.indexed.pairs.data_constructors(item_id) {
        let Some(TermItemIr::Constructor { arguments }) =
            context.lowered.info.get_term_item(constructor_id)
        else {
            continue;
        };

        let mut checked_arguments = vec![];
        for &argument in arguments.iter() {
            state.with_error_crumb(ErrorCrumb::ConstructorArgument(argument), |state| {
                let (checked_argument, _) =
                    types::check_kind(state, context, argument, context.prim.t)?;
                checked_arguments.push(checked_argument);
                Ok(())
            })?;
        }
        constructors.push((constructor_id, checked_arguments));
    }

    Ok(constructors)
}

fn finalise_data_constructors<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for (item_id, PendingDataType { parameters, constructors }) in mem::take(&mut scc.data) {
        // constructor_kind should have already been generalised by the
        // finalise_binding_group function. the kind signature is used
        // as the source of truth for constructing the kind applications
        let Some(constructor_kind) = state.checked.types.get(&item_id).copied() else {
            continue;
        };

        let toolkit::InspectQuantified { binders: kind_binders, quantified } =
            toolkit::inspect_quantified(state, context, constructor_kind)?;

        let toolkit::InspectFunction { arguments: parameter_kinds, .. } =
            toolkit::inspect_function(state, context, quantified)?;

        // parameter_kinds is the post-generalisation kind for each parameter;
        // we want to replace pre-generalisation kinds carried by parameters
        // before constructing the signature for the constructor.
        let get_parameter_kind = |index: usize| {
            if let Some(kind) = parameter_kinds.get(index) {
                *kind
            } else {
                context.unknown("invalid kind")
            }
        };

        let type_reference = context.queries.intern_type(Type::Constructor(context.id, item_id));

        // For the following code loop, let's trace through the declaration:
        //
        //   newtype Tagged :: forall k. k -> Type -> Type
        //
        for (constructor_id, checked_arguments) in constructors {
            let mut result = type_reference;

            // Tagged @k
            for binder in &kind_binders {
                let rigid = context.intern_rigid(binder.name, state.depth, binder.kind);
                result = context.intern_kind_application(result, rigid);
            }

            // Tagged @k t a
            for (index, parameter) in parameters.iter().enumerate() {
                let kind = get_parameter_kind(index);
                let rigid = context.intern_rigid(parameter.name, state.depth, kind);
                result = context.intern_application(result, rigid);
            }

            // a -> Tagged @k t a
            for argument in checked_arguments.into_iter().rev() {
                let argument = zonk::zonk(state, context, argument)?;
                result = context.intern_function(argument, result);
            }

            // forall (a :: Type). a -> Tagged @k t a
            for (index, parameter) in parameters.iter().enumerate().rev() {
                let kind = get_parameter_kind(index);

                let parameter = ForallBinder::clone(parameter);
                let binder = ForallBinder { kind, ..parameter };

                let binder_id = context.intern_forall_binder(binder);
                result = context.intern_forall(binder_id, result);
            }

            // forall (k :: Type) (t :: k) (a :: Type). a -> Tagged @k t a
            for binder in kind_binders.iter().rev() {
                let binder_id = context.intern_forall_binder(binder.clone());
                result = context.intern_forall(binder_id, result);
            }

            state.checked.terms.insert(constructor_id, result);
        }
    }

    Ok(())
}

fn check_synonym_equation<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    bindings: &[TypeVariableBinding],
    synonym: Option<lowering::TypeId>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let (parameters, result) = if let Some(signature_id) = signature
        && let Some(signature_kind) = state.checked.lookup_type(item_id)
    {
        check_synonym_equation_check(state, context, bindings, (signature_id, signature_kind))?
    } else {
        check_synonym_equation_infer(state, context, item_id, bindings)?
    };

    let replacement = if let Some(synonym) = synonym {
        let (synonym, _) = types::check_kind(state, context, synonym, result)?;
        synonym
    } else {
        context.unknown("invalid synonym type")
    };

    scc.synonym.push((item_id, PendingSynonymType { parameters, replacement }));

    Ok(())
}

fn check_synonym_equation_check<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &[TypeVariableBinding],
    signature: (lowering::TypeId, TypeId),
) -> QueryResult<(Vec<ForallBinder>, TypeId)>
where
    Q: ExternalQueries,
{
    let signature = signature::inspect_signature(state, context, signature, bindings)?;
    let parameters = check_type_variable_bindings(state, context, bindings, &signature.arguments)?;
    Ok((parameters, signature.result))
}

fn check_synonym_equation_infer<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    bindings: &[TypeVariableBinding],
) -> QueryResult<(Vec<ForallBinder>, TypeId)>
where
    Q: ExternalQueries,
{
    let bindings = check_type_variable_bindings(state, context, bindings, &[])?;
    let kinds = bindings.iter().map(|binder| binder.kind);
    let result = state.fresh_unification(context.queries, context.prim.t);
    let inferred = context.intern_function_chain(kinds, result);

    if let Some(expected) = state.checked.lookup_type(item_id) {
        unification::subtype(state, context, inferred, expected)?;
    } else {
        state.checked.types.insert(item_id, inferred);
    }

    Ok((bindings, result))
}

fn finalise_synonym_replacements<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for (item_id, PendingSynonymType { parameters, replacement }) in mem::take(&mut scc.synonym) {
        let replacement = zonk::zonk(state, context, replacement)?;
        let checked_synonym = CheckedSynonym { parameters, replacement };
        state.checked.synonyms.insert(item_id, checked_synonym);
    }
    Ok(())
}
