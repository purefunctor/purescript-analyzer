use std::mem;
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TypeItemId};
use itertools::Itertools;
use lowering::{
    ClassIr, DataIr, LoweringError, NewtypeIr, RecursiveGroup, Scc, SynonymIr, TermItemIr,
    TypeItemIr, TypeVariableBinding,
};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{
    CheckedClass, CheckedSynonym, ForallBinder, Role, Type, TypeId, generalise, signature, toolkit,
    unification, zonk,
};
use crate::error::ErrorCrumb;
use crate::source::types;
use crate::state::CheckState;

struct PendingDataType {
    parameters: Vec<ForallBinder>,
    constructors: Vec<(TermItemId, Vec<TypeId>)>,
    declared_roles: Arc<[lowering::Role]>,
}

struct PendingSynonymType {
    parameters: Vec<ForallBinder>,
    synonym: TypeId,
}

struct PendingClassType {
    parameters: Vec<ForallBinder>,
    superclasses: Vec<TypeId>,
    functional_dependencies: Arc<[lowering::FunctionalDependency]>,
    members: Vec<(TermItemId, TypeId)>,
}

#[derive(Default)]
struct TypeSccState {
    data: Vec<(TypeItemId, PendingDataType)>,
    synonym: Vec<(TypeItemId, PendingSynonymType)>,
    class: Vec<(TypeItemId, PendingClassType)>,
    foreign: Vec<(TypeItemId, Arc<[lowering::Role]>)>,
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

        finalise_type_binding_group(state, context, &items)?;
        finalise_roles(state, context, &mut scc_state)?;
        finalise_data_constructors(state, context, &mut scc_state)?;
        finalise_synonym_replacements(state, context, &mut scc_state)?;
        finalise_classes(state, context, &mut scc_state)?;
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

fn finalise_type_binding_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    items: &[TypeItemId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let mut pending = Vec::with_capacity(items.len());

    for &item_id in items {
        let Some(kind) = state.checked.types.get(&item_id).copied() else {
            continue;
        };

        let kind = zonk::zonk(state, context, kind)?;
        let unsolved = generalise::unsolved_unifications(state, context, kind)?;

        pending.push((item_id, kind, unsolved));
    }

    for (item_id, kind, unsolved) in pending {
        let kind = generalise::generalise_unsolved(state, context, kind, &unsolved)?;
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
            check_signature_kind(state, context, item_id, *signature)?;
        }
        TypeItemIr::NewtypeGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_kind(state, context, item_id, *signature)?;
        }
        TypeItemIr::SynonymGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_kind(state, context, item_id, *signature)?;
        }
        TypeItemIr::ClassGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_kind(state, context, item_id, *signature)?;
        }
        TypeItemIr::Foreign { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_kind(state, context, item_id, *signature)?;
        }
        TypeItemIr::Operator { .. } => {}
    }

    Ok(())
}

fn check_signature_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let (checked_kind, _) = types::check_kind(state, context, signature, context.prim.t)?;
    state.checked.types.insert(item_id, checked_kind);
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
        TypeItemIr::DataGroup { signature, data, roles } => {
            let Some(DataIr { variables }) = data else { return Ok(()) };
            check_data_equation(state, context, scc, item_id, *signature, variables, roles)?;
        }
        TypeItemIr::NewtypeGroup { signature, newtype, roles } => {
            let Some(NewtypeIr { variables }) = newtype else { return Ok(()) };
            check_data_equation(state, context, scc, item_id, *signature, variables, roles)?;
        }
        TypeItemIr::SynonymGroup { signature, synonym, .. } => {
            let Some(SynonymIr { variables, synonym }) = synonym else { return Ok(()) };
            check_synonym_equation(state, context, scc, item_id, *signature, variables, *synonym)?;
        }
        TypeItemIr::ClassGroup { signature, class } => {
            let Some(class) = class else { return Ok(()) };
            check_class_equation(state, context, scc, item_id, *signature, class)?;
        }
        TypeItemIr::Foreign { roles, .. } => {
            scc.foreign.push((item_id, Arc::clone(roles)));
        }
        TypeItemIr::Operator { resolution, .. } => {
            check_type_operator(state, context, item_id, *resolution)?;
        }
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
    declared_roles: &Arc<[lowering::Role]>,
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
    let declared_roles = Arc::clone(declared_roles);

    scc.data.push((item_id, PendingDataType { parameters, constructors, declared_roles }));

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
    let signature = signature::expect_signature_bindings(state, context, signature, bindings)?;
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
        state.bindings.bind_forall(equation_binding.id, name, kind);

        let text = if let Some(name) = &equation_binding.name {
            SmolStr::clone(name)
        } else {
            name.as_text()
        };

        let text = context.queries.intern_smol_str(text);
        state.checked.names.insert(name, text);
        let visible = equation_binding.visible;

        binders.push(ForallBinder { visible, name, kind });
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
    let inferred = context.intern_function_chain_iter(kinds, context.prim.t);

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
    for (item_id, PendingDataType { parameters, constructors, .. }) in mem::take(&mut scc.data) {
        // constructor_kind should have already been generalised by the
        // finalise_binding_group function. the kind signature is used
        // as the source of truth for constructing the kind applications
        let Some(constructor_kind) = state.checked.types.get(&item_id).copied() else {
            continue;
        };

        let signature::DecomposedSignature {
            binders: kind_binders,
            arguments: parameter_kinds,
            ..
        } = signature::decompose_signature(
            state,
            context,
            constructor_kind,
            signature::DecomposeSignatureMode::Full,
        )?;

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

                let binder = ForallBinder { kind, ..*parameter };

                let binder_id = context.intern_forall_binder(binder);
                result = context.intern_forall(binder_id, result);
            }

            // forall (k :: Type) (t :: k) (a :: Type). a -> Tagged @k t a
            for binder in kind_binders.iter().rev() {
                let binder_id = context.intern_forall_binder(*binder);
                result = context.intern_forall(binder_id, result);
            }

            state.checked.terms.insert(constructor_id, result);
        }
    }

    Ok(())
}

fn finalise_roles<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for (item_id, pending) in &scc.data {
        let PendingDataType { parameters, constructors, declared_roles } = pending;
        let inferred_roles =
            super::roles::infer_data_roles(state, context, parameters, constructors)?;
        let resolved_roles = super::roles::check_declared_roles(
            state,
            *item_id,
            &inferred_roles,
            declared_roles,
            false,
        );
        state.checked.roles.insert(*item_id, resolved_roles);
    }

    for (item_id, declared_roles) in mem::take(&mut scc.foreign) {
        let Some(kind) = state.checked.lookup_type(item_id) else {
            continue;
        };

        let parameter_count = super::roles::count_kind_arguments(state, context, kind)?;
        let inferred_roles = vec![Role::Nominal; parameter_count];
        let resolved_roles = super::roles::check_declared_roles(
            state,
            item_id,
            &inferred_roles,
            &declared_roles,
            true,
        );

        state.checked.roles.insert(item_id, resolved_roles);
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

    let synonym = if let Some(synonym) = synonym {
        let (synonym, _) = types::check_kind(state, context, synonym, result)?;
        synonym
    } else {
        context.unknown("invalid synonym type")
    };

    scc.synonym.push((item_id, PendingSynonymType { parameters, synonym }));

    Ok(())
}

fn check_synonym_equation_check<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &[TypeVariableBinding],
    (signature_id, signature_kind): (lowering::TypeId, TypeId),
) -> QueryResult<(Vec<ForallBinder>, TypeId)>
where
    Q: ExternalQueries,
{
    let signature = signature::expect_signature_bindings(
        state,
        context,
        (signature_id, signature_kind),
        bindings,
    )?;
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
    let inferred = context.intern_function_chain_iter(kinds, result);

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
    for (item_id, PendingSynonymType { parameters, synonym }) in mem::take(&mut scc.synonym) {
        let Some(kind) = state.checked.lookup_type(item_id) else {
            continue;
        };
        let synonym = zonk::zonk(state, context, synonym)?;
        let synonym = CheckedSynonym { kind, parameters, synonym };
        state.checked.synonyms.insert(item_id, synonym);
    }
    Ok(())
}

fn check_class_equation<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    class: &ClassIr,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let ClassIr { constraints, variables, functional_dependencies } = class;

    let parameters = if let Some(signature_id) = signature
        && let Some(signature_kind) = state.checked.lookup_type(item_id)
    {
        check_class_equation_check(state, context, variables, (signature_id, signature_kind))?
    } else {
        check_class_equation_infer(state, context, item_id, variables)?
    };

    let mut superclasses = vec![];
    for &constraint in constraints.iter() {
        let (superclass, _) =
            types::check_kind(state, context, constraint, context.prim.constraint)?;
        superclasses.push(superclass);
    }

    let functional_dependencies = Arc::clone(functional_dependencies);
    let members = check_class_members(state, context, item_id)?;

    scc.class.push((
        item_id,
        PendingClassType { parameters, superclasses, functional_dependencies, members },
    ));

    Ok(())
}

fn check_class_equation_check<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &[TypeVariableBinding],
    signature: (lowering::TypeId, TypeId),
) -> QueryResult<Vec<ForallBinder>>
where
    Q: ExternalQueries,
{
    let signature = signature::expect_signature_bindings(state, context, signature, bindings)?;
    check_type_variable_bindings(state, context, bindings, &signature.arguments)
}

fn check_class_equation_infer<Q>(
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
    let inferred = context.intern_function_chain_iter(kinds, context.prim.constraint);

    if let Some(expected) = state.checked.lookup_type(item_id) {
        unification::subtype(state, context, inferred, expected)?;
    } else {
        state.checked.types.insert(item_id, inferred);
    }

    Ok(bindings)
}

fn check_class_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<Vec<(TermItemId, TypeId)>>
where
    Q: ExternalQueries,
{
    let mut members = vec![];

    for member_id in context.indexed.pairs.class_members(item_id) {
        let Some(TermItemIr::ClassMember { signature }) =
            context.lowered.info.get_term_item(member_id)
        else {
            continue;
        };

        let Some(signature_id) = signature else { continue };

        let (member_type, _) = types::check_kind(state, context, *signature_id, context.prim.t)?;
        members.push((member_id, member_type));
    }

    Ok(members)
}

fn finalise_classes<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for (item_id, pending) in mem::take(&mut scc.class) {
        let PendingClassType { parameters, superclasses, functional_dependencies, members } =
            pending;

        let Some(class_kind) = state.checked.types.get(&item_id).copied() else {
            continue;
        };

        let signature::DecomposedSignature {
            binders: class_binders,
            arguments: class_parameters,
            ..
        } = signature::decompose_signature(
            state,
            context,
            class_kind,
            signature::DecomposeSignatureMode::Full,
        )?;

        let get_parameter_kind = |index: usize| {
            if let Some(kind) = class_parameters.get(index) {
                *kind
            } else {
                context.unknown("invalid kind")
            }
        };

        let kind_binders = class_binders
            .iter()
            .copied()
            .map(|binder| context.intern_forall_binder(binder))
            .collect_vec();

        let type_parameters = parameters
            .iter()
            .copied()
            .enumerate()
            .map(|(index, parameter)| {
                let kind = get_parameter_kind(index);
                let binder = ForallBinder { kind, ..parameter };
                context.intern_forall_binder(binder)
            })
            .collect_vec();

        let mut class_head = context.queries.intern_type(Type::Constructor(context.id, item_id));

        for binder in &class_binders {
            let rigid = context.intern_rigid(binder.name, state.depth, binder.kind);
            class_head = context.intern_kind_application(class_head, rigid);
        }

        for (index, parameter) in parameters.iter().enumerate() {
            let kind = get_parameter_kind(index);
            let rigid = context.intern_rigid(parameter.name, state.depth, kind);
            class_head = context.intern_application(class_head, rigid);
        }

        let mut canonical = class_head;
        for type_parameter in type_parameters.iter().rev() {
            canonical = context.intern_forall(*type_parameter, canonical);
        }
        for kind_binder in kind_binders.iter().rev() {
            canonical = context.intern_forall(*kind_binder, canonical);
        }

        let superclasses = superclasses
            .into_iter()
            .map(|superclass| zonk::zonk(state, context, superclass))
            .collect::<QueryResult<Vec<_>>>()?;

        for (member_id, member_type) in members.iter() {
            let member_type = zonk::zonk(state, context, *member_type)?;

            let signature::DecomposedSignature {
                binders: member_binders,
                constraints: member_constraints,
                arguments: member_arguments,
                result: member_result,
            } = signature::decompose_signature(
                state,
                context,
                member_type,
                signature::DecomposeSignatureMode::Full,
            )?;

            let mut result = context.intern_function_chain(&member_arguments, member_result);

            for constraint in member_constraints.into_iter().rev() {
                result = context.intern_constrained(constraint, result);
            }

            result = context.intern_constrained(class_head, result);

            for member_binder in member_binders.iter().copied().rev() {
                let binder_id = context.intern_forall_binder(member_binder);
                result = context.intern_forall(binder_id, result);
            }

            for type_parameter in type_parameters.iter().rev() {
                result = context.intern_forall(*type_parameter, result);
            }

            for kind_binder in kind_binders.iter().rev() {
                result = context.intern_forall(*kind_binder, result);
            }

            state.checked.terms.insert(*member_id, result);
        }

        let members = members.into_iter().map(|(item_id, _)| item_id).collect_vec();

        state.checked.classes.insert(
            item_id,
            CheckedClass {
                kind_binders,
                type_parameters,
                canonical,
                superclasses,
                functional_dependencies,
                members,
            },
        );
    }

    Ok(())
}

fn check_type_operator<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    resolution: Option<(FileId, TypeItemId)>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some((file_id, type_id)) = resolution else { return Ok(()) };
    let operator_kind = toolkit::lookup_file_type_operator(state, context, file_id, type_id)?;

    if let Some(item_kind) = state.checked.lookup_type(item_id) {
        unification::subtype(state, context, operator_kind, item_kind)?;
    } else {
        state.checked.types.insert(item_id, operator_kind);
    }

    Ok(())
}
