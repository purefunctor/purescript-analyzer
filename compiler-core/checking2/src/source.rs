//! Implements syntax-driven checking rules for source files.

pub mod operator;
pub mod roles;
pub mod signature;
pub mod synonym;
pub mod terms;
pub mod types;

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
    CheckedClass, CheckedSynonym, ForallBinder, Role, Type, TypeId, generalise, toolkit,
    unification, zonk,
};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::state::CheckState;

struct PendingDataType {
    parameters: Vec<ForallBinder>,
    constructors: Vec<(TermItemId, Vec<TypeId>)>,
    declared_roles: Arc<[lowering::Role]>,
}

struct PendingSynonymType {
    kind: TypeId,
    parameters: Vec<ForallBinder>,
    synonym: TypeId,
}

struct PendingClassType {
    parameters: Vec<ForallBinder>,
    superclasses: Vec<TypeId>,
    functional_dependencies: std::sync::Arc<[lowering::FunctionalDependency]>,
    members: Vec<(TermItemId, TypeId)>,
}

#[derive(Default)]
struct TypeSccState {
    data: Vec<(TypeItemId, PendingDataType)>,
    synonym: Vec<(TypeItemId, PendingSynonymType)>,
    class: Vec<(TypeItemId, PendingClassType)>,
    foreign: Vec<(TypeItemId, Arc<[lowering::Role]>)>,
    operator: Vec<TypeItemId>,
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
        finalise_type_operators(state, context, &mut scc_state)?;
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
            check_type_operator(state, context, scc, item_id, *resolution)?;
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

        let text = if let Some(name) = &equation_binding.name {
            SmolStr::clone(name)
        } else {
            name.as_text()
        };

        let text = context.queries.intern_smol_str(text);
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
    for (item_id, PendingDataType { parameters, constructors, .. }) in mem::take(&mut scc.data) {
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
        let inferred_roles = roles::infer_data_roles(state, context, parameters, constructors)?;
        let resolved_roles =
            roles::check_declared_roles(state, *item_id, &inferred_roles, declared_roles, false);
        state.checked.roles.insert(*item_id, resolved_roles);
    }

    for (item_id, declared_roles) in mem::take(&mut scc.foreign) {
        let Some(kind) = state.checked.lookup_type(item_id) else {
            continue;
        };

        let parameter_count = roles::count_kind_arguments(state, context, kind)?;
        let inferred_roles = vec![Role::Nominal; parameter_count];
        let resolved_roles =
            roles::check_declared_roles(state, item_id, &inferred_roles, &declared_roles, true);

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
    let (parameters, kind, result) = if let Some(signature_id) = signature
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

    scc.synonym.push((item_id, PendingSynonymType { kind, parameters, synonym }));

    Ok(())
}

fn check_synonym_equation_check<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &[TypeVariableBinding],
    (signature_id, signature_kind): (lowering::TypeId, TypeId),
) -> QueryResult<(Vec<ForallBinder>, TypeId, TypeId)>
where
    Q: ExternalQueries,
{
    let signature =
        signature::inspect_signature(state, context, (signature_id, signature_kind), bindings)?;
    let parameters = check_type_variable_bindings(state, context, bindings, &signature.arguments)?;
    Ok((parameters, signature_kind, signature.result))
}

fn check_synonym_equation_infer<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    bindings: &[TypeVariableBinding],
) -> QueryResult<(Vec<ForallBinder>, TypeId, TypeId)>
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

    Ok((bindings, inferred, result))
}

fn finalise_synonym_replacements<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for (item_id, PendingSynonymType { kind, parameters, synonym }) in mem::take(&mut scc.synonym) {
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
    let signature = signature::inspect_signature(state, context, signature, bindings)?;
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
    let inferred = context.intern_function_chain(kinds, context.prim.constraint);

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

        let toolkit::InspectQuantified { binders: class_binders, quantified: class_inner } =
            toolkit::inspect_quantified(state, context, class_kind)?;

        let toolkit::InspectFunction { arguments: class_parameters, .. } =
            toolkit::inspect_function(state, context, class_inner)?;

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

        let mut canonical = context.queries.intern_type(Type::Constructor(context.id, item_id));

        for binder in &class_binders {
            let rigid = context.intern_rigid(binder.name, state.depth, binder.kind);
            canonical = context.intern_kind_application(canonical, rigid);
        }

        for (index, parameter) in parameters.iter().enumerate() {
            let kind = get_parameter_kind(index);
            let rigid = context.intern_rigid(parameter.name, state.depth, kind);
            canonical = context.intern_application(canonical, rigid);
        }

        let superclasses = superclasses
            .into_iter()
            .map(|superclass| zonk::zonk(state, context, superclass))
            .collect::<QueryResult<Vec<_>>>()?;

        for (member_id, member_type) in members.iter() {
            let member_type = zonk::zonk(state, context, *member_type)?;

            let toolkit::InspectQuantified { binders: member_binders, quantified: member_inner } =
                toolkit::inspect_quantified(state, context, member_type)?;

            let mut result = context.intern_constrained(canonical, member_inner);

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
    scc: &mut TypeSccState,
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

    scc.operator.push(item_id);

    Ok(())
}

fn finalise_type_operators<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TypeSccState,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for item_id in mem::take(&mut scc.operator) {
        let Some(kind) = state.checked.types.get(&item_id).copied() else {
            continue;
        };

        if !is_binary_operator_type(state, context, kind)? {
            let kind_message = state.pretty_id(context, kind)?;
            state.insert_error(ErrorKind::InvalidTypeOperator { kind_message });
        }
    }

    Ok(())
}

fn is_binary_operator_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    kind: TypeId,
) -> QueryResult<bool>
where
    Q: ExternalQueries,
{
    let toolkit::InspectQuantified { quantified, .. } =
        toolkit::inspect_quantified(state, context, kind)?;

    let toolkit::InspectFunction { arguments, .. } =
        toolkit::inspect_function(state, context, quantified)?;

    Ok(arguments.len() == 2)
}

// ------------------------------ Term Items ------------------------------- //

#[derive(Default)]
struct TermSccState {
    operator: Vec<TermItemId>,
}

pub fn check_term_items<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.term_scc {
        let items = scc.as_slice();

        for &item in items {
            check_term_signature(state, context, item)?;
        }

        let mut term_scc = TermSccState::default();

        for &item in items {
            check_term_equation(state, context, &mut term_scc, item)?;
        }

        finalise_term_binding_group(state, context, items)?;
        finalise_term_operators(state, context, &mut term_scc)?;
    }

    Ok(())
}

fn check_term_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(item) = context.lowered.info.get_term_item(item_id) else {
        return Ok(());
    };

    match item {
        TermItemIr::Foreign { signature } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_type(state, context, item_id, *signature)?;
        }
        TermItemIr::ValueGroup { signature, .. } => {
            let Some(signature) = signature else { return Ok(()) };
            check_signature_type(state, context, item_id, *signature)?;
        }
        _ => (),
    }

    Ok(())
}

fn check_signature_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    signature: lowering::TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let (checked_kind, _) = types::check_kind(state, context, signature, context.prim.t)?;
    state.checked.terms.insert(item_id, checked_kind);
    Ok(())
}

fn check_term_equation<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TermSccState,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(item) = context.lowered.info.get_term_item(item_id) else {
        return Ok(());
    };

    match item {
        TermItemIr::Operator { resolution, .. } => {
            check_term_operator(state, context, scc, item_id, *resolution)?;
        }
        _ => (),
    }

    Ok(())
}

fn finalise_term_binding_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    items: &[TermItemId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for &item_id in items {
        let Some(kind) = state.checked.terms.get(&item_id).copied() else {
            continue;
        };

        let kind = zonk::zonk(state, context, kind)?;
        let kind = generalise::generalise(state, context, kind)?;
        state.checked.terms.insert(item_id, kind);
    }

    Ok(())
}

fn check_term_operator<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TermSccState,
    item_id: TermItemId,
    resolution: Option<(FileId, TermItemId)>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some((file_id, term_id)) = resolution else { return Ok(()) };
    let operator_type = toolkit::lookup_file_term_operator(state, context, file_id, term_id)?;

    if let Some(item_type) = state.checked.lookup_term(item_id) {
        unification::subtype(state, context, operator_type, item_type)?;
    } else {
        state.checked.terms.insert(item_id, operator_type);
    }

    scc.operator.push(item_id);

    Ok(())
}

fn finalise_term_operators<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TermSccState,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for item_id in mem::take(&mut scc.operator) {
        let Some(t) = state.checked.terms.get(&item_id).copied() else {
            continue;
        };
        if !is_binary_operator_type(state, context, t)? {
            let kind_message = state.pretty_id(context, t)?;
            state.insert_error(ErrorKind::InvalidTypeOperator { kind_message });
        }
    }

    Ok(())
}
