use std::sync::Arc;

use building_types::QueryResult;
use indexing::{TermItemId, TypeItemId};
use itertools::{Itertools, izip};
use lowering::{
    ClassIr, DataIr, NewtypeIr, SynonymIr, TermItemIr, TypeItemIr, TypeVariableBinding,
};
use smol_str::SmolStr;

use crate::ExternalQueries;
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState, CheckedConstructor};
use crate::algorithm::{inspect, kind, quantify, transfer, unification};
use crate::core::{
    Class, DataLike, ForallBinder, Operator, Role, Synonym, Type, TypeId, Variable, debruijn,
};
use crate::error::{ErrorKind, ErrorStep};

const MISSING_NAME: SmolStr = SmolStr::new_static("<MissingName>");

#[derive(Clone)]
pub struct CheckedSynonym {
    pub inferred_kind: Option<TypeId>,
    pub kind_variables: Vec<ForallBinder>,
    pub type_variables: Vec<ForallBinder>,
    pub synonym_type: TypeId,
}

#[derive(Clone)]
pub struct CheckedData {
    pub inferred_kind: Option<TypeId>,
    pub kind_variables: Vec<ForallBinder>,
    pub type_variables: Vec<ForallBinder>,
    pub constructors: Vec<CheckedConstructor>,
    pub declared_roles: Arc<[lowering::Role]>,
}

#[derive(Clone)]
pub struct CheckedClass {
    pub inferred_kind: Option<TypeId>,
    pub kind_variables: Vec<ForallBinder>,
    pub type_variables: Vec<ForallBinder>,
    pub superclasses: Arc<[(TypeId, TypeId)]>,
    pub members: Vec<(TermItemId, TypeId)>,
}

#[derive(Clone)]
pub struct CheckedOperator {
    pub kind: TypeId,
}

#[derive(Clone)]
pub enum CheckedTypeItem {
    Synonym(CheckedSynonym),
    Data(CheckedData),
    Class(CheckedClass),
    Operator(CheckedOperator),
}

/// Checks a type item definition, returning [`CheckedTypeItem`].
///
/// See the following functions to learn more:
/// - [`check_data_definition`]
/// - [`check_synonym_definition`]
/// - [`check_class_definition`]
pub fn check_type_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<Option<CheckedTypeItem>>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TypeDeclaration(item_id), |state| {
        let _span = tracing::debug_span!("check_type_item").entered();

        let Some(item) = context.lowered.info.get_type_item(item_id) else {
            return Ok(None);
        };

        match item {
            TypeItemIr::DataGroup { signature, data, roles } => {
                let Some(DataIr { variables }) = data else {
                    return Ok(None);
                };
                check_data_definition(state, context, item_id, *signature, variables, roles)
            }

            TypeItemIr::NewtypeGroup { signature, newtype, roles } => {
                let Some(NewtypeIr { variables }) = newtype else {
                    return Ok(None);
                };
                check_data_definition(state, context, item_id, *signature, variables, roles)
            }

            TypeItemIr::SynonymGroup { signature, synonym } => {
                let Some(SynonymIr { variables, synonym: Some(synonym) }) = synonym else {
                    return Ok(None);
                };
                check_synonym_definition(state, context, item_id, *signature, variables, *synonym)
            }

            TypeItemIr::ClassGroup { signature, class } => {
                let Some(class) = class else {
                    return Ok(None);
                };
                check_class_definition(state, context, item_id, *signature, class)
            }

            TypeItemIr::Foreign { roles, .. } => {
                check_foreign_definition(state, context, item_id, roles)
            }

            TypeItemIr::Operator { associativity, precedence, resolution } => {
                let Some(associativity) = *associativity else { return Ok(None) };
                let Some(precedence) = *precedence else { return Ok(None) };
                let Some((file_id, type_id)) = *resolution else { return Ok(None) };

                let operator = Operator { associativity, precedence, file_id, type_id };
                state.checked.operators.insert(item_id, operator);

                let kind = kind::lookup_file_type(state, context, file_id, type_id)?;
                unify_pending_kind(state, context, item_id, kind)?;

                Ok(Some(CheckedTypeItem::Operator(CheckedOperator { kind })))
            }
        }
    })
}

/// Checks a data/newtype definition, returning [`CheckedTypeItem::Data`].
fn check_data_definition<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    declared_roles: &Arc<[lowering::Role]>,
) -> QueryResult<Option<CheckedTypeItem>>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, item_id, signature, variables, |_| context.prim.t)?
    else {
        return Ok(None);
    };

    let mut inferred_kind = None;

    if signature.is_none() {
        let data_kind = type_variables.iter().rfold(result_kind, |result, variable| {
            state.storage.intern(Type::Function(variable.kind, result))
        });

        unify_pending_kind(state, context, item_id, data_kind)?;
        inferred_kind.replace(data_kind);
    }

    let constructors = check_constructor_arguments(state, context, item_id)?;

    let type_unbind_level = type_variables.first().map(|variable| variable.level);
    let kind_unbind_level = kind_variables.first().map(|variable| variable.level);

    if let Some(level) = type_unbind_level {
        state.type_scope.unbind(level);
    }
    if let Some(level) = kind_unbind_level {
        state.type_scope.unbind(level);
    }

    crate::debug_fields!(state, context, { inferred_kind = inferred_kind });

    Ok(Some(CheckedTypeItem::Data(CheckedData {
        inferred_kind,
        kind_variables,
        type_variables,
        constructors,
        declared_roles: declared_roles.clone(),
    })))
}

/// Checks a synonym body, returning [`CheckedTypeItem::Synonym`].
fn check_synonym_definition<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    synonym: lowering::TypeId,
) -> QueryResult<Option<CheckedTypeItem>>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, item_id, signature, variables, |state| {
            state.fresh_unification_type(context)
        })?
    else {
        return Ok(None);
    };

    let (synonym_type, _) = kind::check_surface_kind(state, context, synonym, result_kind)?;

    let mut inferred_kind = None;

    if signature.is_none() {
        let synonym_kind = type_variables.iter().rfold(result_kind, |result, binder| {
            state.storage.intern(Type::Function(binder.kind, result))
        });
        unify_pending_kind(state, context, item_id, synonym_kind)?;
        inferred_kind.replace(synonym_kind);
    }

    if let Some(variable) = type_variables.first() {
        state.type_scope.unbind(variable.level);
    }
    if let Some(variable) = kind_variables.first() {
        state.type_scope.unbind(variable.level);
    }

    crate::debug_fields!(state, context, { synonym_type = synonym_type });

    Ok(Some(CheckedTypeItem::Synonym(CheckedSynonym {
        inferred_kind,
        kind_variables,
        type_variables,
        synonym_type,
    })))
}

/// Checks a class body, returning [`CheckedTypeItem::Class`].
fn check_class_definition<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    ClassIr { constraints, variables, .. }: &ClassIr,
) -> QueryResult<Option<CheckedTypeItem>>
where
    Q: ExternalQueries,
{
    let Some(SignatureLike { kind_variables, type_variables, result_kind }) =
        check_signature_like(state, context, item_id, signature, variables, |_| {
            context.prim.constraint
        })?
    else {
        return Ok(None);
    };

    // Check superclasses
    let superclasses = constraints.iter().map(|&constraint| {
        let (constraint_type, constraint_kind) =
            kind::check_surface_kind(state, context, constraint, context.prim.constraint)?;
        Ok((constraint_type, constraint_kind))
    });
    let superclasses: Arc<[(TypeId, TypeId)]> = superclasses.collect::<QueryResult<_>>()?;

    // Build class reference for member type wrapping
    let class_reference = {
        let reference_type = state.storage.intern(Type::Constructor(context.id, item_id));
        type_variables.iter().cloned().fold(reference_type, |reference_type, binder| {
            let variable = Variable::Bound(binder.level, binder.kind);
            let variable = state.storage.intern(Type::Variable(variable));
            state.storage.intern(Type::Application(reference_type, variable))
        })
    };

    let mut inferred_kind = None;

    if signature.is_none() {
        let class_kind = type_variables.iter().rfold(result_kind, |result, variable| {
            state.storage.intern(Type::Function(variable.kind, result))
        });
        unify_pending_kind(state, context, item_id, class_kind)?;
        inferred_kind.replace(class_kind);
    }

    let members = check_class_members(
        state,
        context,
        item_id,
        &kind_variables,
        &type_variables,
        class_reference,
    )?;

    if let Some(variable) = type_variables.first() {
        state.type_scope.unbind(variable.level);
    }
    if let Some(variable) = kind_variables.first() {
        state.type_scope.unbind(variable.level);
    }

    crate::debug_fields!(state, context, { ?superclass_count = superclasses.len(), ?member_count = members.len() });

    Ok(Some(CheckedTypeItem::Class(CheckedClass {
        inferred_kind,
        kind_variables,
        type_variables,
        superclasses,
        members,
    })))
}

/// Checks class members inline, returning their types.
fn check_class_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    kind_variables: &[ForallBinder],
    type_variables: &[ForallBinder],
    class_reference: TypeId,
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

        let (member_type, _) =
            kind::check_surface_kind(state, context, *signature_id, context.prim.t)?;

        let (member_foralls, member_inner) = collect_foralls(state, member_type);

        let constrained_type =
            state.storage.intern(Type::Constrained(class_reference, member_inner));

        let all_variables = {
            let from_kind = kind_variables.iter().cloned();
            let from_type = type_variables.iter().cloned();
            let from_member = member_foralls.into_iter();
            from_kind.chain(from_type).chain(from_member)
        };

        let member_type = all_variables.rfold(constrained_type, |inner, variable| {
            state.storage.intern(Type::Forall(variable, inner))
        });

        members.push((member_id, member_type));
    }

    Ok(members)
}

/// Generalises the inferred types for a [`CheckedTypeItem`].
pub fn commit_type_item<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    item: CheckedTypeItem,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    match item {
        CheckedTypeItem::Synonym(synonym) => commit_synonym(state, context, item_id, synonym),
        CheckedTypeItem::Data(data) => commit_data(state, context, item_id, data),
        CheckedTypeItem::Class(class) => commit_class(state, context, item_id, class),
        CheckedTypeItem::Operator(operator) => commit_operator(state, context, item_id, operator),
    }
}

/// Generalises the inferred types for a [`CheckedSynonym`].
fn commit_synonym<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    checked: CheckedSynonym,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckedSynonym { inferred_kind, kind_variables, type_variables, synonym_type } = checked;

    if let Some(inferred_kind) = inferred_kind
        && let Some((quantified_type, _)) = quantify::quantify(state, inferred_kind)
    {
        let type_id = transfer::globalize(state, context, quantified_type);
        state.checked.types.insert(item_id, type_id);
    }

    let synonym_type = type_variables.iter().rfold(synonym_type, |inner, binder| {
        let binder = binder.clone();
        state.storage.intern(Type::Forall(binder, inner))
    });

    let synonym_type = kind_variables.iter().rfold(synonym_type, |inner, binder| {
        let binder = binder.clone();
        state.storage.intern(Type::Forall(binder, inner))
    });

    if let Some((quantified_synonym, quantified_variables)) =
        quantify::quantify(state, synonym_type)
    {
        let kind_var_count = kind_variables.len() as u32;
        let kind_variables = debruijn::Size(kind_var_count);

        let kind_var_count = type_variables.len() as u32;
        let type_variables = debruijn::Size(kind_var_count);

        let synonym_type = transfer::globalize(state, context, quantified_synonym);

        let synonym =
            Synonym { quantified_variables, kind_variables, type_variables, synonym_type };

        state.checked.synonyms.insert(item_id, synonym);
    }

    Ok(())
}

/// Generalises the inferred types for a [`CheckedData`].
fn commit_data<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    checked: CheckedData,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckedData { inferred_kind, kind_variables, type_variables, constructors, declared_roles } =
        checked;

    let kind_variable_count = kind_variables.len() as u32;

    let inferred_roles = infer_roles(state, &type_variables, &constructors);
    check_roles(state, item_id, &inferred_roles, &declared_roles, false);

    if let Some(inferred_kind) = inferred_kind {
        let Some((quantified_type, quantified_variables)) =
            quantify::quantify(state, inferred_kind)
        else {
            return Ok(());
        };

        let kind_variables = debruijn::Size(kind_variable_count);

        let data_like = DataLike { quantified_variables, kind_variables };
        state.checked.data.insert(item_id, data_like);

        let type_id = transfer::globalize(state, context, quantified_type);
        state.checked.types.insert(item_id, type_id);
    } else {
        let quantified_variables = debruijn::Size(0);
        let kind_variables = debruijn::Size(kind_variable_count);

        let data_like = DataLike { quantified_variables, kind_variables };
        state.checked.data.insert(item_id, data_like);
    };

    let data_reference =
        build_data_reference(state, context, item_id, &kind_variables, &type_variables);

    for constructor in constructors {
        let constructor_type =
            constructor.arguments.iter().rfold(data_reference, |result, &argument| {
                state.storage.intern(Type::Function(argument, result))
            });

        let all_variables = {
            let from_kind = kind_variables.iter();
            let from_type = type_variables.iter();
            from_kind.chain(from_type).cloned()
        };

        let constructor_type = all_variables.rfold(constructor_type, |inner, variable| {
            state.storage.intern(Type::Forall(variable, inner))
        });

        if let Some((quantified_constructor, _)) = quantify::quantify(state, constructor_type) {
            let constructor_type = transfer::globalize(state, context, quantified_constructor);
            state.checked.terms.insert(constructor.item_id, constructor_type);
        }
    }

    Ok(())
}

fn build_data_reference<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    kind_variables: &[ForallBinder],
    type_variables: &[ForallBinder],
) -> TypeId
where
    Q: ExternalQueries,
{
    let reference_type = state.storage.intern(Type::Constructor(context.id, item_id));

    let reference_type = kind_variables.iter().fold(reference_type, |reference, binder| {
        let variable = Variable::Bound(binder.level, binder.kind);
        let variable = state.storage.intern(Type::Variable(variable));
        state.storage.intern(Type::KindApplication(reference, variable))
    });

    let unsolved_kinds = type_variables.iter().filter_map(|binder| {
        let kind = state.normalize_type(binder.kind);
        if let Type::Unification(id) = state.storage[kind] { Some((kind, id)) } else { None }
    });

    let mut unsolved_kinds = unsolved_kinds.collect_vec();
    unsolved_kinds.sort_by_key(|&(_, id)| (state.unification.get(id).domain, id));

    let reference_type = unsolved_kinds.iter().fold(reference_type, |reference, &(kind, _)| {
        state.storage.intern(Type::KindApplication(reference, kind))
    });

    type_variables.iter().fold(reference_type, |reference, binder| {
        let variable = Variable::Bound(binder.level, binder.kind);
        let variable = state.storage.intern(Type::Variable(variable));
        state.storage.intern(Type::Application(reference, variable))
    })
}

/// Generalises the inferred types for a [`CheckedClass`].
fn commit_class<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    checked: CheckedClass,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckedClass { inferred_kind, kind_variables, type_variables, superclasses, members } =
        checked;

    let mut quantified_type = None;
    let mut quantified_variables = debruijn::Size(0);

    if let Some(inferred_kind) = inferred_kind
        && let Some((q_type, q_variables)) = quantify::quantify(state, inferred_kind)
    {
        quantified_type = Some(q_type);
        quantified_variables = q_variables;
    };

    let mut class = {
        let kind_var_count = kind_variables.len() as u32;
        let kind_variables = debruijn::Size(kind_var_count);
        let type_variable_kinds = type_variables.iter().map(|binder| binder.kind).collect();
        Class { superclasses, type_variable_kinds, quantified_variables, kind_variables }
    };

    let class_quantified_count =
        quantify::quantify_class(state, &mut class).unwrap_or(debruijn::Size(0));

    debug_assert_eq!(
        quantified_variables, class_quantified_count,
        "critical violation: class type signature and declaration should have the same number of variables"
    );

    class.quantified_variables = quantified_variables;

    let superclasses = class.superclasses.iter().map(|&(t, k)| {
        let t = transfer::globalize(state, context, t);
        let k = transfer::globalize(state, context, k);
        (t, k)
    });

    class.superclasses = superclasses.collect();

    let type_variable_kinds =
        class.type_variable_kinds.iter().map(|&kind| transfer::globalize(state, context, kind));

    class.type_variable_kinds = type_variable_kinds.collect();

    state.checked.classes.insert(item_id, class);

    if let Some(quantified_type) = quantified_type {
        let type_id = transfer::globalize(state, context, quantified_type);
        state.checked.types.insert(item_id, type_id);
    }

    for (member_id, member_type) in members {
        if let Some((quantified_member, _)) = quantify::quantify(state, member_type) {
            let member_type = transfer::globalize(state, context, quantified_member);
            state.checked.terms.insert(member_id, member_type);
        }
    }

    Ok(())
}

/// Commits an operator, checking validity and storing the kind.
fn commit_operator<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    checked: CheckedOperator,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckedOperator { kind } = checked;

    // Now that all items in the SCC are processed, the kind should be fully resolved
    if !is_binary_operator_type(state, kind) {
        let kind_message = state.render_local_type(context, kind);
        state.insert_error(ErrorKind::InvalidTypeOperator { kind_message });
    }

    // Generalize and store the kind
    if let Some((quantified_type, _)) = quantify::quantify(state, kind) {
        let type_id = transfer::globalize(state, context, quantified_type);
        state.checked.types.insert(item_id, type_id);
    }

    Ok(())
}

/// Checks the kind signature of a type item.
///
/// This function also generalises the type and inserts it directly to
/// [`CheckState::checked`], effectively making signatures the ground
/// truth for type definitions to check against.
///
/// To enable scoped type variables, this function also populates the
/// [`CheckState::surface_bindings`] with the kind variables found in
/// the signature.
pub fn check_type_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TypeDeclaration(item_id), |state| {
        let _span = tracing::debug_span!("check_type_signature").entered();

        let Some(item) = context.lowered.info.get_type_item(item_id) else {
            return Ok(());
        };

        match item {
            TypeItemIr::DataGroup { signature, .. }
            | TypeItemIr::NewtypeGroup { signature, .. }
            | TypeItemIr::SynonymGroup { signature, .. }
            | TypeItemIr::ClassGroup { signature, .. }
            | TypeItemIr::Foreign { signature, .. } => {
                let Some(signature) = signature else {
                    return Ok(());
                };

                let signature_variables = inspect::collect_signature_variables(context, *signature);
                state.surface_bindings.insert_type(item_id, signature_variables);

                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature, context.prim.t)?;

                if let Some((quantified_type, _)) = quantify::quantify(state, inferred_type) {
                    let type_id = transfer::globalize(state, context, quantified_type);
                    state.checked.types.insert(item_id, type_id);
                }
            }

            TypeItemIr::Operator { .. } => {}
        }

        Ok(())
    })
}

struct SignatureLike {
    kind_variables: Vec<ForallBinder>,
    type_variables: Vec<ForallBinder>,
    result_kind: TypeId,
}

fn check_signature_like<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    signature: Option<lowering::TypeId>,
    variables: &[TypeVariableBinding],
    infer_result: impl FnOnce(&mut CheckState) -> TypeId,
) -> QueryResult<Option<SignatureLike>>
where
    Q: ExternalQueries,
{
    let signature = if let Some(signature_id) = signature {
        let stored_kind = kind::lookup_file_type(state, context, context.id, item_id)?;

        let surface_bindings = state.surface_bindings.get_type(item_id);
        let surface_bindings = surface_bindings.as_deref().unwrap_or_default();

        let signature = inspect::inspect_signature(state, context, stored_kind, surface_bindings)?;

        if variables.len() != signature.arguments.len() {
            state.insert_error(ErrorKind::TypeSignatureVariableMismatch {
                id: signature_id,
                expected: 0,
                actual: 0,
            });

            if let Some(variable) = signature.variables.first() {
                state.type_scope.unbind(variable.level);
            }

            return Ok(None);
        };

        let variables = variables.iter();
        let arguments = signature.arguments.iter();

        let kinds = variables
            .zip(arguments)
            .map(|(variable, &argument)| {
                // Use contravariant subtyping for type variables:
                //
                // data Example :: Argument -> Type
                // data Example (a :: Variable) = Example
                //
                // Signature: Argument -> Type
                // Inferred: Variable -> Type
                //
                // Given
                //   Variable -> Type <: Argument -> Type
                //
                // Therefore
                //   [Argument <: Variable, Type <: Type]
                let kind = if let Some(kind_id) = variable.kind {
                    let (kind, _) = kind::infer_surface_kind(state, context, kind_id)?;
                    let valid = unification::subtype(state, context, argument, kind)?;
                    if valid { kind } else { context.prim.unknown }
                } else {
                    argument
                };

                let name = variable.name.clone().unwrap_or(MISSING_NAME);
                Ok((variable.id, variable.visible, name, kind))
            })
            .collect::<QueryResult<Vec<_>>>()?;

        let kind_variables = signature.variables;
        let result_kind = signature.result;
        let type_variables = kinds.into_iter().map(|(id, visible, name, kind)| {
            let level = state.type_scope.bind_forall(id, kind);
            ForallBinder { visible, name, level, kind }
        });

        let type_variables = type_variables.collect_vec();

        SignatureLike { kind_variables, type_variables, result_kind }
    } else {
        let kind_variables = vec![];
        let result_kind = infer_result(state);
        let type_variables = variables.iter().map(|variable| {
            let kind = if let Some(id) = variable.kind {
                let (kind, _) = kind::check_surface_kind(state, context, id, context.prim.t)?;
                kind
            } else {
                state.fresh_unification_type(context)
            };

            let visible = variable.visible;
            let name = variable.name.clone().unwrap_or(MISSING_NAME);
            let level = state.type_scope.bind_forall(variable.id, kind);
            Ok(ForallBinder { visible, name, level, kind })
        });

        let type_variables = type_variables.collect::<QueryResult<Vec<_>>>()?;

        SignatureLike { kind_variables, type_variables, result_kind }
    };

    Ok(Some(signature))
}

fn collect_foralls(state: &CheckState, mut id: TypeId) -> (Vec<ForallBinder>, TypeId) {
    let mut foralls = vec![];

    while let Type::Forall(ref binder, inner) = state.storage[id] {
        foralls.push(binder.clone());
        id = inner;
    }

    (foralls, id)
}

pub fn is_binary_operator_type(state: &mut CheckState, mut id: TypeId) -> bool {
    // Normalize to resolve unification variables (important for Scc::Mutual)
    id = state.normalize_type(id);

    while let Type::Forall(_, inner_id) = state.storage[id] {
        id = inner_id;
    }

    let Type::Function(_, result_id) = state.storage[id] else {
        return false;
    };

    let result_id = state.normalize_type(result_id);
    let Type::Function(_, result_id) = state.storage[result_id] else {
        return false;
    };

    let result_id = state.normalize_type(result_id);
    !matches!(state.storage[result_id], Type::Function(_, _))
}

/// Unifies a computed kind with the pending kind for recursive/mutual types.
///
/// Only called when the item has no explicit signature (signature.is_none()).
/// For Scc::Recursive and Scc::Mutual, unifies with the pending kind created
/// by begin_type_group. For Scc::Base, this is a no-op since there's no
/// pending kind to unify with.
fn unify_pending_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    item_kind: TypeId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    if let Some(pending_kind) = state.binding_group.lookup_type(item_id) {
        let _ = unification::subtype(state, context, item_kind, pending_kind)?;
    }
    Ok(())
}

fn check_constructor_arguments<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
) -> QueryResult<Vec<CheckedConstructor>>
where
    Q: ExternalQueries,
{
    let _span = tracing::debug_span!("check_constructor_arguments").entered();
    let mut constructors = vec![];

    for item_id in context.indexed.pairs.data_constructors(item_id) {
        let Some(TermItemIr::Constructor { arguments }) =
            context.lowered.info.get_term_item(item_id)
        else {
            continue;
        };

        let arguments = arguments
            .iter()
            .map(|argument| infer_constructor_argument(state, context, *argument))
            .try_collect()?;

        constructors.push(CheckedConstructor { item_id, arguments });
    }

    Ok(constructors)
}

fn infer_constructor_argument<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    argument: lowering::TypeId,
) -> Result<interner::Id<Type>, building_types::QueryError>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::ConstructorArgument(argument), |state| {
        let _span = tracing::debug_span!("infer_constructor_argument").entered();
        let (inferred_type, _) =
            kind::check_surface_kind(state, context, argument, context.prim.t)?;
        Ok(inferred_type)
    })
}

/// Infers roles for type parameters based on their usage in constructors.
fn infer_roles(
    state: &mut CheckState,
    type_variables: &[ForallBinder],
    constructors: &[CheckedConstructor],
) -> Vec<Role> {
    fn aux(
        state: &mut CheckState,
        roles: &mut [Role],
        variables: &[ForallBinder],
        type_id: TypeId,
        under_constraint: bool,
        is_variable_argument: bool,
    ) {
        let type_id = state.normalize_type(type_id);
        match state.storage[type_id].clone() {
            Type::Variable(Variable::Bound(level, _)) => {
                if let Some(index) = variables.iter().position(|v| v.level == level) {
                    // The following cases infer to nominal roles:
                    //
                    // ```
                    // -- `a` appears under a constraint
                    // data Shown a = Shown ((Show a => a -> String) -> String)
                    //
                    // -- `a` appears under another variable
                    // data Parametric f a = Parametric (f a)
                    // ```
                    let role = if under_constraint || is_variable_argument {
                        Role::Nominal
                    } else {
                        Role::Representational
                    };
                    roles[index] = roles[index].max(role);
                }
            }

            Type::Application(function, argument) => {
                let function_id = state.normalize_type(function);
                let is_type_variable =
                    matches!(state.storage[function_id], Type::Variable(Variable::Bound(..)));

                aux(state, roles, variables, function, under_constraint, false);
                aux(state, roles, variables, argument, under_constraint, is_type_variable);
            }

            Type::Constrained(constraint, inner) => {
                aux(state, roles, variables, constraint, true, false);
                aux(state, roles, variables, inner, true, false);
            }

            Type::Forall(binder, inner) => {
                aux(state, roles, variables, binder.kind, under_constraint, false);
                aux(state, roles, variables, inner, under_constraint, false);
            }

            Type::Function(arg, result) => {
                aux(state, roles, variables, arg, under_constraint, false);
                aux(state, roles, variables, result, under_constraint, false);
            }

            Type::KindApplication(function, argument) => {
                aux(state, roles, variables, function, under_constraint, false);
                aux(state, roles, variables, argument, under_constraint, false);
            }

            Type::Kinded(inner, kind) => {
                aux(state, roles, variables, inner, under_constraint, false);
                aux(state, roles, variables, kind, under_constraint, false);
            }

            Type::OperatorApplication(_, _, left, right) => {
                aux(state, roles, variables, left, under_constraint, false);
                aux(state, roles, variables, right, under_constraint, false);
            }

            Type::Row(row) => {
                for field in row.fields.iter() {
                    aux(state, roles, variables, field.id, under_constraint, false);
                }
                if let Some(tail) = row.tail {
                    aux(state, roles, variables, tail, under_constraint, false);
                }
            }

            Type::SynonymApplication(_, _, _, arguments) => {
                for &arg in arguments.iter() {
                    aux(state, roles, variables, arg, under_constraint, false);
                }
            }

            Type::Variable(ref variable) => match variable {
                Variable::Bound(_, kind) | Variable::Skolem(_, kind) => {
                    aux(state, roles, variables, *kind, under_constraint, false);
                }
                Variable::Free(_) => {}
            },

            Type::Constructor(_, _)
            | Type::Integer(_)
            | Type::Operator(_, _)
            | Type::String(_, _)
            | Type::Unification(_)
            | Type::Unknown => (),
        }
    }

    let mut roles = vec![Role::Phantom; type_variables.len()];

    for constructor in constructors {
        for &field_type in &constructor.arguments {
            aux(state, &mut roles, type_variables, field_type, false, false);
        }
    }

    roles
}

/// Check declared roles against inferred roles, inserting the final roles.
///
/// `data` and `newtype` can only strengthen roles, Phantom -> Nominal; `foreign`
/// can loosen roles, Nominal -> Phantom, since there's no usage to infer from.
fn check_roles(
    state: &mut CheckState,
    type_id: TypeItemId,
    inferred: &[Role],
    declared: &[lowering::Role],
    is_foreign: bool,
) {
    let mut validated = inferred.to_vec();

    for (index, (validated, &inferred, declared)) in
        izip!(validated.iter_mut(), inferred, declared).enumerate()
    {
        let declared = match declared {
            lowering::Role::Phantom => Role::Phantom,
            lowering::Role::Representational => Role::Representational,
            lowering::Role::Nominal => Role::Nominal,
            lowering::Role::Unknown => continue,
        };

        if is_foreign || declared >= inferred {
            *validated = declared;
        } else {
            state.with_error_step(ErrorStep::TypeDeclaration(type_id), |state| {
                state.insert_error(ErrorKind::InvalidRoleDeclaration { index, declared, inferred });
            });
        }
    }

    let validated = Arc::from(validated);
    state.checked.roles.insert(type_id, validated);
}

/// Counts the number of type parameters in a kind by counting function arrows.
fn count_kind_arguments(state: &mut CheckState, type_id: TypeId) -> usize {
    let mut count = 0;
    let mut current_id = type_id;

    safe_loop! {
        current_id = state.normalize_type(current_id);
        match &state.storage[current_id] {
            Type::Function(_, result) => {
                count += 1;
                current_id = *result;
            }
            Type::Forall(_, inner) => {
                current_id = *inner;
            }
            _ => break,
        }
    }

    count
}

/// Checks a foreign type declaration.
fn check_foreign_definition<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TypeItemId,
    declared_roles: &Arc<[lowering::Role]>,
) -> QueryResult<Option<CheckedTypeItem>>
where
    Q: ExternalQueries,
{
    let stored_kind = kind::lookup_file_type(state, context, context.id, item_id)?;
    let parameter_count = count_kind_arguments(state, stored_kind);

    let inferred_roles = vec![Role::Nominal; parameter_count];
    check_roles(state, item_id, &inferred_roles, declared_roles, true);

    Ok(None)
}
