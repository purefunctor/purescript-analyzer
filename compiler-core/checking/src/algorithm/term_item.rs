use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TermItemKind, TypeItemId};
use itertools::Itertools;
use lowering::TermItemIr;

use crate::ExternalQueries;
use crate::algorithm::safety::safe_loop;
use crate::algorithm::state::{CheckContext, CheckState, InstanceHeadBinding, PendingType};
use crate::algorithm::{
    constraint, equation, inspect, kind, quantify, substitute, term, toolkit, transfer, unification,
};
use crate::core::{Instance, InstanceKind, Name, Type, TypeId, Variable, debruijn};
use crate::error::{ErrorKind, ErrorStep};

#[derive(Clone)]
pub struct InferredValueGroup {
    pub inferred_type: TypeId,
    pub residual_constraints: Vec<TypeId>,
}

/// Checks signature declarations for terms.
///
/// This function checks the term signatures for [`TermItemIr::Foreign`],
/// [`TermItemIr::ValueGroup`], and [`TermItemIr::Operator`], inserting
/// them into [`CheckState::pending_terms`] as [`PendingTermType`] entries.
///
/// For [`TermItemIr::ValueGroup`] specifically, it also invokes the
/// [`inspect::collect_signature_variables`] function to collect type
/// variables that need to be rebound during [`check_value_group`].
pub fn check_term_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let _span = tracing::debug_span!("check_term_signature").entered();

        let Some(item) = context.lowered.info.get_term_item(item_id) else {
            return Ok(());
        };

        match item {
            TermItemIr::Foreign { signature } => {
                let Some(signature) = signature else { return Ok(()) };

                let signature_variables = inspect::collect_signature_variables(context, *signature);
                state.surface_bindings.insert_term(item_id, signature_variables);

                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature, context.prim.t)?;

                let quantified_type = quantify::quantify(state, inferred_type)
                    .map(|(quantified_type, _)| quantified_type)
                    .unwrap_or(inferred_type);

                crate::debug_fields!(state, context, { quantified_type = quantified_type });
                state.pending_terms.insert(item_id, PendingType::Immediate(quantified_type));
            }
            TermItemIr::ValueGroup { signature, .. } => {
                let Some(signature) = signature else { return Ok(()) };

                let signature_variables = inspect::collect_signature_variables(context, *signature);
                state.surface_bindings.insert_term(item_id, signature_variables);

                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature, context.prim.t)?;

                crate::debug_fields!(state, context, { inferred_type = inferred_type });
                state.pending_terms.insert(item_id, PendingType::Deferred(inferred_type));
            }
            TermItemIr::Operator { resolution, .. } => {
                let Some((file_id, term_id)) = *resolution else { return Ok(()) };
                let inferred_type = term::lookup_file_term(state, context, file_id, term_id)?;

                crate::debug_fields!(state, context, { inferred_type = inferred_type });
                state.pending_terms.insert(item_id, PendingType::Deferred(inferred_type));
            }
            _ => (),
        }

        Ok(())
    })
}

/// Input fields for [`check_instance`].
pub struct CheckInstance<'a> {
    pub item_id: TermItemId,
    pub constraints: &'a [lowering::TypeId],
    pub arguments: &'a [lowering::TypeId],
    pub resolution: &'a Option<(FileId, TypeItemId)>,
}

/// Checks an instance declaration, deferring instance members.
///
/// See [`check_instance_members`] for instance member checking. Instance
/// members are checked separately to ensure that any value groups that
/// they depend on exist in the environment already. Value groups can be
/// checked first because class member signatures are already known.
///
/// This function checks the kinds of instance arguments and collects the
/// implicit variables that must be rebound when checking instance members.
///
/// [`core::Instance`] information is inserted onto [`CheckState::checked`]
/// upon completion.
///
/// [`core::Instance`]: crate::core::Instance
pub fn check_instance<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckInstance<'_>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckInstance { item_id, constraints, arguments, resolution } = input;
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let _span = tracing::debug_span!("check_instance").entered();

        let Some((class_file, class_item)) = *resolution else {
            return Ok(());
        };

        crate::debug_fields!(state, context, { ?class_file = class_file, ?class_item = class_item });

        let TermItemKind::Instance { id: instance_id } = context.indexed.items[item_id].kind else {
            return Ok(());
        };

        let Some(chain_id) = context.indexed.pairs.instance_chain_id(instance_id) else {
            return Ok(());
        };

        let chain_position =
            context.indexed.pairs.instance_chain_position(instance_id).unwrap_or(0);

        // Save the current size of the environment for unbinding.
        let size = state.type_scope.size();

        let class_kind = kind::lookup_file_type(state, context, class_file, class_item)?;
        let expected_kinds = instantiate_class_kind(state, context, class_kind)?;

        if expected_kinds.len() != arguments.len() {
            state.insert_error(ErrorKind::InstanceHeadMismatch {
                class_file,
                class_item,
                expected: expected_kinds.len(),
                actual: arguments.len(),
            });
        }

        let mut core_arguments = vec![];
        for (argument, expected_kind) in arguments.iter().zip(expected_kinds) {
            let (inferred_type, inferred_kind) =
                kind::check_surface_kind(state, context, *argument, expected_kind)?;
            core_arguments.push((inferred_type, inferred_kind));
        }

        let mut core_constraints = vec![];
        for constraint in constraints.iter() {
            let (inferred_type, inferred_kind) =
                kind::infer_surface_kind(state, context, *constraint)?;
            core_constraints.push((inferred_type, inferred_kind));
        }

        let mut instance = Instance {
            arguments: core_arguments,
            constraints: core_constraints,
            resolution: (class_file, class_item),
            kind: InstanceKind::Chain { id: chain_id, position: chain_position },
            kind_variables: vec![],
        };

        quantify::quantify_instance(state, &mut instance);

        constraint::validate_instance_rows(
            state,
            context,
            class_file,
            class_item,
            &instance.arguments,
        )?;

        let arguments = instance.arguments.iter().map(|&(t, k)| {
            let t = transfer::globalize(state, context, t);
            let k = transfer::globalize(state, context, k);
            (t, k)
        });

        instance.arguments = arguments.collect();

        let constraints = instance.constraints.iter().map(|&(t, k)| {
            let t = transfer::globalize(state, context, t);
            let k = transfer::globalize(state, context, k);
            (t, k)
        });

        instance.constraints = constraints.collect();

        let kind_variables = instance.kind_variables.iter().map(|(name, k)| {
            (name.clone(), transfer::globalize(state, context, *k))
        });

        instance.kind_variables = kind_variables.collect();

        state.checked.instances.insert(instance_id, instance);

        // Capture implicit variables from the instance head before unbinding.
        let implicits = state.type_scope.unbind_implicits(debruijn::Level(size.0));
        state.surface_bindings.insert_instance_head(item_id, Arc::from(implicits));

        Ok(())
    })
}

/// Instantiates a class kind to extract the expected kinds for instance arguments.
///
/// Class kinds have the form `forall k1 k2. T1 -> T2 -> ... -> Constraint` where:
/// - `k1, k2, ...` are kind variables
/// - `T1, T2, ...` are the argument kinds
///
/// This function instantiates kind variables as unification variables and
/// extracts the argument kinds to check them against instance arguments.
pub fn instantiate_class_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_kind: TypeId,
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    const FUEL: u32 = 1_000_000;

    let mut arguments = vec![];
    let mut current = class_kind;

    for _ in 0..FUEL {
        current = toolkit::normalise_expand_type(state, context, current)?;
        match state.storage[current] {
            Type::Forall(ref binder, inner) => {
                let binder_variable = binder.variable.clone();
                let binder_kind = binder.kind;

                let replacement = state.fresh_unification_kinded(binder_kind);
                current =
                    substitute::SubstituteBound::on(state, binder_variable, replacement, inner);
            }

            Type::Function(argument_kind, result_kind) => {
                arguments.push(argument_kind);
                current = result_kind;
            }

            _ => return Ok(arguments),
        }
    }

    unreachable!("fuel exhausted in instantiate_class_kind")
}

/// Input fields for [`check_value_group`].
#[derive(Clone, Copy)]
pub struct CheckValueGroup<'a> {
    pub item_id: TermItemId,
    pub signature: &'a Option<lowering::TypeId>,
    pub equations: &'a [lowering::Equation],
}

/// Checks a value declaration group.
///
/// This function optionally returns [`InferredValueGroup`]
/// for value declarations that do not have a signature.
pub fn check_value_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckValueGroup<'_>,
) -> QueryResult<Option<InferredValueGroup>>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(input.item_id), |state| {
        state.with_implication(|state| {
            let _span = tracing::debug_span!("check_value_group").entered();
            check_value_group_core(context, state, input)
        })
    })
}

fn check_value_group_core<Q>(
    context: &CheckContext<Q>,
    state: &mut CheckState,
    input: CheckValueGroup<'_>,
) -> QueryResult<Option<InferredValueGroup>>
where
    Q: ExternalQueries,
{
    let CheckValueGroup { item_id, signature, equations } = input;
    if let Some(signature_id) = signature {
        let group_type = term::lookup_file_term(state, context, context.id, item_id)?;

        let surface_bindings = state.surface_bindings.get_term(item_id);
        let surface_bindings = surface_bindings.as_deref().unwrap_or_default();

        let signature = inspect::inspect_signature(state, context, group_type, surface_bindings)?;

        equation::check_equations(state, context, *signature_id, signature, equations)?;
        crate::debug_fields!(state, context, { group_type = group_type }, "checked");
        Ok(None)
    } else {
        let (inferred_type, residual_constraints) =
            equation::infer_equations(state, context, item_id, equations)?;
        crate::debug_fields!(state, context, { inferred_type = inferred_type }, "inferred");
        Ok(Some(InferredValueGroup { inferred_type, residual_constraints }))
    }
}

pub fn commit_checked_value_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let Some(PendingType::Deferred(inferred_type)) = state.pending_terms.remove(&item_id) else {
        return Ok(());
    };

    let quantified_type = quantify::quantify(state, inferred_type)
        .map(|(quantified_type, _)| quantified_type)
        .unwrap_or(inferred_type);

    let global_type = transfer::globalize(state, context, quantified_type);
    state.checked.terms.insert(item_id, global_type);

    Ok(())
}

/// Commits remaining pending term entries from [`CheckState::pending_terms`]
/// into [`CheckedModule::terms`].
pub fn commit_pending_terms<Q>(state: &mut CheckState, context: &CheckContext<Q>)
where
    Q: ExternalQueries,
{
    for (item_id, pending_type) in state.pending_terms.drain().collect_vec() {
        let local_type = match pending_type {
            PendingType::Immediate(id) => id,
            PendingType::Deferred(id) => {
                quantify::quantify(state, id).map(|(id, _)| id).unwrap_or(id)
            }
        };
        let global_type = transfer::globalize(state, context, local_type);
        state.checked.terms.insert(item_id, global_type);
    }
}

/// Generalises an [`InferredValueGroup`].
pub fn commit_inferred_value_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    inferred: InferredValueGroup,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let InferredValueGroup { inferred_type, residual_constraints } = inferred;

    let Some(result) =
        quantify::quantify_with_constraints(state, context, inferred_type, residual_constraints)?
    else {
        return Ok(());
    };

    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let _span = tracing::debug_span!("commit_value_group").entered();
        for constraint in result.ambiguous {
            let constraint = state.render_local_type(context, constraint);
            state.insert_error(ErrorKind::AmbiguousConstraint { constraint });
        }
        for constraint in result.unsatisfied {
            let constraint = state.render_local_type(context, constraint);
            state.insert_error(ErrorKind::NoInstanceFound { constraint });
        }
        crate::debug_fields!(state, context, { quantified = result.quantified });
    });

    let type_id = transfer::globalize(state, context, result.quantified);
    state.checked.terms.insert(item_id, type_id);

    Ok(())
}

/// Input fields for [`check_instance_members`].
pub struct CheckInstanceMembers<'a> {
    pub instance_id: TermItemId,
    pub members: &'a [lowering::InstanceMemberGroup],
    pub class_file: FileId,
    pub class_id: TypeItemId,
    pub instance_arguments: &'a [(TypeId, TypeId)],
    pub instance_constraints: &'a [(TypeId, TypeId)],
    pub kind_variables: &'a [(Name, TypeId)],
}

/// Checks instance member declarations.
///
/// As mentioned in [`check_instance`], instance members are checked after
/// value groups to ensure that any value groups an instance member depends
/// on are already known.
///
/// This function uses the implicit variables collected by [`check_instance`]
/// and passes them onto [`check_instance_member_group`], which contains most
/// of the implementation.
pub fn check_instance_members<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckInstanceMembers<'_>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckInstanceMembers {
        instance_id,
        members,
        class_file,
        class_id,
        instance_arguments,
        instance_constraints,
        kind_variables,
    } = input;

    // Fetch implicit variables captured by check_instance.
    let instance_bindings = state.surface_bindings.get_instance_head(instance_id);
    let instance_bindings = instance_bindings.as_deref().unwrap_or(&[]);

    for member in members {
        check_instance_member_group(
            state,
            context,
            CheckInstanceMemberGroup {
                instance_id,
                instance_bindings,
                member,
                class_file,
                class_id,
                instance_arguments,
                instance_constraints,
                kind_variables,
            },
        )?;
    }

    Ok(())
}

fn lookup_class_member<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    resolution: Option<(FileId, TermItemId)>,
) -> QueryResult<Option<TypeId>>
where
    Q: ExternalQueries,
{
    let Some((member_file, member_id)) = resolution else {
        return Ok(None);
    };

    let global_type = if member_file == context.id {
        state.checked.terms.get(&member_id).copied()
    } else {
        let checked = context.queries.checked(member_file)?;
        checked.terms.get(&member_id).copied()
    };

    Ok(global_type.map(|global_type| transfer::localize(state, context, global_type)))
}

/// Input fields for [`check_instance_member_group`].
pub struct CheckInstanceMemberGroup<'a> {
    instance_id: TermItemId,
    instance_bindings: &'a [InstanceHeadBinding],
    member: &'a lowering::InstanceMemberGroup,
    class_file: FileId,
    class_id: TypeItemId,
    instance_arguments: &'a [(TypeId, TypeId)],
    instance_constraints: &'a [(TypeId, TypeId)],
    kind_variables: &'a [(Name, TypeId)],
}

/// Checks an instance member group against its specialised class member type.
///
/// This rule maintains the following invariants:
/// - Check mode: `inferred_type <: signature_type` and `signature_type ~ specialised_type`
/// - Infer mode: `inferred_type <: specialised_type`
///
/// The inferred type of a member group is a subtype of the signature_type,
/// which may be polymorphic. Recall that the subtyping rule skolemises
/// polymorphic types that occur on the right-hand side.
///
/// The signature type of a member group must unify with the specialised
/// type of the class member. The signature cannot be more general than
/// the specialised type. See tests 118 and 125 for a demonstration.
pub fn check_instance_member_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckInstanceMemberGroup<'_>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(input.instance_id), |state| {
        state.with_implication(|state| check_instance_member_group_core(state, context, input))
    })
}

fn check_instance_member_group_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckInstanceMemberGroup<'_>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckInstanceMemberGroup {
        instance_bindings,
        member,
        class_file,
        class_id,
        instance_arguments,
        instance_constraints,
        kind_variables,
        ..
    } = input;

    let _span = tracing::debug_span!("check_instance_member_group").entered();

    // Save the current size of the environment for unbinding.
    let size = state.type_scope.size();

    // Bind kind variables generalised after instance head checking.
    for (name, kind_variable) in kind_variables {
        let kind = transfer::localize(state, context, *kind_variable);
        let name = state.fresh_name(&name.text);
        state.type_scope.bind_core(kind, name);
    }

    for binding in instance_bindings {
        state.type_scope.bind_implicit(
            binding.node,
            binding.id,
            binding.kind,
            binding.name.clone(),
        );
    }

    let class_member_type = lookup_class_member(state, context, member.resolution)?;

    for (constraint_type, _) in instance_constraints {
        let local_constraint = transfer::localize(state, context, *constraint_type);
        state.push_given(local_constraint);
    }

    let specialised_type = if let Some(class_member_type) = class_member_type {
        specialise_class_member(
            state,
            context,
            class_member_type,
            (class_file, class_id),
            instance_arguments,
        )?
    } else {
        None
    };

    // The specialised type may have constraints like `Show a => (a -> b) -> f a -> f b`.
    // We push `Show a` as a given and use the body `(a -> b) -> f a -> f b` for checking.
    let specialised_type = specialised_type.map(|mut t| {
        safe_loop! {
            let normalized = state.normalize_type(t);
            let Type::Constrained(constraint, constrained) = state.storage[normalized] else {
                break t;
            };
            state.push_given(constraint);
            t = constrained;
        }
    });

    if let Some(signature_id) = &member.signature {
        let surface_bindings = inspect::collect_signature_variables(context, *signature_id);

        let (member_type, _) =
            kind::check_surface_kind(state, context, *signature_id, context.prim.t)?;

        if let Some(specialised_type) = specialised_type {
            let unified = unification::unify(state, context, member_type, specialised_type)?;
            if !unified {
                let expected = state.render_local_type(context, specialised_type);
                let actual = state.render_local_type(context, member_type);
                state.insert_error(ErrorKind::InstanceMemberTypeMismatch { expected, actual });
            }
        }

        let signature = inspect::inspect_signature(state, context, member_type, &surface_bindings)?;

        equation::check_equations(state, context, *signature_id, signature, &member.equations)?;
    } else if let Some(specialised_type) = specialised_type {
        let inferred_type = state.fresh_unification_type(context);
        equation::infer_equations_core(state, context, inferred_type, &member.equations)?;

        let origin = equation::ExhaustivenessOrigin::FromType(specialised_type);
        equation::patterns(state, context, origin, &member.equations)?;

        let matches = unification::subtype(state, context, inferred_type, specialised_type)?;
        if !matches {
            let expected = state.render_local_type(context, specialised_type);
            let actual = state.render_local_type(context, inferred_type);
            state.insert_error(ErrorKind::InstanceMemberTypeMismatch { expected, actual });
        }

        let _ = equation::constraints(state, context, equation::ConstraintsPolicy::Report)?;
    }

    state.type_scope.unbind(debruijn::Level(size.0));

    Ok(())
}

macro_rules! debug_assert_class_constraint {
    ($state:expr, $constraint:expr, $class_file:expr, $class_id:expr) => {
        #[cfg(debug_assertions)]
        {
            let application = constraint::constraint_application($state, $constraint);
            debug_assert!(
                application.is_some_and(|a| a.file_id == $class_file && a.item_id == $class_id),
                "invariant violated: expected to find class constraint"
            );
        }
        // Silence unused variable errors.
        let _ = ($state, $constraint, $class_file, $class_id);
    };
}

/// Specialises a class member type for a specific instance.
///
/// Given a class member type like `forall a. Show a => a -> String`,
/// and instance arguments like `Int`, this returns `Int -> String`.
fn specialise_class_member<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_member_type: TypeId,
    (class_file, class_id): (FileId, TypeItemId),
    instance_arguments: &[(TypeId, TypeId)],
) -> QueryResult<Option<TypeId>>
where
    Q: ExternalQueries,
{
    let Some(class_info) = constraint::lookup_file_class(state, context, class_file, class_id)?
    else {
        return Ok(None);
    };

    let mut specialised = class_member_type;

    let arguments = instance_arguments.iter().map(|(t, k)| {
        let t = transfer::localize(state, context, *t);
        let k = transfer::localize(state, context, *k);
        (t, k)
    });

    let arguments = arguments.collect_vec();
    let kind_variables = class_info.quantified_variables.0 + class_info.kind_variables.0;

    // Class member types are originally bound with only the class' type
    // variables in scope. When specialising for an instance, we shift
    // their levels up by the current scope size to avoid conflicts.
    //
    // First, without shifting
    //
    // ```purescript
    // class Functor f:0 where
    //   map :: forall a:1 b:2. (a -> b) -> f a -> f b
    //
    // instance Functor (Vector s:0 n:1) where
    //   -- map :: forall f:0 a:1 b:2. (a -> b) -> f a -> f b
    // ```
    //
    // Then, with shifting
    //
    // ```purescript
    // instance Functor (Vector s:0 n:1) where
    //   -- map :: forall f:2 a:3 b:4. (a -> b) -> f a -> f b
    // ```
    // With globally unique Names, shifting is no longer needed.

    let mut kind_variables = 0..kind_variables;
    let mut arguments = arguments.into_iter();

    while let normalized = state.normalize_type(specialised)
        && let Type::Forall(binder, inner) = &state.storage[normalized]
    {
        let binder_variable = binder.variable.clone();
        let binder_kind = binder.kind;
        let inner = *inner;

        let replacement = if kind_variables.next().is_some() {
            state.fresh_unification_kinded(binder_kind)
        } else if let Some((argument_type, argument_kind)) = arguments.next() {
            let _ = unification::unify(state, context, binder_kind, argument_kind);
            argument_type
        } else {
            let skolem = Variable::Skolem(binder_variable.clone(), binder_kind);
            state.storage.intern(Type::Variable(skolem))
        };

        specialised = substitute::SubstituteBound::on(state, binder_variable, replacement, inner);
    }

    specialised = state.normalize_type(specialised);
    if let Type::Constrained(constraint, constrained) = state.storage[specialised] {
        debug_assert_class_constraint!(state, constraint, class_file, class_id);
        specialised = constrained;
    }

    Ok(Some(specialised))
}
