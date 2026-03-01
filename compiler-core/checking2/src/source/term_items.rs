use std::mem;

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TermItemKind, TypeItemId};
use lowering::TermItemIr;
use rustc_hash::FxHashMap;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::{
    CheckedInstance, Type, TypeId, constraint, generalise, normalise, toolkit, unification, zonk,
};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::source::terms::equations;
use crate::source::types;
use crate::state::CheckState;

#[derive(Default)]
struct TermSccState {
    operator: Vec<TermItemId>,
    value_groups: FxHashMap<TermItemId, PendingValueGroup>,
}

enum PendingValueGroup {
    Checked { residuals: Vec<TypeId> },
    Inferred { residuals: Vec<TypeId> },
}

pub fn check_term_items<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    check_instance_declarations(state, context)?;
    check_value_groups(state, context)?;
    check_instance_members(state, context)?;
    Ok(())
}

pub fn check_instance_declarations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.term_scc {
        let items = scc.as_slice();

        let items = items.iter().filter_map(|&item_id| {
            let item = context.lowered.info.get_term_item(item_id)?;
            let TermItemIr::Instance { constraints, resolution, arguments, .. } = item else {
                return None;
            };
            let resolution = *resolution;
            Some(CheckInstanceDeclaration { item_id, constraints, resolution, arguments })
        });

        for item in items {
            check_instance_declaration(state, context, item)?;
        }
    }

    Ok(())
}

struct CheckInstanceDeclaration<'a> {
    item_id: TermItemId,
    constraints: &'a [lowering::TypeId],
    resolution: Option<(FileId, TypeItemId)>,
    arguments: &'a [lowering::TypeId],
}

fn check_instance_declaration<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item: CheckInstanceDeclaration,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckInstanceDeclaration { item_id, constraints, resolution, arguments } = item;

    let Some((class_file, class_id)) = resolution else {
        return Ok(());
    };

    let TermItemKind::Instance { id: instance_id } = context.indexed.items[item_id].kind else {
        return Ok(());
    };

    let class_kind = toolkit::lookup_file_type(state, context, class_file, class_id)?;

    let expected_kinds = {
        let toolkit::InspectQuantified { quantified, .. } =
            toolkit::inspect_quantified(state, context, class_kind)?;
        let toolkit::InspectFunction { arguments, .. } =
            toolkit::inspect_function(state, context, quantified)?;
        arguments
    };

    if expected_kinds.len() != arguments.len() {
        state.insert_error(ErrorKind::InstanceHeadMismatch {
            class_file,
            class_item: class_id,
            expected: expected_kinds.len(),
            actual: arguments.len(),
        });
    }

    let mut class_type = context.queries.intern_type(Type::Constructor(class_file, class_id));
    let mut class_kind = class_kind;
    let mut checked_arguments = Vec::with_capacity(arguments.len());

    for &argument in arguments {
        (class_type, class_kind) =
            types::infer_application_kind(state, context, (class_type, class_kind), argument)?;
        let (_, extracted_arguments) =
            toolkit::extract_type_application(state, context, class_type)?;
        if let Some(&checked_argument) = extracted_arguments.last() {
            checked_arguments.push(checked_argument);
        }
    }

    unification::subtype(state, context, class_kind, context.prim.constraint)?;

    let mut checked_constraints = Vec::with_capacity(constraints.len());
    for &constraint in constraints {
        let (constraint_type, _) =
            types::check_kind(state, context, constraint, context.prim.constraint)?;
        checked_constraints.push(constraint_type);
    }

    let mut canonical = class_type;
    for &constraint in checked_constraints.iter().rev() {
        canonical = context.intern_constrained(constraint, canonical);
    }

    constraint::instances::validate_rows(state, context, class_file, class_id, &checked_arguments)?;

    let resolution = (class_file, class_id);
    let canonical = zonk::zonk(state, context, canonical)?;
    let canonical = generalise::generalise_implicit(state, context, canonical)?;

    state.checked.instances.insert(instance_id, CheckedInstance { resolution, canonical });

    Ok(())
}

fn check_value_groups<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.term_scc {
        let items = scc.as_slice();

        for &item in items {
            check_term_signature(state, context, item)?;
        }

        if scc.is_recursive() {
            prepare_binding_group(state, context, items);
        }

        let mut term_scc = TermSccState::default();

        for &item in items {
            check_term_equation(state, context, &mut term_scc, item)?;
        }

        finalise_term_binding_group(state, context, &mut term_scc, items)?;
        finalise_term_operators(state, context, &mut term_scc)?;
    }

    Ok(())
}

fn check_instance_members<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.term_scc {
        for &item_id in scc.as_slice() {
            let Some(TermItemIr::Instance { members, resolution, .. }) =
                context.lowered.info.get_term_item(item_id)
            else {
                continue;
            };

            let Some((class_file, class_id)) = *resolution else {
                continue;
            };

            let TermItemKind::Instance { id: instance_id } = context.indexed.items[item_id].kind
            else {
                continue;
            };

            let Some(instance) = state.checked.lookup_instance(instance_id) else {
                continue;
            };

            let Some(instance) = toolkit::decompose_instance(state, context, &instance)? else {
                continue;
            };

            for member in members.iter() {
                check_instance_member_group(
                    state,
                    context,
                    item_id,
                    member,
                    (class_file, class_id),
                    &instance.constraints,
                    &instance.arguments,
                )?;
            }
        }
    }

    Ok(())
}

fn check_instance_member_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    instance_item_id: TermItemId,
    member: &lowering::InstanceMemberGroup,
    (class_file, class_id): (FileId, TypeItemId),
    instance_constraints: &[TypeId],
    instance_arguments: &[TypeId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_crumb(ErrorCrumb::TermDeclaration(instance_item_id), |state| {
        state.with_implication(|state| {
            check_instance_member_group_core(
                state,
                context,
                member,
                (class_file, class_id),
                instance_constraints,
                instance_arguments,
            )
        })
    })
}

fn check_instance_member_group_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    member: &lowering::InstanceMemberGroup,
    (class_file, class_id): (FileId, TypeItemId),
    instance_constraints: &[TypeId],
    instance_arguments: &[TypeId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for &constraint in instance_constraints {
        state.push_given(constraint);
    }

    let class_member_type = if let Some((member_file, member_id)) = member.resolution {
        Some(toolkit::lookup_file_term(state, context, member_file, member_id)?)
    } else {
        None
    };

    let class_member_type = if let Some(class_member_type) = class_member_type {
        specialise_class_member_type(
            state,
            context,
            class_member_type,
            (class_file, class_id),
            instance_arguments,
        )?
    } else {
        None
    };

    let residuals = if let Some(signature_id) = member.signature {
        let (signature_member_type, _) =
            types::check_kind(state, context, signature_id, context.prim.t)?;

        if let Some(class_member_type) = class_member_type {
            let unified =
                unification::unify(state, context, signature_member_type, class_member_type)?;
            if !unified {
                let expected = state.pretty_id(context, class_member_type)?;
                let actual = state.pretty_id(context, signature_member_type)?;
                state.insert_error(ErrorKind::InstanceMemberTypeMismatch { expected, actual });
            }
        }

        equations::check_equations_with(
            state,
            context,
            equations::EquationTypeOrigin::Explicit(signature_id),
            signature_member_type,
            &member.equations,
        )?
    } else if let Some(specialised_type) = class_member_type {
        equations::check_equations_with(
            state,
            context,
            equations::EquationTypeOrigin::Implicit,
            specialised_type,
            &member.equations,
        )?
    } else {
        vec![]
    };

    for residual in residuals {
        let constraint = state.pretty_id(context, residual)?;
        state.insert_error(ErrorKind::NoInstanceFound { constraint });
    }

    Ok(())
}

fn specialise_class_member_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_member_type: TypeId,
    (class_file, class_id): (FileId, TypeItemId),
    instance_arguments: &[TypeId],
) -> QueryResult<Option<TypeId>>
where
    Q: ExternalQueries,
{
    let Some(class_info) = toolkit::lookup_file_class(state, context, class_file, class_id)? else {
        return Ok(None);
    };

    let toolkit::InspectQuantified { binders, quantified } =
        toolkit::inspect_quantified(state, context, class_member_type)?;

    let class_binder_count = class_info.kind_binders.len() + class_info.type_parameters.len();
    if binders.len() < class_binder_count
        || instance_arguments.len() != class_info.type_parameters.len()
    {
        return Ok(None);
    }

    let (class_binders, member_binders) = binders.split_at(class_binder_count);
    let (kind_binders, type_binders) = class_binders.split_at(class_info.kind_binders.len());

    let mut bindings = NameToType::default();
    for binder in kind_binders {
        let replacement = state.fresh_unification(context.queries, binder.kind);
        bindings.insert(binder.name, replacement);
    }
    for (binder, &argument) in type_binders.iter().zip(instance_arguments) {
        bindings.insert(binder.name, argument);
    }

    let mut specialised = SubstituteName::many(state, context, &bindings, quantified)?;
    specialised = normalise::normalise(state, context, specialised)?;

    let Type::Constrained(constraint, mut constrained) = context.lookup_type(specialised) else {
        return Ok(None);
    };

    let Some(application) = constraint::constraint_application(state, context, constraint)? else {
        return Ok(None);
    };

    if (application.file_id, application.item_id) != (class_file, class_id) {
        return Ok(None);
    }

    for binder in member_binders.iter().rev() {
        let binder_id = context.intern_forall_binder(*binder);
        constrained = context.intern_forall(binder_id, constrained);
    }

    Ok(Some(constrained))
}

fn prepare_binding_group<Q>(state: &mut CheckState, context: &CheckContext<Q>, items: &[TermItemId])
where
    Q: ExternalQueries,
{
    for &item_id in items {
        if state.checked.terms.contains_key(&item_id) {
            continue;
        }
        let t = state.fresh_unification(context.queries, context.prim.t);
        state.checked.terms.insert(item_id, t);
    }
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
        TermItemIr::ValueGroup { signature, equations } => {
            let pending = state.with_implication(|state| {
                check_value_group(state, context, item_id, *signature, equations)
            })?;
            if let Some(pending) = pending {
                scc.value_groups.insert(item_id, pending);
            }
        }
        _ => (),
    }

    Ok(())
}

fn check_value_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    signature: Option<lowering::TypeId>,
    equations: &[lowering::Equation],
) -> QueryResult<Option<PendingValueGroup>>
where
    Q: ExternalQueries,
{
    state.with_error_crumb(ErrorCrumb::TermDeclaration(item_id), |state| {
        check_value_group_core(state, context, item_id, signature, equations)
    })
}

fn check_value_group_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    signature: Option<lowering::TypeId>,
    equations: &[lowering::Equation],
) -> QueryResult<Option<PendingValueGroup>>
where
    Q: ExternalQueries,
{
    if let Some(signature_id) = signature
        && let Some(signature_type) = state.checked.lookup_term(item_id)
    {
        let residuals =
            check_value_group_core_check(state, context, signature_id, signature_type, equations)?;
        Ok(Some(PendingValueGroup::Checked { residuals }))
    } else {
        let residuals = check_value_group_core_infer(state, context, item_id, equations)?;
        Ok(Some(PendingValueGroup::Inferred { residuals }))
    }
}

fn check_value_group_core_check<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_id: lowering::TypeId,
    signature_type: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    equations::check_equations_with(
        state,
        context,
        equations::EquationTypeOrigin::Explicit(signature_id),
        signature_type,
        equations,
    )
}

fn check_value_group_core_infer<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    equations: &[lowering::Equation],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    let group_type = state.fresh_unification(context.queries, context.prim.t);
    state.checked.terms.insert(item_id, group_type);
    equations::infer_equations_core(state, context, group_type, equations)?;

    state.solve_constraints(context)
}

fn finalise_term_binding_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    scc: &mut TermSccState,
    items: &[TermItemId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    struct Pending {
        t: TypeId,
        unsolved: Vec<u32>,
        errors: generalise::ConstraintErrors,
    }

    let mut pending = vec![];

    for &item_id in items {
        let Some(t) = state.checked.terms.get(&item_id).copied() else {
            continue;
        };

        let group = scc.value_groups.remove(&item_id);
        let t = zonk::zonk(state, context, t)?;

        let mut errors = generalise::ConstraintErrors::default();

        let t = match group {
            Some(PendingValueGroup::Checked { residuals }) => {
                errors.unsatisfied.extend(residuals);
                t
            }
            Some(PendingValueGroup::Inferred { residuals }) => {
                generalise::insert_inferred_residuals(state, context, t, residuals, &mut errors)?
            }
            None => t,
        };

        let t = zonk::zonk(state, context, t)?;
        let unsolved = generalise::unsolved_unifications(state, context, t)?;

        pending.push((item_id, Pending { t, unsolved, errors }));
    }

    for (item_id, Pending { t, unsolved, errors }) in pending {
        let t = generalise::generalise_unsolved(state, context, t, &unsolved)?;
        state.checked.terms.insert(item_id, t);

        for constraint in errors.ambiguous {
            let constraint = zonk::zonk(state, context, constraint)?;
            let constraint = state.pretty_id(context, constraint)?;
            state.insert_error(ErrorKind::AmbiguousConstraint { constraint });
        }
        for constraint in errors.unsatisfied {
            let constraint = zonk::zonk(state, context, constraint)?;
            let constraint = state.pretty_id(context, constraint)?;
            state.insert_error(ErrorKind::NoInstanceFound { constraint });
        }
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

        if !super::is_binary_operator_type(state, context, t)? {
            let kind_message = state.pretty_id(context, t)?;
            state.insert_error(ErrorKind::InvalidTypeOperator { kind_message });
        }
    }

    Ok(())
}
