use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TermItemKind, TypeItemId};
use lowering::TermItemIr;
use rustc_hash::FxHashMap;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::constraint::CanonicalConstraintId;
use crate::core::substitute::{NameToType, SubstituteName};
use crate::core::{
    CheckedInstance, ForallBinder, KindOrType, Type, TypeId, constraint, generalise, normalise,
    signature, toolkit, unification, zonk,
};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::source::terms::equations;
use crate::source::{derive, types};
use crate::state::CheckState;

#[derive(Default)]
struct TermSccState {
    value_groups: FxHashMap<TermItemId, PendingValueGroup>,
}

enum PendingValueGroup {
    Checked { residuals: Vec<CanonicalConstraintId> },
    Inferred { residuals: Vec<CanonicalConstraintId> },
}

pub fn check_term_items<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    check_instance_declarations(state, context)?;
    let derive_results = derive::check_derive_declarations(state, context)?;
    check_value_groups(state, context)?;
    check_instance_members(state, context)?;
    derive::check_derive_members(state, context, &derive_results)?;
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
        let signature::DecomposedSignature { arguments, .. } = signature::decompose_signature(
            state,
            context,
            class_kind,
            signature::DecomposeSignatureMode::Full,
        )?;
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
                    &instance,
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
    instance: &toolkit::DecomposedInstance,
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
                instance,
            )
        })
    })
}

fn check_instance_member_group_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    member: &lowering::InstanceMemberGroup,
    (class_file, class_id): (FileId, TypeItemId),
    instance: &toolkit::DecomposedInstance,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let FreshenedInstanceRigids {
        constraints: instance_constraints,
        arguments: instance_arguments,
        substitution,
    } = freshen_instance_rigids(state, context, instance)?;

    state.with_implicit(context, &substitution, |state| {
        for &constraint in &instance_constraints {
            if !constraint::is_type_error(state, context, constraint)? {
                state.push_given(constraint);
            }
        }

        let class_member_type = if let Some((member_file, member_id)) = member.resolution {
            Some(toolkit::lookup_file_term(state, context, member_file, member_id)?)
        } else {
            None
        };

        let class_member_type = if let Some(class_member_type) = class_member_type {
            instantiate_class_member_type(
                state,
                context,
                class_member_type,
                (class_file, class_id),
                &instance_arguments,
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

            let equation_set = equations::analyse_equation_set(
                state,
                context,
                equations::EquationMode::Check {
                    origin: equations::EquationTypeOrigin::Explicit(signature_id),
                    expected_type: signature_member_type,
                },
                &member.equations,
            )?;
            let exhaustiveness = equations::compute_equation_exhaustiveness(
                state,
                context,
                &equation_set,
                &member.equations,
            )?;
            state.report_exhaustiveness(context, exhaustiveness);
            state.solve_constraints(context)?
        } else if let Some(expected_type) = class_member_type {
            let equation_set = equations::analyse_equation_set(
                state,
                context,
                equations::EquationMode::Check {
                    origin: equations::EquationTypeOrigin::Implicit,
                    expected_type,
                },
                &member.equations,
            )?;
            let exhaustiveness = equations::compute_equation_exhaustiveness(
                state,
                context,
                &equation_set,
                &member.equations,
            )?;
            state.report_exhaustiveness(context, exhaustiveness);
            state.solve_constraints(context)?
        } else {
            vec![]
        };

        for residual in residuals {
            let constraint = state.pretty_constraint_id(context, residual)?;
            state.insert_error(ErrorKind::NoInstanceFound { constraint });
        }

        Ok(())
    })
}

struct FreshenedInstanceRigids {
    constraints: Vec<TypeId>,
    arguments: Vec<KindOrType>,
    substitution: NameToType,
}

fn freshen_instance_rigids<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    instance: &toolkit::DecomposedInstance,
) -> QueryResult<FreshenedInstanceRigids>
where
    Q: ExternalQueries,
{
    let mut substitution = NameToType::default();

    for binder in &instance.binders {
        let kind = SubstituteName::many(state, context, &substitution, binder.kind)?;
        let text = state.checked.lookup_name(binder.name);
        let rigid = state.fresh_rigid_named(context.queries, kind, text);
        substitution.insert(binder.name, rigid);
    }

    let constraints = instance
        .constraints
        .iter()
        .map(|&constraint| SubstituteName::many(state, context, &substitution, constraint))
        .collect::<QueryResult<Vec<_>>>()?;

    let arguments = instance
        .arguments
        .iter()
        .map(|&argument| substitute_kind_or_type(state, context, &substitution, argument))
        .collect::<QueryResult<Vec<_>>>()?;

    Ok(FreshenedInstanceRigids { constraints, arguments, substitution })
}

fn instantiate_class_member_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    class_member_type: TypeId,
    (class_file, class_id): (FileId, TypeItemId),
    instance_arguments: &[KindOrType],
) -> QueryResult<Option<TypeId>>
where
    Q: ExternalQueries,
{
    let Some(class_info) = toolkit::lookup_file_class(state, context, class_file, class_id)? else {
        return Ok(None);
    };

    let signature::DecomposedSignature { binders, constraints, arguments, result } =
        signature::decompose_signature(
            state,
            context,
            class_member_type,
            signature::DecomposeSignatureMode::Full,
        )?;

    let class_binder_count = class_info.kind_binders.len() + class_info.type_parameters.len();
    if binders.len() < class_binder_count {
        return Ok(None);
    }

    let (class_binders, member_binders) = binders.split_at(class_binder_count);

    let mut bindings = NameToType::default();
    let mut instance_arguments = instance_arguments.iter().copied();

    for binder in class_binders {
        let Some(argument) = instance_arguments.next() else {
            return Ok(None);
        };

        let argument = match argument {
            KindOrType::Kind(argument) | KindOrType::Type(argument) => argument,
        };

        bindings.insert(binder.name, argument);
    }

    if instance_arguments.next().is_some() {
        return Ok(None);
    }

    let constraints = substitute_normalise_types(state, context, &bindings, &constraints)?;
    let arguments = substitute_normalise_types(state, context, &bindings, &arguments)?;
    let mut constrained = substitute_normalise_type(state, context, &bindings, result)?;

    let mut constraints = constraints.into_iter();
    let Some(constraint) = constraints.next() else {
        return Ok(None);
    };

    let Some(constraint) = constraint::canonical::canonicalise(state, context, constraint)? else {
        return Ok(None);
    };

    let constraint = state.canonicals[constraint].clone();

    if (constraint.file_id, constraint.type_id) != (class_file, class_id) {
        return Ok(None);
    }

    constrained = context.intern_function_list(&arguments, constrained);
    for constraint in constraints.rev() {
        constrained = context.intern_constrained(constraint, constrained);
    }

    for binder in member_binders.iter().rev() {
        // member_binders refers to type variables from class_binders
        let kind = substitute_normalise_type(state, context, &bindings, binder.kind)?;
        let binder_id = context.intern_forall_binder(ForallBinder { kind, ..*binder });
        constrained = context.intern_forall(binder_id, constrained);
    }

    Ok(Some(constrained))
}

fn substitute_normalise_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &NameToType,
    type_id: TypeId,
) -> QueryResult<TypeId>
where
    Q: ExternalQueries,
{
    let type_id = SubstituteName::many(state, context, bindings, type_id)?;
    normalise::normalise(state, context, type_id)
}

fn substitute_normalise_types<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &NameToType,
    types: &[TypeId],
) -> QueryResult<Vec<TypeId>>
where
    Q: ExternalQueries,
{
    types
        .iter()
        .map(|&type_id| substitute_normalise_type(state, context, bindings, type_id))
        .collect()
}

fn substitute_kind_or_type<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    bindings: &NameToType,
    argument: KindOrType,
) -> QueryResult<KindOrType>
where
    Q: ExternalQueries,
{
    Ok(match argument {
        KindOrType::Kind(argument) => {
            KindOrType::Kind(SubstituteName::many(state, context, bindings, argument)?)
        }
        KindOrType::Type(argument) => {
            KindOrType::Type(SubstituteName::many(state, context, bindings, argument)?)
        }
    })
}

fn prepare_binding_group<Q>(state: &mut CheckState, context: &CheckContext<Q>, items: &[TermItemId])
where
    Q: ExternalQueries,
{
    for &item_id in items {
        if state.checked.terms.contains_key(&item_id) {
            continue;
        }

        let item = context.lowered.info.get_term_item(item_id);

        let resolution = item.and_then(|item| match item {
            TermItemIr::Operator { resolution, .. } => *resolution,
            _ => None,
        });

        let item_type = resolution.and_then(|(file_id, item_id)| {
            if file_id == context.id { state.checked.lookup_term(item_id) } else { None }
        });

        let item_type = if let Some(item_type) = item_type {
            item_type
        } else {
            state.fresh_unification(context.queries, context.prim.t)
        };

        state.checked.terms.insert(item_id, item_type);
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
            check_term_operator(state, context, item_id, *resolution)?;
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
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let equation_set = equations::analyse_equation_set(
        state,
        context,
        equations::EquationMode::Check {
            origin: equations::EquationTypeOrigin::Explicit(signature_id),
            expected_type: signature_type,
        },
        equations,
    )?;
    let exhaustiveness =
        equations::compute_equation_exhaustiveness(state, context, &equation_set, equations)?;
    state.report_exhaustiveness(context, exhaustiveness);
    state.solve_constraints(context)
}

fn check_value_group_core_infer<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    equations: &[lowering::Equation],
) -> QueryResult<Vec<CanonicalConstraintId>>
where
    Q: ExternalQueries,
{
    let group_type = state.fresh_unification(context.queries, context.prim.t);
    state.checked.terms.insert(item_id, group_type);
    let equation_set = equations::analyse_equation_set(
        state,
        context,
        equations::EquationMode::Infer { group_type },
        equations,
    )?;
    let exhaustiveness =
        equations::compute_equation_exhaustiveness(state, context, &equation_set, equations)?;
    state.report_exhaustiveness(context, exhaustiveness);

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
            let constraint = state.pretty_constraint_id(context, constraint)?;
            state.with_error_crumb(ErrorCrumb::TermDeclaration(item_id), |state| {
                state.insert_error(ErrorKind::AmbiguousConstraint { constraint });
            });
        }
        for constraint in errors.unsatisfied {
            let constraint = state.pretty_constraint_id(context, constraint)?;
            state.with_error_crumb(ErrorCrumb::TermDeclaration(item_id), |state| {
                state.insert_error(ErrorKind::NoInstanceFound { constraint });
            });
        }
    }

    Ok(())
}

fn check_term_operator<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
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

    Ok(())
}
