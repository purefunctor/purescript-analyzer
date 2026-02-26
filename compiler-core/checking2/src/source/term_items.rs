use std::mem;

use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TermItemKind, TypeItemId};
use lowering::TermItemIr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{CheckedInstance, Type, TypeId, generalise, toolkit, unification, zonk};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::source::terms::equations;
use crate::source::types;
use crate::state::CheckState;

#[derive(Default)]
struct TermSccState {
    operator: Vec<TermItemId>,
}

pub fn check_term_items<Q>(state: &mut CheckState, context: &CheckContext<Q>) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    check_instance_declarations(state, context)?;
    check_value_groups(state, context)?;
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
            let Some(item) = context.lowered.info.get_term_item(item_id) else {
                return None;
            };
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

    for &argument in arguments {
        (class_type, class_kind) =
            types::infer_application_kind(state, context, (class_type, class_kind), argument)?;
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
            prepare_binding_group(state, context, &items);
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
            check_value_group(state, context, item_id, *signature, equations)?;
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
) -> QueryResult<()>
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
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    if let Some(signature_id) = signature
        && let Some(signature_type) = state.checked.lookup_term(item_id)
    {
        check_value_group_core_check(state, context, signature_id, signature_type, equations)?;
    } else {
        check_value_group_core_infer(state, context, item_id, equations)?;
    }

    Ok(())
}

fn check_value_group_core_check<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    signature_id: lowering::TypeId,
    signature_type: TypeId,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let toolkit::InspectQuantified { quantified, .. } =
        toolkit::inspect_quantified(state, context, signature_type)?;

    let toolkit::InspectFunction { arguments, result } =
        toolkit::inspect_function(state, context, quantified)?;

    let function = context.intern_function_chain(&arguments, result);

    equations::check_equations_core(
        state,
        context,
        signature_id,
        &arguments,
        result,
        function,
        equations,
    )?;

    Ok(())
}

fn check_value_group_core_infer<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
    equations: &[lowering::Equation],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let group_type = state.fresh_unification(context.queries, context.prim.t);
    state.checked.terms.insert(item_id, group_type);
    equations::infer_equations_core(state, context, group_type, equations)?;

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
    let mut pending = Vec::with_capacity(items.len());

    for &item_id in items {
        let Some(kind) = state.checked.terms.get(&item_id).copied() else {
            continue;
        };

        let kind = zonk::zonk(state, context, kind)?;
        let unsolved = generalise::unsolved_unifications(state, context, kind)?;

        pending.push((item_id, kind, unsolved));
    }

    for (item_id, kind, unsolved) in pending {
        let kind = generalise::generalise_unsolved(state, context, kind, &unsolved)?;
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

        if !super::is_binary_operator_type(state, context, t)? {
            let kind_message = state.pretty_id(context, t)?;
            state.insert_error(ErrorKind::InvalidTypeOperator { kind_message });
        }
    }

    Ok(())
}
