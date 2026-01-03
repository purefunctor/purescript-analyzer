use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TermItemKind, TypeItemId};
use lowering::TermItemIr;

use crate::ExternalQueries;
use crate::algorithm::kind::infer_surface_kind;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{inspect, kind, quantify, term, transfer};
use crate::core::{Instance, debruijn};
use crate::error::ErrorStep;

pub fn check_term_signature<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item_id: TermItemId,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        let Some(item) = context.lowered.info.get_term_item(item_id) else {
            return Ok(());
        };

        match item {
            TermItemIr::Foreign { signature } | TermItemIr::ValueGroup { signature, .. } => {
                let Some(signature) = signature else { return Ok(()) };

                let signature_variables = inspect::collect_signature_variables(context, *signature);
                state.surface_bindings.insert_term(item_id, signature_variables);

                let (inferred_type, _) =
                    kind::check_surface_kind(state, context, *signature, context.prim.t)?;

                let quantified_type = quantify::quantify(state, inferred_type)
                    .map(|(quantified_type, _)| quantified_type)
                    .unwrap_or(inferred_type);

                let global_type = transfer::globalize(state, context, quantified_type);
                state.checked.terms.insert(item_id, global_type);
            }
            TermItemIr::Operator { resolution, .. } => {
                let Some((file_id, term_id)) = *resolution else { return Ok(()) };
                let id = term::lookup_file_term(state, context, file_id, term_id)?;
                let global_type = transfer::globalize(state, context, id);
                state.checked.terms.insert(item_id, global_type);
            }
            _ => (),
        }

        Ok(())
    })
}

pub struct CheckInstance<'a> {
    pub item_id: TermItemId,
    pub constraints: &'a [lowering::TypeId],
    pub arguments: &'a [lowering::TypeId],
    pub resolution: &'a Option<(FileId, TypeItemId)>,
}

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
        let Some(resolution) = *resolution else {
            return Ok(());
        };

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

        let mut core_arguments = vec![];
        for argument in arguments.iter() {
            let (inferred_type, inferred_kind) = infer_surface_kind(state, context, *argument)?;

            let inferred_type = transfer::globalize(state, context, inferred_type);
            let inferred_kind = transfer::globalize(state, context, inferred_kind);

            core_arguments.push((inferred_type, inferred_kind));
        }

        let mut core_constraints = vec![];
        for constraint in constraints.iter() {
            let (inferred_type, inferred_kind) = infer_surface_kind(state, context, *constraint)?;

            let inferred_type = transfer::globalize(state, context, inferred_type);
            let inferred_kind = transfer::globalize(state, context, inferred_kind);

            core_constraints.push((inferred_type, inferred_kind));
        }

        state.checked.instances.push(Instance {
            arguments: core_arguments,
            constraints: core_constraints,
            resolution,
            chain_id,
            chain_position,
        });

        state.type_scope.unbind(debruijn::Level(size.0));

        Ok(())
    })
}

#[derive(Clone, Copy)]
pub struct CheckValueGroup<'a> {
    pub item_id: TermItemId,
    pub signature: &'a Option<lowering::TypeId>,
    pub equations: &'a [lowering::Equation],
}

pub fn check_value_group<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    input: CheckValueGroup<'_>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let CheckValueGroup { item_id, signature, equations } = input;
    state.with_error_step(ErrorStep::TermDeclaration(item_id), |state| {
        if let Some(signature_id) = signature {
            let group_type = term::lookup_file_term(state, context, context.id, item_id)?;

            let surface_bindings = state.surface_bindings.get_term(item_id);
            let surface_bindings = surface_bindings.as_deref().unwrap_or_default();

            let signature =
                inspect::inspect_signature_core(state, context, group_type, surface_bindings)?;

            term::check_equations(state, context, *signature_id, signature, equations)
        } else {
            term::infer_equations(state, context, item_id, equations)
        }
    })
}
