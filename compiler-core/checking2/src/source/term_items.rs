use std::mem;

use building_types::QueryResult;
use files::FileId;
use indexing::TermItemId;
use lowering::TermItemIr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{generalise, toolkit, unification, zonk};
use crate::error::ErrorKind;
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

    if let TermItemIr::Operator { resolution, .. } = item {
        check_term_operator(state, context, scc, item_id, *resolution)?;
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

        if !super::is_binary_operator_type(state, context, t)? {
            let kind_message = state.pretty_id(context, t)?;
            state.insert_error(ErrorKind::InvalidTypeOperator { kind_message });
        }
    }

    Ok(())
}
