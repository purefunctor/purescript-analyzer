use building_types::QueryResult;
use files::FileId;
use indexing::{TermItemId, TermItemKind, TypeItemId};
use lowering::TermItemIr;

use crate::ExternalQueries;
use crate::context::CheckContext;
use crate::core::{CheckedInstance, Type, constraint, generalise, toolkit, unification, zonk};
use crate::error::{ErrorCrumb, ErrorKind};
use crate::source::types;
use crate::state::CheckState;

use super::{DeriveDispatch, DeriveHeadResult, DeriveStrategy, derive_dispatch, eq_ord};

pub fn check_derive_declarations<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
) -> QueryResult<Vec<DeriveHeadResult>>
where
    Q: ExternalQueries,
{
    let mut results = vec![];

    for scc in &context.grouped.term_scc {
        let items = scc.as_slice();

        let items = items.iter().filter_map(|&item_id| {
            let item = context.lowered.info.get_term_item(item_id)?;
            let TermItemIr::Derive { constraints, resolution, arguments, .. } = item else {
                return None;
            };
            let resolution = *resolution;
            Some(CheckDeriveDeclaration { item_id, constraints, resolution, arguments })
        });

        for item in items {
            if let Some(result) = check_derive_declaration(state, context, item)? {
                results.push(result);
            }
        }
    }

    Ok(results)
}

struct CheckDeriveDeclaration<'a> {
    item_id: TermItemId,
    constraints: &'a [lowering::TypeId],
    resolution: Option<(FileId, TypeItemId)>,
    arguments: &'a [lowering::TypeId],
}

struct CheckDeriveDeclarationCore<'a> {
    derive_id: indexing::DeriveId,
    item_id: TermItemId,
    class_file: FileId,
    class_id: TypeItemId,
    constraints: &'a [lowering::TypeId],
    arguments: &'a [lowering::TypeId],
}

fn check_derive_declaration<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item: CheckDeriveDeclaration,
) -> QueryResult<Option<DeriveHeadResult>>
where
    Q: ExternalQueries,
{
    let CheckDeriveDeclaration { item_id, constraints, resolution, arguments } = item;

    let Some((class_file, class_id)) = resolution else {
        return Ok(None);
    };

    let TermItemKind::Derive { id: derive_id } = context.indexed.items[item_id].kind else {
        return Ok(None);
    };

    state.with_error_crumb(ErrorCrumb::TermDeclaration(item_id), |state| {
        check_derive_declaration_core(state, context, CheckDeriveDeclarationCore {
            derive_id,
            item_id,
            class_file,
            class_id,
            constraints,
            arguments,
        })
    })
}

fn check_derive_declaration_core<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    item: CheckDeriveDeclarationCore,
) -> QueryResult<Option<DeriveHeadResult>>
where
    Q: ExternalQueries,
{
    let CheckDeriveDeclarationCore {
        derive_id,
        item_id,
        class_file,
        class_id,
        constraints,
        arguments,
    } = item;

    let class_kind = toolkit::lookup_file_type(state, context, class_file, class_id)?;

    let expected_kinds = {
        let toolkit::InspectQuantified { quantified, .. } =
            toolkit::inspect_quantified(state, context, class_kind)?;
        let toolkit::InspectFunction { arguments, .. } =
            toolkit::inspect_function(state, context, quantified)?;
        arguments
    };

    if expected_kinds.len() != arguments.len() {
        state.insert_error(ErrorKind::DeriveInvalidArity {
            class_file,
            class_id,
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

    state.checked.derived.insert(derive_id, CheckedInstance { resolution, canonical });

    let strategy = match derive_dispatch(context, class_file, class_id) {
        DeriveDispatch::Eq => eq_ord::check_derive_eq(
            state,
            context,
            class_file,
            class_id,
            &checked_arguments,
        )?,
        DeriveDispatch::SupportedButNotImplemented => Some(DeriveStrategy::Unsupported),
        DeriveDispatch::Unsupported => {
            state.insert_error(ErrorKind::CannotDeriveClass { class_file, class_id });
            None
        }
    };

    Ok(strategy.map(|strategy| DeriveHeadResult {
        item_id,
        constraints: checked_constraints,
        class_file,
        class_id,
        arguments: checked_arguments,
        strategy,
    }))
}
