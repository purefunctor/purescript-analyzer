pub mod inspect;
pub mod items;
pub mod kind;
pub mod operator;
pub mod quantify;
pub mod state;
pub mod substitute;
pub mod term;
pub mod transfer;
pub mod unification;

use building_types::QueryResult;
use files::FileId;
use itertools::Itertools;
use lowering::Scc;

use crate::core::{Type, TypeId};
use crate::{CheckedModule, ExternalQueries};

pub(crate) fn check_source(
    queries: &impl ExternalQueries,
    file_id: FileId,
) -> QueryResult<CheckedModule> {
    let mut state = state::CheckState::default();
    let context = state::CheckContext::new(queries, &mut state, file_id)?;

    for scc in &context.lowered.type_scc {
        match scc {
            Scc::Base(id) | Scc::Recursive(id) => {
                items::check_type_signature(&mut state, &context, *id)?;
            }
            Scc::Mutual(mutual) => {
                for id in mutual {
                    items::check_type_signature(&mut state, &context, *id)?;
                }
            }
        }
    }

    let needs_binding_group = |id: &indexing::TypeItemId| {
        let item = &context.indexed.items[*id];
        matches!(
            item.kind,
            indexing::TypeItemKind::Data { signature: None, .. }
                | indexing::TypeItemKind::Newtype { signature: None, .. }
                | indexing::TypeItemKind::Synonym { signature: None, .. }
                | indexing::TypeItemKind::Class { signature: None, .. }
        )
    };

    for scc in &context.lowered.type_scc {
        match scc {
            Scc::Base(item) | Scc::Recursive(item) => {
                if !state.binding_group.types.contains_key(item) && needs_binding_group(item) {
                    state.type_binding_group(&context, [*item]);
                }
                items::check_type_item(&mut state, &context, *item)?;
                state.commit_binding_group(&context);
            }
            Scc::Mutual(items) => {
                let with_signature = items
                    .iter()
                    .filter(|item| state.binding_group.types.contains_key(item))
                    .copied()
                    .collect_vec();

                let without_signature =
                    items.iter().filter(|item| needs_binding_group(item)).copied().collect_vec();

                let group = without_signature.iter().copied();
                state.type_binding_group(&context, group);

                for item in &without_signature {
                    items::check_type_item(&mut state, &context, *item)?;
                }
                state.commit_binding_group(&context);

                for item in &with_signature {
                    items::check_type_item(&mut state, &context, *item)?;
                }
                state.commit_binding_group(&context);
            }
        }
    }

    for scc in &context.lowered.term_scc {
        match scc {
            Scc::Base(item) | Scc::Recursive(item) => {
                items::check_term_signature(&mut state, &context, *item)?;
            }
            Scc::Mutual(items) => {
                for item in items {
                    items::check_term_signature(&mut state, &context, *item)?;
                }
            }
        }
    }

    let needs_binding_group = |item: &indexing::TermItemId| {
        let item = &context.indexed.items[*item];
        matches!(item.kind, indexing::TermItemKind::Value { signature: None, .. })
    };

    for scc in &context.lowered.term_scc {
        match scc {
            Scc::Base(item) | Scc::Recursive(item) => {
                if !state.binding_group.terms.contains_key(item) && needs_binding_group(item) {
                    state.term_binding_group(&context, [*item]);
                }
                items::check_term_item(&mut state, &context, *item)?;
                state.commit_binding_group(&context);
            }
            Scc::Mutual(items) => {
                let with_signature = items
                    .iter()
                    .filter(|item| state.binding_group.terms.contains_key(item))
                    .copied()
                    .collect_vec();

                let without_signature =
                    items.iter().filter(|item| needs_binding_group(item)).copied().collect_vec();

                let group = without_signature.iter().copied();
                state.term_binding_group(&context, group);

                for item in &without_signature {
                    items::check_term_item(&mut state, &context, *item)?;
                }
                state.commit_binding_group(&context);

                for item in &with_signature {
                    items::check_term_item(&mut state, &context, *item)?;
                }
                state.commit_binding_group(&context);
            }
        }
    }

    Ok(state.checked)
}

pub(crate) fn check_prim(
    queries: &impl ExternalQueries,
    file_id: FileId,
) -> QueryResult<CheckedModule> {
    let mut checked_module = CheckedModule::default();
    let resolved = queries.resolved(file_id)?;

    let lookup_type = |name: &str| {
        let prim_type = resolved.exports.lookup_type(name);
        prim_type.unwrap_or_else(|| unreachable!("invariant violated: {name} not in Prim"))
    };

    let type_core = {
        let (file_id, item_id) = lookup_type("Type");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let row_core = {
        let (file_id, item_id) = lookup_type("Row");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let constraint_core = {
        let (file_id, item_id) = lookup_type("Constraint");
        queries.intern_type(Type::Constructor(file_id, item_id))
    };

    let type_to_type_core = queries.intern_type(Type::Function(type_core, type_core));
    let function_core = queries.intern_type(Type::Function(type_core, type_to_type_core));

    let row_type_core = queries.intern_type(Type::Application(row_core, type_core));
    let record_core = queries.intern_type(Type::Function(row_type_core, type_core));

    let mut insert_type = |name: &str, id: TypeId| {
        let (_, item_id) = lookup_type(name);
        checked_module.types.insert(item_id, id)
    };

    insert_type("Type", type_core);
    insert_type("Function", function_core);
    insert_type("Array", type_to_type_core);
    insert_type("Record", record_core);
    insert_type("Number", type_core);
    insert_type("Int", type_core);
    insert_type("String", type_core);
    insert_type("Char", type_core);
    insert_type("Boolean", type_core);
    insert_type("Partial", constraint_core);
    insert_type("Constraint", type_core);
    insert_type("Symbol", type_core);
    insert_type("Row", type_to_type_core);

    Ok(checked_module)
}
