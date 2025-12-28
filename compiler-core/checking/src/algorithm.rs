//! Implements the type checking algorithm for PureScript.
//!
//! The type checker algorithm is organised into different submodules,
//! which are listed below. This module implements the [`check_source`]
//! and [`check_prim`] functions. `check_source` defines the order in
//! which items in a module are checked; `check_prim` implements special
//! handling for the `Prim` module as it contains language primitives.

/// Inference and checking for [`lowering::BinderKind`].
pub mod binder;

/// Implements the type class constraint solver.
pub mod constraint;

/// Implements type folding for traversals that modify.
pub mod fold;

/// Implements type signature inspection.
pub mod inspect;

/// Implements kind inference and checking for [`lowering::TypeKind`].
pub mod kind;

/// Implements surface-generic operator chain inference.
pub mod operator;

/// Implements generalisation for inferred types.
pub mod quantify;

/// Implements the algorithm's core state structures.
pub mod state;

/// Implements various type variable substitutions.
pub mod substitute;

/// Implements type inference and checking for [`lowering::ExpressionKind`].
pub mod term;

/// Implements type inference and checking for [`lowering::TermItemIr`].
pub mod term_item;

/// Implements context transfer for types.
pub mod transfer;

/// Implements type inference and checking for [`lowering::TypeItemIr`].
pub mod type_item;

/// Implements the subsumption and unification algorithms.
pub mod unification;

use std::slice;

use building_types::QueryResult;
use files::FileId;
use itertools::Itertools;
use lowering::Scc;

use crate::core::{Type, TypeId};
use crate::{CheckedModule, ExternalQueries};

pub fn check_source(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let mut state = state::CheckState::default();
    let context = state::CheckContext::new(queries, &mut state, file_id)?;

    check_type_signatures(&mut state, &context)?;
    check_type_items(&mut state, &context)?;

    check_term_signatures(&mut state, &context)?;
    check_instances(&mut state, &context)?;
    check_value_groups(&mut state, &context)?;

    Ok(state.checked)
}

fn check_type_signatures<Q: ExternalQueries>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()> {
    for scc in &context.lowered.type_scc {
        match scc {
            Scc::Base(id) | Scc::Recursive(id) => {
                type_item::check_type_signature(state, context, *id)?;
            }
            Scc::Mutual(mutual) => {
                for id in mutual {
                    type_item::check_type_signature(state, context, *id)?;
                }
            }
        }
    }

    Ok(())
}

fn check_type_items<Q: ExternalQueries>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()> {
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
                    state.type_binding_group(context, [*item]);
                }
                type_item::check_type_item(state, context, *item)?;
                state.commit_binding_group(context);
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
                state.type_binding_group(context, group);

                for item in &without_signature {
                    type_item::check_type_item(state, context, *item)?;
                }
                state.commit_binding_group(context);

                for item in &with_signature {
                    type_item::check_type_item(state, context, *item)?;
                }
                state.commit_binding_group(context);
            }
        }
    }

    Ok(())
}

fn check_term_signatures<Q: ExternalQueries>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()> {
    for scc in &context.lowered.term_scc {
        match scc {
            Scc::Base(item) | Scc::Recursive(item) => {
                term_item::check_term_signature(state, context, *item)?;
            }
            Scc::Mutual(items) => {
                for item in items {
                    term_item::check_term_signature(state, context, *item)?;
                }
            }
        }
    }

    Ok(())
}

fn check_instances<Q: ExternalQueries>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()> {
    let is_instance = |item: &&indexing::TermItemId| {
        matches!(context.indexed.items[**item].kind, indexing::TermItemKind::Instance { .. })
    };

    let flattened = context.lowered.term_scc.iter().flat_map(|scc| match scc {
        Scc::Base(item) | Scc::Recursive(item) => slice::from_ref(item),
        Scc::Mutual(items) => items.as_slice(),
    });

    for item in flattened.filter(is_instance) {
        term_item::check_instance(state, context, *item)?;
    }

    Ok(())
}

fn check_value_groups<Q: ExternalQueries>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()> {
    let is_value_group = |item: &&indexing::TermItemId| {
        matches!(context.indexed.items[**item].kind, indexing::TermItemKind::Value { .. })
    };

    let needs_binding_group = |item: &indexing::TermItemId| {
        let item = &context.indexed.items[*item];
        matches!(item.kind, indexing::TermItemKind::Value { signature: None, .. })
    };

    for scc in &context.lowered.term_scc {
        match scc {
            Scc::Base(item) | Scc::Recursive(item) if is_value_group(&item) => {
                if !state.checked.terms.contains_key(item) && needs_binding_group(item) {
                    state.term_binding_group(context, [*item]);
                }
                term_item::check_value_group(state, context, *item)?;
                state.commit_binding_group(context);
            }
            Scc::Mutual(items) => {
                let with_signature = items
                    .iter()
                    .filter(|item| is_value_group(item) && state.checked.terms.contains_key(item))
                    .copied()
                    .collect_vec();

                let without_signature = items
                    .iter()
                    .filter(|item| is_value_group(item) && needs_binding_group(item))
                    .copied()
                    .collect_vec();

                let group = without_signature.iter().copied();
                state.term_binding_group(context, group);

                for item in &without_signature {
                    term_item::check_value_group(state, context, *item)?;
                }
                state.commit_binding_group(context);

                for item in &with_signature {
                    term_item::check_value_group(state, context, *item)?;
                }
                state.commit_binding_group(context);
            }
            _ => {}
        }
    }
    Ok(())
}

pub fn check_prim(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
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
