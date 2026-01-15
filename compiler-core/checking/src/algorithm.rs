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

/// Implements type class deriving.
pub mod derive;

/// Implements type folding for traversals that modify.
pub mod fold;

/// Implements type visiting for read-only traversals.
pub mod visit;

/// Implements type signature inspection.
pub mod inspect;

/// Implements kind inference and checking for [`lowering::TypeKind`].
pub mod kind;

/// Implements surface-generic operator chain inference.
pub mod operator;

/// Implements generalisation for inferred types.
pub mod quantify;

/// Safety mechanisms for the type checker.
pub mod safety;

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
use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::TermItemKind;
use itertools::Itertools;
use lowering::{Scc, TermItemIr};

use crate::core::{Role, Type, TypeId};
use crate::{CheckedModule, ExternalQueries};

pub fn check_source(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let mut state = state::CheckState::default();
    let context = state::CheckContext::new(queries, &mut state, file_id)?;

    check_type_signatures(&mut state, &context)?;
    check_type_definitions(&mut state, &context)?;

    check_term_signatures(&mut state, &context)?;
    check_instance_heads(&mut state, &context)?;
    check_derive_heads(&mut state, &context)?;
    check_value_groups(&mut state, &context)?;
    check_instance_members(&mut state, &context)?;

    Ok(state.checked)
}

/// See [`type_item::check_type_signature`]
///
/// Kind signatures are acyclic, and can be checked separately from the
/// type definitions. Checking these early adds better information for
/// inference, especially for mutually recursive type declarations.
///
/// Consider the following example:
///
/// ```purescript
/// data F a = MkF (G a)
///
/// data G :: Int -> Type
/// data G a = MkG (F a)
/// ```
///
/// By checking the kind signature of `G` first, we can avoid allocating
/// a unification variable for `G` when checking the mutually recursive
/// declarations of `{F, G}`
fn check_type_signatures<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.type_scc {
        let items = match scc {
            Scc::Base(id) | Scc::Recursive(id) => slice::from_ref(id),
            Scc::Mutual(items) => items,
        };
        for id in items {
            type_item::check_type_signature(state, context, *id)?;
        }
    }
    Ok(())
}

/// See [`type_item::check_type_item`]
///
/// This function calls [`state::CheckState::with_type_group`] to insert
/// placeholder unification variables for recursive binding groups. After
/// checking a binding group, it calls [`type_item::commit_type_item`] to
/// generalise the types and add them to [`state::CheckState::checked`].
fn check_type_definitions<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.type_scc {
        match scc {
            Scc::Base(id) => {
                if let Some(item) = type_item::check_type_item(state, context, *id)? {
                    type_item::commit_type_item(state, context, *id, item)?;
                }
            }
            Scc::Recursive(id) => {
                state.with_type_group(context, [*id], |state| {
                    if let Some(item) = type_item::check_type_item(state, context, *id)? {
                        type_item::commit_type_item(state, context, *id, item)?;
                    }
                    Ok(())
                })?;
            }
            Scc::Mutual(mutual) => {
                state.with_type_group(context, mutual, |state| {
                    let mut items = vec![];
                    for &id in mutual {
                        if let Some(item) = type_item::check_type_item(state, context, id)? {
                            items.push((id, item));
                        }
                    }
                    for (id, item) in items {
                        type_item::commit_type_item(state, context, id, item)?;
                    }
                    Ok(())
                })?;
            }
        }
    }
    Ok(())
}

fn check_term_signatures<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for scc in &context.grouped.term_scc {
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

fn check_instance_heads<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let items = context.grouped.term_scc.iter().flat_map(|scc| match scc {
        Scc::Base(id) | Scc::Recursive(id) => slice::from_ref(id),
        Scc::Mutual(id) => id,
    });

    for &item_id in items {
        let Some(TermItemIr::Instance { constraints, resolution, arguments, .. }) =
            context.lowered.info.get_term_item(item_id)
        else {
            continue;
        };

        let check_instance =
            term_item::CheckInstance { item_id, constraints, arguments, resolution };

        term_item::check_instance(state, context, check_instance)?;
    }

    Ok(())
}

fn check_instance_members<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let items = context.grouped.term_scc.iter().flat_map(|scc| match scc {
        Scc::Base(id) | Scc::Recursive(id) => slice::from_ref(id),
        Scc::Mutual(id) => id,
    });

    for &item_id in items {
        let Some(TermItemIr::Instance { members, resolution, .. }) =
            context.lowered.info.get_term_item(item_id)
        else {
            continue;
        };

        let Some((class_file, class_id)) = *resolution else {
            continue;
        };

        let TermItemKind::Instance { id: instance_id } = context.indexed.items[item_id].kind else {
            continue;
        };

        let Some(instance) = state.checked.instances.get(&instance_id) else {
            continue;
        };

        let instance_arguments = instance.arguments.clone();
        let instance_constraints = instance.constraints.clone();
        let kind_variables = instance.kind_variables.clone();

        let check_members = term_item::CheckInstanceMembers {
            instance_id: item_id,
            members,
            class_file,
            class_id,
            instance_arguments: &instance_arguments,
            instance_constraints: &instance_constraints,
            kind_variables: &kind_variables,
        };

        term_item::check_instance_members(state, context, check_members)?;
    }

    Ok(())
}

fn check_derive_heads<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let items = context.grouped.term_scc.iter().flat_map(|scc| match scc {
        Scc::Base(item) | Scc::Recursive(item) => slice::from_ref(item),
        Scc::Mutual(items) => items.as_slice(),
    });

    for &item_id in items {
        let Some(TermItemIr::Derive { newtype, constraints, arguments, resolution }) =
            context.lowered.info.get_term_item(item_id)
        else {
            continue;
        };

        let Some((class_file, class_id)) = *resolution else {
            continue;
        };

        let TermItemKind::Derive { id: derive_id } = context.indexed.items[item_id].kind else {
            continue;
        };

        let check_derive = derive::CheckDerive {
            item_id,
            derive_id,
            constraints,
            arguments,
            class_file,
            class_id,
            is_newtype: *newtype,
        };

        derive::check_derive(state, context, check_derive)?;
    }

    Ok(())
}

fn check_value_groups<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let extract_value_group = |item_id: indexing::TermItemId| {
        let TermItemIr::ValueGroup { signature, equations } =
            context.lowered.info.get_term_item(item_id)?
        else {
            return None;
        };
        Some(term_item::CheckValueGroup { item_id, signature, equations })
    };

    for scc in &context.grouped.term_scc {
        match scc {
            Scc::Base(id) | Scc::Recursive(id) => {
                let Some(value_group) = extract_value_group(*id) else {
                    continue;
                };
                state.with_term_group(context, [*id], |state| {
                    if let Some(item) = term_item::check_value_group(state, context, value_group)? {
                        term_item::commit_value_group(state, context, *id, item)?;
                    }
                    Ok(())
                })?;
            }
            Scc::Mutual(mutual) => {
                let value_groups =
                    mutual.iter().filter_map(|&id| extract_value_group(id)).collect_vec();

                let with_signature = value_groups
                    .iter()
                    .filter(|value_group| value_group.signature.is_some())
                    .copied()
                    .collect_vec();

                let without_signature = value_groups
                    .iter()
                    .filter(|value_group| value_group.signature.is_none())
                    .copied()
                    .collect_vec();

                let group = without_signature.iter().map(|value_group| value_group.item_id);
                state.with_term_group(context, group, |state| {
                    let mut groups = vec![];
                    for value_group in &without_signature {
                        if let Some(group) =
                            term_item::check_value_group(state, context, *value_group)?
                        {
                            groups.push((value_group.item_id, group));
                        }
                    }
                    for (item_id, group) in groups {
                        term_item::commit_value_group(state, context, item_id, group)?;
                    }
                    Ok(())
                })?;

                for value_group in with_signature {
                    term_item::check_value_group(state, context, value_group)?;
                }
            }
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

    let mut insert_roles = |name: &str, roles: &[Role]| {
        let (_, item_id) = lookup_type(name);
        checked_module.roles.insert(item_id, Arc::from(roles));
    };

    insert_roles("Type", &[]);
    insert_roles("Function", &[Role::Representational, Role::Representational]);
    insert_roles("Array", &[Role::Representational]);
    insert_roles("Record", &[Role::Representational]);
    insert_roles("Number", &[]);
    insert_roles("Int", &[]);
    insert_roles("String", &[]);
    insert_roles("Char", &[]);
    insert_roles("Boolean", &[]);
    insert_roles("Constraint", &[]);
    insert_roles("Symbol", &[]);
    insert_roles("Row", &[Role::Representational]);

    Ok(checked_module)
}
