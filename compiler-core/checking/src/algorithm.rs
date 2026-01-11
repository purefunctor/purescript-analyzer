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
use indexing::TermItemKind;
use itertools::Itertools;
use lowering::{DataIr, NewtypeIr, Scc, TermItemIr, TypeItemIr};

use crate::core::{ForallBinder, Type, TypeId, debruijn};
use crate::{CheckedModule, ExternalQueries};

pub fn check_source(queries: &impl ExternalQueries, file_id: FileId) -> QueryResult<CheckedModule> {
    let mut state = state::CheckState::default();
    let context = state::CheckContext::new(queries, &mut state, file_id)?;

    check_type_signatures(&mut state, &context)?;
    check_type_items(&mut state, &context)?;

    check_term_signatures(&mut state, &context)?;
    check_instance_heads(&mut state, &context)?;
    check_derive_heads(&mut state, &context)?;
    check_value_groups(&mut state, &context)?;
    check_instance_members(&mut state, &context)?;

    Ok(state.checked)
}

fn check_type_signatures<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
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

fn check_type_items<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let needs_binding_group = |id: &indexing::TypeItemId| {
        // Use the lowering representation to detypeine if we need a binding
        // group to match invariant expectations in downstream checking rules.
        // Previously, this used the indexing representation which would crash
        // on partially-specified kind signatures like `data Foo ::`.
        let Some(lowered) = context.lowered.info.get_type_item(*id) else {
            return false;
        };
        matches!(
            lowered,
            TypeItemIr::DataGroup { signature: None, .. }
                | TypeItemIr::NewtypeGroup { signature: None, .. }
                | TypeItemIr::SynonymGroup { signature: None, .. }
                | TypeItemIr::ClassGroup { signature: None, .. }
        )
    };

    for scc in &context.lowered.type_scc {
        match scc {
            Scc::Base(item) | Scc::Recursive(item) => {
                if !state.binding_group.types.contains_key(item) && needs_binding_group(item) {
                    state.type_binding_group(context, [*item]);
                }
                type_item::check_type_item(state, context, *item)?;

                build_data_constructor_types(state, context, slice::from_ref(item))?;
                state.commit_binding_group(context)?;
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

                build_data_constructor_types(state, context, &without_signature)?;
                state.commit_binding_group(context)?;

                for item in &with_signature {
                    type_item::check_type_item(state, context, *item)?;
                }

                build_data_constructor_types(state, context, &with_signature)?;
                state.commit_binding_group(context)?;
            }
        }
    }

    Ok(())
}

fn build_data_constructor_types<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
    items: &[indexing::TypeItemId],
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    for &item_id in items {
        let Some(data) = state.binding_group.data.remove(&item_id) else {
            continue;
        };

        let Some(item) = context.lowered.info.get_type_item(item_id) else {
            continue;
        };

        let variables = match item {
            TypeItemIr::DataGroup { data: Some(DataIr { variables }), .. } => variables,
            TypeItemIr::NewtypeGroup { newtype: Some(NewtypeIr { variables }), .. } => variables,
            _ => continue,
        };

        let kind_variables = data.kind_variables.iter();
        let surface_bindings = state.surface_bindings.get_type(item_id);
        let surface_bindings = surface_bindings.as_deref().unwrap_or_default();
        let mut surface_binding_iter = surface_bindings.iter();

        let kind_variables = kind_variables.map(|binder| {
            let level = if let Some(&binding_id) = surface_binding_iter.next() {
                state.type_scope.bound.bind(debruijn::Variable::Forall(binding_id))
            } else {
                state.type_scope.bound.bind(debruijn::Variable::Core)
            };
            state.type_scope.kinds.insert(level, binder.kind);
            ForallBinder {
                visible: binder.visible,
                name: binder.name.clone(),
                level,
                kind: binder.kind,
            }
        });

        let kind_variables = kind_variables.collect_vec();

        let type_variables = data.type_variables.iter();
        let surface_bindings = variables.iter();

        let type_variables = type_variables.zip(surface_bindings).map(|(binder, variable)| {
            let level = state.type_scope.bind_forall(variable.id, binder.kind);
            ForallBinder {
                visible: binder.visible,
                name: binder.name.clone(),
                level,
                kind: binder.kind,
            }
        });

        let type_variables = type_variables.collect_vec();

        type_item::build_constructor_types(
            state,
            context,
            item_id,
            &kind_variables,
            &type_variables,
            &data.constructors,
        )?;

        if let Some(variable) = type_variables.first() {
            state.type_scope.unbind(variable.level);
        }

        if let Some(variable) = kind_variables.first() {
            state.type_scope.unbind(variable.level);
        }

        state.binding_group.data.insert(item_id, data);
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

fn check_instance_heads<Q>(
    state: &mut state::CheckState,
    context: &state::CheckContext<Q>,
) -> QueryResult<()>
where
    Q: ExternalQueries,
{
    let items = context.lowered.term_scc.iter().flat_map(|scc| match scc {
        Scc::Base(item) | Scc::Recursive(item) => slice::from_ref(item),
        Scc::Mutual(items) => items.as_slice(),
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
    let items = context.lowered.term_scc.iter().flat_map(|scc| match scc {
        Scc::Base(item) | Scc::Recursive(item) => slice::from_ref(item),
        Scc::Mutual(items) => items.as_slice(),
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

        let check_members = term_item::CheckInstanceMembers {
            instance_id: item_id,
            members,
            class_file,
            class_id,
            instance_arguments: &instance_arguments,
            instance_constraints: &instance_constraints,
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
    let items = context.lowered.term_scc.iter().flat_map(|scc| match scc {
        Scc::Base(item) | Scc::Recursive(item) => slice::from_ref(item),
        Scc::Mutual(items) => items.as_slice(),
    });

    for &item_id in items {
        let Some(TermItemIr::Derive { constraints, arguments, resolution, .. }) =
            context.lowered.info.get_term_item(item_id)
        else {
            continue;
        };

        let check_derive = term_item::CheckDerive { item_id, constraints, arguments, resolution };

        term_item::check_derive(state, context, check_derive)?;
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

    for scc in &context.lowered.term_scc {
        match scc {
            Scc::Base(item) | Scc::Recursive(item) => {
                let Some(value_group) = extract_value_group(*item) else {
                    continue;
                };
                if !state.checked.terms.contains_key(item) && value_group.signature.is_none() {
                    state.term_binding_group(context, [*item]);
                }
                term_item::check_value_group(state, context, value_group)?;
                state.commit_binding_group(context)?;
            }
            Scc::Mutual(items) => {
                let value_groups =
                    items.iter().filter_map(|&id| extract_value_group(id)).collect_vec();

                let with_signature = value_groups
                    .iter()
                    .filter(|value_group| state.checked.terms.contains_key(&value_group.item_id))
                    .collect_vec();

                let without_signature = value_groups
                    .iter()
                    .filter(|value_group| value_group.signature.is_none())
                    .collect_vec();

                let group = without_signature.iter().map(|value_group| value_group.item_id);
                state.term_binding_group(context, group);

                for value_group in without_signature {
                    term_item::check_value_group(state, context, *value_group)?;
                }
                state.commit_binding_group(context)?;

                for value_group in with_signature {
                    term_item::check_value_group(state, context, *value_group)?;
                }
                state.commit_binding_group(context)?;
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

    Ok(checked_module)
}
