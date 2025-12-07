//! Operator-related kind checking.
//!
//! This module handles the kind checking of type operators, including:
//! - Inferring kinds for operator chains after bracketing
//! - Traversing operator trees and checking argument kinds
//! - Instantiating kind-polymorphic operators

use files::FileId;
use indexing::TypeItemId;
use sugar::OperatorTree;

use crate::ExternalQueries;
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::{kind, substitute, unification};
use crate::core::{ForallBinder, Type, TypeId};

#[derive(Copy, Clone, Debug)]
enum OperatorKindMode {
    Infer,
    Check { expected_kind: TypeId },
}

pub fn infer_operator_chain_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let unknown = (context.prim.unknown, context.prim.unknown);

    let Ok(bracketed) = context.queries.bracketed(context.id) else {
        return unknown;
    };

    let Some(operator_tree) = bracketed.types.get(&id) else {
        return unknown;
    };

    let Ok(operator_tree) = operator_tree else {
        return unknown;
    };

    operator_tree_kind(state, context, operator_tree, OperatorKindMode::Infer)
}

fn operator_tree_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    operator_tree: &OperatorTree<lowering::TypeId>,
    mode: OperatorKindMode,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    match operator_tree {
        OperatorTree::Leaf(None) => match mode {
            OperatorKindMode::Infer => (context.prim.unknown, context.prim.unknown),
            OperatorKindMode::Check { expected_kind } => (context.prim.unknown, expected_kind),
        },

        OperatorTree::Leaf(Some(type_id)) => match mode {
            OperatorKindMode::Infer => kind::infer_surface_kind(state, context, *type_id),
            OperatorKindMode::Check { expected_kind } => {
                kind::check_surface_kind(state, context, *type_id, expected_kind)
            }
        },

        OperatorTree::Branch(operator_id, children) => {
            let Some((file_id, type_item_id)) =
                context.lowered.info.get_type_operator(*operator_id)
            else {
                return match mode {
                    OperatorKindMode::Infer => (context.prim.unknown, context.prim.unknown),
                    OperatorKindMode::Check { expected_kind } => {
                        (context.prim.unknown, expected_kind)
                    }
                };
            };

            let operator_kind = kind::lookup_file_type(state, context, file_id, type_item_id)
                .unwrap_or(context.prim.unknown);

            operator_branch_kind(
                state,
                context,
                file_id,
                type_item_id,
                operator_kind,
                children,
                mode,
            )
        }
    }
}

fn operator_branch_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_item_id: TypeItemId,
    operator_kind: TypeId,
    children: &[OperatorTree<lowering::TypeId>; 2],
    mode: OperatorKindMode,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let unknown = match mode {
        OperatorKindMode::Infer => (context.prim.unknown, context.prim.unknown),
        OperatorKindMode::Check { expected_kind } => (context.prim.unknown, expected_kind),
    };

    let operator_kind = instantiate_kind_foralls(state, operator_kind);
    let operator_kind = state.normalize_type(operator_kind);

    let Type::Function(left_kind, operator_kind) = state.storage[operator_kind] else {
        return unknown;
    };

    let operator_kind = state.normalize_type(operator_kind);
    let Type::Function(right_kind, result_kind) = state.storage[operator_kind] else {
        return unknown;
    };

    if let OperatorKindMode::Check { expected_kind } = mode {
        unification::subsumes(state, context, result_kind, expected_kind);
    }

    let [left_tree, right_tree] = children;

    let (left_type, _) = operator_tree_kind(
        state,
        context,
        left_tree,
        OperatorKindMode::Check { expected_kind: left_kind },
    );

    let (right_type, _) = operator_tree_kind(
        state,
        context,
        right_tree,
        OperatorKindMode::Check { expected_kind: right_kind },
    );

    let t = state.storage.intern(Type::OperatorApplication(
        file_id,
        type_item_id,
        left_type,
        right_type,
    ));

    let k = state.normalize_type(result_kind);

    (t, k)
}

pub fn elaborate_operator_application_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_id: TypeItemId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let operator_kind =
        kind::lookup_file_type(state, context, file_id, type_id).unwrap_or(context.prim.unknown);

    let operator_kind = instantiate_kind_foralls(state, operator_kind);

    let operator_kind = state.normalize_type(operator_kind);
    let Type::Function(_, operator_kind) = state.storage[operator_kind] else {
        return context.prim.unknown;
    };

    let operator_kind = state.normalize_type(operator_kind);
    let Type::Function(_, result_kind) = state.storage[operator_kind] else {
        return context.prim.unknown;
    };

    result_kind
}

fn instantiate_kind_foralls(state: &mut CheckState, mut kind_id: TypeId) -> TypeId {
    loop {
        kind_id = state.normalize_type(kind_id);
        if let Type::Forall(ForallBinder { kind, .. }, inner) = state.storage[kind_id] {
            let unification = state.fresh_unification_kinded(kind);
            kind_id = substitute::substitute_bound(state, unification, inner);
        } else {
            break kind_id;
        }
    }
}
