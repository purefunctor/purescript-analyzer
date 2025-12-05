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
use crate::algorithm::kind::{infer_surface_kind, lookup_file_type};
use crate::algorithm::state::{CheckContext, CheckState};
use crate::algorithm::substitute::substitute_bound;
use crate::algorithm::unification;
use crate::core::{ForallBinder, Type, TypeId};

pub fn infer_operator_chain_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: lowering::TypeId,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let default = (context.prim.unknown, context.prim.unknown);

    let Ok(bracketed) = context.queries.bracketed(context.id) else {
        return default;
    };

    let Some(operator_tree) = bracketed.types.get(&id) else {
        return default;
    };

    if let Ok(operator_tree) = operator_tree {
        infer_operator_tree_kind(state, context, operator_tree)
    } else {
        default
    }
}

fn infer_operator_tree_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    tree: &OperatorTree<lowering::TypeId>,
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let default = (context.prim.unknown, context.prim.unknown);

    match tree {
        OperatorTree::Leaf(None) => default,

        OperatorTree::Leaf(Some(type_id)) => infer_surface_kind(state, context, *type_id),

        OperatorTree::Branch(operator_id, children) => {
            let Some((file_id, type_item_id)) =
                context.lowered.info.get_type_operator(*operator_id)
            else {
                return default;
            };

            let operator_k = lookup_file_type(state, context, file_id, type_item_id)
                .unwrap_or(context.prim.unknown);

            infer_operator_app_kind(state, context, file_id, type_item_id, operator_k, children)
        }
    }
}

fn infer_operator_app_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    file_id: FileId,
    type_item_id: TypeItemId,
    operator_k: TypeId,
    children: &[OperatorTree<lowering::TypeId>],
) -> (TypeId, TypeId)
where
    Q: ExternalQueries,
{
    let default = (context.prim.unknown, context.prim.unknown);

    let [left_tree, right_tree] = children else {
        return default;
    };

    let (left_t, _) = infer_operator_tree_kind(state, context, left_tree);
    let (right_t, _) = infer_operator_tree_kind(state, context, right_tree);

    let operator_k = instantiate_kind_foralls(state, operator_k);
    let operator_k = state.normalize_type(operator_k);

    let Type::Function(left_k, rest_k) = state.storage[operator_k] else {
        return default;
    };

    let left_k_elaborated = elaborate_operand_kind(state, context, left_t);
    unification::subsumes(state, context, left_k_elaborated, left_k);

    let rest_k = state.normalize_type(rest_k);
    let Type::Function(right_k, result_k) = state.storage[rest_k] else {
        return default;
    };

    let right_k_elaborated = elaborate_operand_kind(state, context, right_t);
    unification::subsumes(state, context, right_k_elaborated, right_k);

    let t = state.storage.intern(Type::OperatorApplication(file_id, type_item_id, left_t, right_t));
    let k = state.normalize_type(result_k);

    (t, k)
}

fn instantiate_kind_foralls(state: &mut CheckState, mut kind_id: TypeId) -> TypeId {
    loop {
        kind_id = state.normalize_type(kind_id);
        if let Type::Forall(ForallBinder { kind, .. }, inner) = state.storage[kind_id] {
            let unification = state.fresh_unification_kinded(kind);
            kind_id = substitute_bound(state, unification, inner);
        } else {
            break kind_id;
        }
    }
}

fn elaborate_operand_kind<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    id: TypeId,
) -> TypeId
where
    Q: ExternalQueries,
{
    let id = state.normalize_type(id);
    if let Type::OperatorApplication(file_id, type_id, _, _) = state.storage[id] {
        elaborate_operator_application_kind(state, context, file_id, type_id)
    } else {
        crate::algorithm::kind::elaborate_kind(state, context, id)
    }
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
    let default = context.prim.unknown;

    let operator_kind = lookup_file_type(state, context, file_id, type_id).unwrap_or(default);
    let operator_kind = instantiate_kind_foralls(state, operator_kind);

    let operator_kind = state.normalize_type(operator_kind);
    let Type::Function(_, result_id) = state.storage[operator_kind] else {
        return default;
    };

    let result_id = state.normalize_type(result_id);
    let Type::Function(_, result) = state.storage[result_id] else {
        return default;
    };

    result
}
