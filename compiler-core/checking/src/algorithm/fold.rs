use std::sync::Arc;

use crate::algorithm::state::CheckState;
use crate::core::{ForallBinder, RowType, Type, TypeId};

/// Controls behavior during type folding.
pub enum FoldAction {
    /// Replace this node entirely (skip recursion)
    Replace(TypeId),
    /// Continue with default recursion
    Continue,
}

/// Trait for implementing type transformations.
pub trait TypeFold {
    /// Called before recursing into a type. Return `Replace(id)` to short-circuit.
    fn transform(&mut self, state: &mut CheckState, id: TypeId, t: &Type) -> FoldAction;

    /// Called when visiting a Forall binder. Override to modify binder fields.
    fn transform_binder(&mut self, _binder: &mut ForallBinder) {}
}

/// Recursively fold over a type, applying the given transformation.
pub fn fold_type<F: TypeFold>(state: &mut CheckState, id: TypeId, folder: &mut F) -> TypeId {
    let id = state.normalize_type(id);
    let t = state.storage[id].clone();

    // Check if transform wants to replace this node
    if let FoldAction::Replace(id) = folder.transform(state, id, &t) {
        return id;
    }

    match t {
        Type::Application(function, argument) => {
            let function = fold_type(state, function, folder);
            let argument = fold_type(state, argument, folder);
            state.storage.intern(Type::Application(function, argument))
        }
        Type::Constrained(constraint, inner) => {
            let constraint = fold_type(state, constraint, folder);
            let inner = fold_type(state, inner, folder);
            state.storage.intern(Type::Constrained(constraint, inner))
        }
        Type::Constructor(_, _) => id,
        Type::Forall(mut binder, inner) => {
            folder.transform_binder(&mut binder);
            binder.kind = fold_type(state, binder.kind, folder);
            let inner = fold_type(state, inner, folder);
            state.storage.intern(Type::Forall(binder, inner))
        }
        Type::Function(argument, result) => {
            let argument = fold_type(state, argument, folder);
            let result = fold_type(state, result, folder);
            state.storage.intern(Type::Function(argument, result))
        }
        Type::Integer(_) => id,
        Type::KindApplication(function, argument) => {
            let function = fold_type(state, function, folder);
            let argument = fold_type(state, argument, folder);
            state.storage.intern(Type::KindApplication(function, argument))
        }
        Type::Kinded(inner, kind) => {
            let inner = fold_type(state, inner, folder);
            let kind = fold_type(state, kind, folder);
            state.storage.intern(Type::Kinded(inner, kind))
        }
        Type::Operator(_, _) => id,
        Type::OperatorApplication(file_id, type_id, left, right) => {
            let left = fold_type(state, left, folder);
            let right = fold_type(state, right, folder);
            state.storage.intern(Type::OperatorApplication(file_id, type_id, left, right))
        }
        Type::Row(RowType { fields, tail }) => {
            let mut fields = fields.to_vec();
            fields.iter_mut().for_each(|field| field.id = fold_type(state, field.id, folder));
            let tail = tail.map(|tail| fold_type(state, tail, folder));
            let row = RowType { fields: Arc::from(fields), tail };
            state.storage.intern(Type::Row(row))
        }
        Type::String(_, _) => id,
        Type::SynonymApplication(saturation, file_id, type_id, arguments) => {
            let arguments =
                arguments.iter().map(|&argument| fold_type(state, argument, folder)).collect();
            state.storage.intern(Type::SynonymApplication(saturation, file_id, type_id, arguments))
        }
        Type::Unification(_) => id,
        Type::Variable(_) => id,
        Type::Unknown => id,
    }
}
