//! Implements type variable substitution.

use std::sync::Arc;

use crate::algorithm::state::CheckState;
use crate::core::{RowType, Type, TypeId, Variable, debruijn};

/// Substitutes a bound variable at a specific level with a replacement type.
///
/// Since levels are absolute positions, no scope tracking is needed -
/// we simply match on the target level directly.
pub fn substitute_bound(
    state: &mut CheckState,
    target_level: debruijn::Level,
    with_type: TypeId,
    in_type: TypeId,
) -> TypeId {
    fn aux(
        state: &mut CheckState,
        target: debruijn::Level,
        with_type: TypeId,
        in_type: TypeId,
    ) -> TypeId {
        match state.storage[in_type] {
            Type::Variable(Variable::Bound(level)) if level == target => with_type,

            Type::Application(function, argument) => {
                let function = aux(state, target, with_type, function);
                let argument = aux(state, target, with_type, argument);
                state.storage.intern(Type::Application(function, argument))
            }

            Type::Function(argument, result) => {
                let argument = aux(state, target, with_type, argument);
                let result = aux(state, target, with_type, result);
                state.storage.intern(Type::Function(argument, result))
            }

            Type::Forall(ref binder, inner) => {
                let mut binder = binder.clone();

                binder.kind = aux(state, target, with_type, binder.kind);
                let inner = aux(state, target, with_type, inner);

                state.storage.intern(Type::Forall(binder, inner))
            }

            Type::KindApplication(function, argument) => {
                let function = aux(state, target, with_type, function);
                let argument = aux(state, target, with_type, argument);
                state.storage.intern(Type::KindApplication(function, argument))
            }

            Type::Constrained(constraint, inner) => {
                let constraint = aux(state, target, with_type, constraint);
                let inner = aux(state, target, with_type, inner);
                state.storage.intern(Type::Constrained(constraint, inner))
            }

            Type::Kinded(inner, kind) => {
                let inner = aux(state, target, with_type, inner);
                let kind = aux(state, target, with_type, kind);
                state.storage.intern(Type::Kinded(inner, kind))
            }

            Type::OperatorApplication(file_id, type_id, left, right) => {
                let left = aux(state, target, with_type, left);
                let right = aux(state, target, with_type, right);
                state.storage.intern(Type::OperatorApplication(file_id, type_id, left, right))
            }

            Type::Row(RowType { ref fields, tail }) => {
                let mut fields = fields.to_vec();
                fields
                    .iter_mut()
                    .for_each(|field| field.id = aux(state, target, with_type, field.id));

                let tail = tail.map(|tail| aux(state, target, with_type, tail));
                let row = RowType { fields: Arc::from(fields), tail };

                state.storage.intern(Type::Row(row))
            }

            Type::SynonymApplication(saturation, file_id, type_id, ref arguments) => {
                let arguments = Arc::clone(arguments);
                let arguments = arguments
                    .iter()
                    .map(|&argument| aux(state, target, with_type, argument))
                    .collect();
                state
                    .storage
                    .intern(Type::SynonymApplication(saturation, file_id, type_id, arguments))
            }

            Type::Constructor(_, _)
            | Type::Integer(_)
            | Type::Operator(_, _)
            | Type::String(_, _)
            | Type::Unification(_)
            | Type::Variable(_)
            | Type::Unknown => in_type,
        }
    }

    aux(state, target_level, with_type, in_type)
}
