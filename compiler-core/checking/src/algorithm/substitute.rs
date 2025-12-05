//! Implements type variable substitution.

use crate::algorithm::state::CheckState;
use crate::core::{Type, TypeId, Variable, debruijn};

pub fn substitute_bound(state: &mut CheckState, with_type: TypeId, in_type: TypeId) -> TypeId {
    fn aux(state: &mut CheckState, index: u32, with_type: TypeId, in_type: TypeId) -> TypeId {
        match state.storage[in_type] {
            Type::Variable(Variable::Bound(bound)) if bound.0 == index => with_type,

            Type::Application(function, argument) => {
                let function = aux(state, index, with_type, function);
                let argument = aux(state, index, with_type, argument);
                state.storage.intern(Type::Application(function, argument))
            }

            Type::Function(argument, result) => {
                let argument = aux(state, index, with_type, argument);
                let result = aux(state, index, with_type, result);
                state.storage.intern(Type::Function(argument, result))
            }

            Type::Forall(ref binder, inner) => {
                let mut binder = binder.clone();

                binder.kind = aux(state, index, with_type, binder.kind);
                let inner = aux(state, index + 1, with_type, inner);

                state.storage.intern(Type::Forall(binder, inner))
            }

            Type::KindApplication(function, argument) => {
                let function = aux(state, index, with_type, function);
                let argument = aux(state, index, with_type, argument);
                state.storage.intern(Type::KindApplication(function, argument))
            }

            Type::Constrained(constraint, inner) => {
                let constraint = aux(state, index, with_type, constraint);
                let inner = aux(state, index, with_type, inner);
                state.storage.intern(Type::Constrained(constraint, inner))
            }

            Type::Kinded(inner, kind) => {
                let inner = aux(state, index, with_type, inner);
                let kind = aux(state, index, with_type, kind);
                state.storage.intern(Type::Kinded(inner, kind))
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

    aux(state, 0, with_type, in_type)
}

pub fn shift_indices(state: &mut CheckState, amount: u32, in_type: TypeId) -> TypeId {
    fn aux(state: &mut CheckState, cutoff: u32, amount: u32, in_type: TypeId) -> TypeId {
        match state.storage[in_type] {
            Type::Variable(Variable::Bound(index)) if index.0 >= cutoff => {
                let shifted = debruijn::Index(index.0 + amount);
                state.storage.intern(Type::Variable(Variable::Bound(shifted)))
            }

            Type::Application(function, argument) => {
                let function = aux(state, cutoff, amount, function);
                let argument = aux(state, cutoff, amount, argument);
                state.storage.intern(Type::Application(function, argument))
            }

            Type::Function(argument, result) => {
                let argument = aux(state, cutoff, amount, argument);
                let result = aux(state, cutoff, amount, result);
                state.storage.intern(Type::Function(argument, result))
            }

            Type::Forall(ref binder, inner) => {
                let mut binder = binder.clone();

                binder.kind = aux(state, cutoff, amount, binder.kind);
                let inner = aux(state, cutoff + 1, amount, inner);

                state.storage.intern(Type::Forall(binder, inner))
            }

            Type::KindApplication(function, argument) => {
                let function = aux(state, cutoff, amount, function);
                let argument = aux(state, cutoff, amount, argument);
                state.storage.intern(Type::KindApplication(function, argument))
            }

            Type::Constrained(constraint, inner) => {
                let constraint = aux(state, cutoff, amount, constraint);
                let inner = aux(state, cutoff, amount, inner);
                state.storage.intern(Type::Constrained(constraint, inner))
            }

            Type::Kinded(inner, kind) => {
                let inner = aux(state, cutoff, amount, inner);
                let kind = aux(state, cutoff, amount, kind);
                state.storage.intern(Type::Kinded(inner, kind))
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

    aux(state, 0, amount, in_type)
}
