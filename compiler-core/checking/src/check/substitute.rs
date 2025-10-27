use crate::check::CheckState;
use crate::core::{Type, TypeId, Variable, debruijn};

pub fn substitute_bound(state: &mut CheckState, with_type: TypeId, in_type: TypeId) -> TypeId {
    fn aux(
        state: &mut CheckState,
        index: debruijn::Index,
        with_type: TypeId,
        in_type: TypeId,
    ) -> TypeId {
        match state.storage[in_type] {
            Type::Variable(Variable::Bound(bound)) if bound == index => with_type,

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
                let inner = aux(state, index.increment(), with_type, inner);

                state.storage.intern(Type::Forall(binder, inner))
            }

            Type::KindApplication(function, argument) => {
                let function = aux(state, index, with_type, function);
                let argument = aux(state, index, with_type, argument);
                state.storage.intern(Type::KindApplication(function, argument))
            }

            Type::Constructor(_, _) | Type::Unification(_) | Type::Variable(_) | Type::Unknown => {
                in_type
            }
        }
    }

    aux(state, debruijn::Index(0), with_type, in_type)
}
