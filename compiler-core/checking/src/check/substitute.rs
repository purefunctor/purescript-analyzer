use crate::{
    check::CheckState,
    core::{Type, TypeId, TypeStorage, Variable, debruijn},
};

pub fn substitute_bound<S>(state: &mut CheckState<S>, with_t: TypeId, t: TypeId) -> TypeId
where
    S: TypeStorage,
{
    substitute_index(state, debruijn::Index(0), with_t, t)
}

fn substitute_index<S>(
    state: &mut CheckState<S>,
    index: debruijn::Index,
    with_type: TypeId,
    in_type: TypeId,
) -> TypeId
where
    S: TypeStorage,
{
    match *state.storage.index(in_type) {
        Type::Variable(Variable::Bound(bound)) if bound == index => with_type,

        Type::Application(function, argument) => {
            let function = substitute_index(state, index, with_type, function);
            let argument = substitute_index(state, index, with_type, argument);
            state.storage.intern(Type::Application(function, argument))
        }

        Type::Function(argument, result) => {
            let argument = substitute_index(state, index, with_type, argument);
            let result = substitute_index(state, index, with_type, result);
            state.storage.intern(Type::Function(argument, result))
        }

        Type::Forall(ref binder, inner) => {
            let mut binder = binder.clone();

            binder.kind = substitute_index(state, index, with_type, binder.kind);
            let inner = substitute_index(state, index.increment(), with_type, inner);

            state.storage.intern(Type::Forall(binder, inner))
        }

        Type::KindApplication(function, argument) => {
            let function = substitute_index(state, index, with_type, function);
            let argument = substitute_index(state, index, with_type, argument);
            state.storage.intern(Type::KindApplication(function, argument))
        }

        Type::Constructor(_, _) | Type::Unification(_) | Type::Variable(_) | Type::Unknown => {
            in_type
        }
    }
}
