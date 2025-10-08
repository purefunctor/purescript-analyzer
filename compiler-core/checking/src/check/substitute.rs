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
    with_t: TypeId,
    t: TypeId,
) -> TypeId
where
    S: TypeStorage,
{
    match state.storage.index(t) {
        Type::Variable(Variable::Bound(i)) if *i == index => with_t,

        Type::Application(function, argument) => {
            let function = *function;
            let argument = *argument;

            let function = substitute_index(state, index, with_t, function);
            let argument = substitute_index(state, index, with_t, argument);

            state.storage.intern(Type::Application(function, argument))
        }

        Type::Function(argument, result) => {
            let argument = *argument;
            let result = *result;

            let argument = substitute_index(state, index, with_t, argument);
            let result = substitute_index(state, index, with_t, result);

            state.storage.intern(Type::Function(argument, result))
        }

        Type::Forall(binder, inner) => {
            let mut binder = binder.clone();
            let inner = *inner;

            binder.kind = substitute_index(state, index, with_t, binder.kind);
            let inner = substitute_index(state, index.increment(), with_t, inner);

            state.storage.intern(Type::Forall(binder, inner))
        }

        Type::KindApplication(function, argument) => {
            let function = *function;
            let argument = *argument;

            let function = substitute_index(state, index, with_t, function);
            let argument = substitute_index(state, index, with_t, argument);

            state.storage.intern(Type::KindApplication(function, argument))
        }

        Type::Lambda(body) => {
            let body = *body;

            let body = substitute_index(state, index, with_t, body);

            state.storage.intern(Type::Lambda(body))
        }

        Type::Constructor(_, _)
        | Type::Pruning(_, _)
        | Type::Unification(_, _)
        | Type::Variable(_)
        | Type::Unknown => t,
    }
}
