use crate::{
    ExternalQueries, Type, TypeId,
    check::{CheckContext, CheckState},
};

pub fn localize<Q>(state: &mut CheckState, context: &CheckContext<Q>, global_id: TypeId) -> TypeId
where
    Q: ExternalQueries,
{
    let global_type = context.queries.lookup_type(global_id);

    let local_type = match global_type {
        Type::Application(function, argument) => {
            let function = localize(state, context, function);
            let argument = localize(state, context, argument);
            Type::Application(function, argument)
        }

        Type::Constructor(file_id, item_id) => Type::Constructor(file_id, item_id),

        Type::Forall(binder, inner) => {
            let mut binder = binder.clone();

            binder.kind = localize(state, context, binder.kind);
            let inner = localize(state, context, inner);

            Type::Forall(binder, inner)
        }

        Type::Function(argument, result) => {
            let argument = localize(state, context, argument);
            let result = localize(state, context, result);
            Type::Function(argument, result)
        }

        Type::KindApplication(function, argument) => {
            let function = localize(state, context, function);
            let argument = localize(state, context, argument);
            Type::KindApplication(function, argument)
        }

        Type::Variable(variable) => Type::Variable(variable),

        Type::Unknown => Type::Unknown,

        Type::Unification(_) => {
            unreachable!("invariant violated: unification variable in global type")
        }
    };

    state.storage.intern(local_type)
}
