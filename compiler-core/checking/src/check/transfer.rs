use crate::{
    ExternalQueries, Type, TypeId,
    check::{CheckContext, CheckState},
};

pub fn localize<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> TypeId
where
    Q: ExternalQueries,
{
    traverse::<Q, Localize>(state, context, id)
}

pub fn globalize<Q>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> TypeId
where
    Q: ExternalQueries,
{
    traverse::<Q, Globalize>(state, context, id)
}

struct Localize;
struct Globalize;

trait TraversalExt<Q: ExternalQueries> {
    fn intern(state: &mut CheckState, context: &CheckContext<Q>, ty: Type) -> TypeId;
    fn lookup(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> Type;
}

impl<Q> TraversalExt<Q> for Localize
where
    Q: ExternalQueries,
{
    fn intern(state: &mut CheckState, _context: &CheckContext<Q>, ty: Type) -> TypeId {
        state.storage.intern(ty)
    }

    fn lookup(_state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> Type {
        context.queries.lookup_type(id)
    }
}

impl<Q> TraversalExt<Q> for Globalize
where
    Q: ExternalQueries,
{
    fn intern(_state: &mut CheckState, context: &CheckContext<Q>, ty: Type) -> TypeId {
        context.queries.intern_type(ty)
    }

    fn lookup(state: &mut CheckState, _context: &CheckContext<Q>, id: TypeId) -> Type {
        let id = state.normalize_type(id);
        state.storage[id].clone()
    }
}

fn traverse<Q, E>(state: &mut CheckState, context: &CheckContext<Q>, id: TypeId) -> TypeId
where
    Q: ExternalQueries,
    E: TraversalExt<Q>,
{
    let ty = match E::lookup(state, context, id) {
        Type::Application(function, argument) => {
            let function = traverse::<Q, E>(state, context, function);
            let argument = traverse::<Q, E>(state, context, argument);
            Type::Application(function, argument)
        }

        Type::Constructor(file_id, item_id) => Type::Constructor(file_id, item_id),

        Type::Forall(binder, inner) => {
            let mut binder = binder.clone();

            binder.kind = traverse::<Q, E>(state, context, binder.kind);
            let inner = traverse::<Q, E>(state, context, inner);

            Type::Forall(binder, inner)
        }

        Type::Function(argument, result) => {
            let argument = traverse::<Q, E>(state, context, argument);
            let result = traverse::<Q, E>(state, context, result);
            Type::Function(argument, result)
        }

        Type::KindApplication(function, argument) => {
            let function = traverse::<Q, E>(state, context, function);
            let argument = traverse::<Q, E>(state, context, argument);
            Type::KindApplication(function, argument)
        }

        Type::Unification(_) => {
            unreachable!("invariant violated: unification variable in global type")
        }

        Type::Variable(variable) => Type::Variable(variable),

        Type::Unknown => Type::Unknown,
    };
    E::intern(state, context, ty)
}
