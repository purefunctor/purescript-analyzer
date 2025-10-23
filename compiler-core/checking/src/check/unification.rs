pub mod context;
pub mod level;

pub use context::*;

use crate::{
    ExternalQueries,
    check::{CheckContext, CheckState, kind},
    core::{Type, TypeId},
};

pub fn unify<Q>(state: &mut CheckState, context: &CheckContext<Q>, t1: TypeId, t2: TypeId) -> bool
where
    Q: ExternalQueries,
{
    let t1 = state.normalize_type(t1);
    let t2 = state.normalize_type(t2);
    match (&state.storage[t1], &state.storage[t2]) {
        (
            &Type::Application(t1_function, t1_argument),
            &Type::Application(t2_function, t2_argument),
        ) => {
            unify(state, context, t1_function, t2_function)
                && unify(state, context, t1_argument, t2_argument)
        }

        (Type::Constructor(t1_file_id, t1_item_id), Type::Constructor(t2_file_id, t2_item_id)) => {
            (t1_file_id, t1_item_id) == (t2_file_id, t2_item_id)
        }

        (&Type::Unification(unification_id), _) => {
            solve(state, context, unification_id, t2).is_some()
        }

        (_, &Type::Unification(unification_id)) => {
            solve(state, context, unification_id, t1).is_some()
        }

        _ => false,
    }
}

pub fn solve<Q>(
    state: &mut CheckState,
    context: &CheckContext<Q>,
    unification_id: u32,
    solution: TypeId,
) -> Option<u32>
where
    Q: ExternalQueries,
{
    let codomain = state.bound.size();
    let occurs = Some(unification_id);

    if !state.promote_type(occurs, codomain, unification_id, solution) {
        return None;
    }

    let unification_kind = state.unification.get(unification_id).kind;
    let solution_kind = kind::elaborate_kind(state, context, solution);
    unify(state, context, unification_kind, solution_kind);

    state.unification.solve(unification_id, solution);

    Some(unification_id)
}
