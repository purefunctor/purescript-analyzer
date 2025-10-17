pub mod context;
pub mod level;

pub use context::*;

use crate::{
    check::CheckState,
    core::{Type, TypeId, TypeStorage},
};

pub fn unify<S>(state: &mut CheckState<S>, t1: TypeId, t2: TypeId) -> bool
where
    S: TypeStorage,
{
    let t1 = state.force_unification(t1);
    let t2 = state.force_unification(t2);
    match (state.storage.index(t1), state.storage.index(t2)) {
        (
            &Type::Application(t1_function, t1_argument),
            &Type::Application(t2_function, t2_argument),
        ) => unify(state, t1_function, t2_function) && unify(state, t1_argument, t2_argument),

        (
            &Type::Constructor(t1_file_id, t1_item_id),
            &Type::Constructor(t2_file_id, t2_item_id),
        ) => (t1_file_id, t1_item_id) == (t2_file_id, t2_item_id),

        (&Type::Unification(unification_id), _) => {
            let codomain = state.bound.size();
            state.solve(codomain, unification_id, t2).is_some()
        }

        (_, &Type::Unification(unification_id)) => {
            let codomain = state.bound.size();
            state.solve(codomain, unification_id, t1).is_some()
        }

        (_, _) => false,
    }
}
