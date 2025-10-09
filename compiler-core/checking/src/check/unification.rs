pub mod context;
pub mod pattern;

use std::sync::Arc;

pub use context::*;
pub use pattern::*;

use crate::{
    check::CheckState,
    core::{Type, TypeId, TypeStorage},
};

pub fn unify<S>(state: &mut CheckState<S>, t1: TypeId, t2: TypeId) -> bool
where
    S: TypeStorage,
{
    let t1 = state.normalize(t1);
    let t2 = state.normalize(t2);

    match (state.storage.index(t1), state.storage.index(t2)) {
        (
            &Type::Application(t1_function, t1_argument),
            &Type::Application(t2_function, t2_argument),
        ) => unify(state, t1_function, t2_function) && unify(state, t1_argument, t2_argument),

        (
            &Type::Constructor(t1_file_id, t1_item_id),
            &Type::Constructor(t2_file_id, t2_item_id),
        ) => (t1_file_id, t1_item_id) == (t2_file_id, t2_item_id),

        (&Type::Unification(unification, ref spine), _) => {
            let spine = Arc::clone(spine);
            let codomain = state.bound.level();
            state.solve(codomain, unification, &spine, t2).is_some()
        }

        (_, &Type::Unification(unification, ref spine)) => {
            let spine = Arc::clone(spine);
            let codomain = state.bound.level();
            state.solve(codomain, unification, &spine, t1).is_some()
        }

        (_, _) => false,
    }
}
