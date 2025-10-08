pub mod context;
pub mod pattern;

pub use context::*;
pub use pattern::*;

use crate::{
    check::CheckState,
    core::{TypeId, TypeStorage},
};

pub fn unify<S: TypeStorage>(_state: &mut CheckState<S>, _t1: TypeId, _t2: TypeId) {}
