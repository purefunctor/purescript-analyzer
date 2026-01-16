mod prim_coerce;
mod prim_int;
mod prim_reflectable;
mod prim_row;
mod prim_row_list;
mod prim_symbol;
mod prim_type_error;

pub use prim_coerce::*;
pub use prim_int::*;
pub use prim_reflectable::*;
pub use prim_row::*;
pub use prim_row_list::*;
pub use prim_symbol::*;
pub use prim_type_error::*;

use smol_str::SmolStr;

use crate::algorithm::state::CheckState;
use crate::{Type, TypeId};

pub(super) fn extract_integer(state: &CheckState, id: TypeId) -> Option<i32> {
    let Type::Integer(value) = state.storage[id] else { return None };
    Some(value)
}

pub(super) fn extract_symbol(state: &CheckState, id: TypeId) -> Option<SmolStr> {
    let Type::String(_, value) = &state.storage[id] else { return None };
    Some(SmolStr::clone(value))
}
