//! Implements the type inference routines for PureScript.

mod solve;
mod unify;
mod value;

use super::constraint::Constraint;

pub(crate) use value::infer_binding_group_query;

#[derive(Debug, Default)]
struct InferState {
    fresh_index: usize,
    constraints: Vec<Constraint>,
}
