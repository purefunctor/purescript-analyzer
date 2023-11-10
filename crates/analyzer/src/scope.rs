//! Database for scope information.
mod data;

pub use data::*;

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase {}
