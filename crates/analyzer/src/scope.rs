//! Database for scope information.
mod collect;
mod data;

use crate::{id::InFile, resolver::ValueGroupId, SurfaceDatabase};
use std::sync::Arc;

use collect::CollectContext;
pub use data::*;

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: SurfaceDatabase {
    #[salsa::invoke(CollectContext::value_scope_query)]
    fn value_scope(&self, id: InFile<ValueGroupId>) -> Arc<WithScope<ValueGroupScope>>;
}
