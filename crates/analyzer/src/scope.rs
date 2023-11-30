//! Database for scope information.
mod collect;
mod data;
mod recursive;
mod resolve;

use crate::{id::InFile, resolver::ValueGroupId, SurfaceDatabase};
use std::sync::Arc;

use collect::CollectContext;
pub use data::*;
use recursive::RecursiveLetsContext;
use resolve::ResolveContext;

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: SurfaceDatabase {
    #[salsa::invoke(CollectContext::value_scope_query)]
    fn value_scope(&self, id: InFile<ValueGroupId>) -> Arc<WithScope<ValueGroupScope>>;

    #[salsa::invoke(ResolveContext::value_resolve_query)]
    fn value_resolved(&self, id: InFile<ValueGroupId>) -> Arc<ValueGroupResolutions>;

    #[salsa::invoke(RecursiveLetsContext::value_recursive_lets_query)]
    fn value_recursive_lets(&self, id: InFile<ValueGroupId>) -> Arc<ValueGroupRecursiveLets>;
}
