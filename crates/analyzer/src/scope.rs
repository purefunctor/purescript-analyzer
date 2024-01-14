//! Database for scope information.
mod collect;
mod data;
mod resolve;

use crate::{
    id::InFile,
    resolver::{DataGroupId, ValueGroupId},
    SurfaceDatabase,
};
use std::sync::Arc;

use collect::CollectContext;
pub use data::*;
use resolve::ResolveContext;

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: SurfaceDatabase {
    #[salsa::invoke(CollectContext::value_scope_query)]
    fn value_scope(&self, id: InFile<ValueGroupId>) -> Arc<WithScope<ValueGroupScope>>;

    #[salsa::invoke(ResolveContext::data_resolutions_query)]
    fn data_resolutions(&self, id: InFile<DataGroupId>) -> Arc<Resolutions>;

    #[salsa::invoke(ResolveContext::value_resolutions_query)]
    fn value_resolutions(&self, id: InFile<ValueGroupId>) -> Arc<Resolutions>;
}
