//! Database for scope information.
mod collect;
mod data;
mod recursive;
mod resolve;

use crate::{id::InFile, resolver::ValueGroupId, SurfaceDatabase};
use std::sync::Arc;

use collect::CollectContext;
pub use data::*;
use files::FileId;
use recursive::{BindingGroupsContext, LetBindingGroupsContext};
use resolve::ResolveContext;

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: SurfaceDatabase {
    #[salsa::invoke(CollectContext::value_scope_query)]
    fn value_scope(&self, id: InFile<ValueGroupId>) -> Arc<WithScope<ValueGroupScope>>;

    #[salsa::invoke(ResolveContext::value_resolve_query)]
    fn value_resolved(&self, id: InFile<ValueGroupId>) -> Arc<ValueGroupResolutions>;

    #[salsa::invoke(BindingGroupsContext::binding_groups_query)]
    fn binding_groups(&self, file_id: FileId) -> Arc<BindingGroups>;

    #[salsa::invoke(LetBindingGroupsContext::let_binding_groups_qiery)]
    fn let_binding_groups(&self, id: InFile<ValueGroupId>) -> Arc<LetBindingGroups>;
}
