//! Database for syntax sugar.

mod group;

use std::sync::Arc;

use crate::{id::InFile, resolver::ValueGroupId, ScopeDatabase};

use files::FileId;

pub use group::{BindingGroup, BindingGroupId, BindingGroups, LetBindingGroups};
use group::{BindingGroupsContext, LetBindingGroupsContext};

#[salsa::query_group(SugarStorage)]
pub trait SugarDatabase: ScopeDatabase {
    #[salsa::invoke(BindingGroupsContext::binding_groups_query)]
    fn binding_groups(&self, file_id: FileId) -> Arc<BindingGroups>;

    #[salsa::invoke(LetBindingGroupsContext::let_binding_groups_query)]
    fn let_binding_groups(&self, id: InFile<ValueGroupId>) -> Arc<LetBindingGroups>;
}
