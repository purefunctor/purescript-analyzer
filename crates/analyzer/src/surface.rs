//! Database for lowering the CST.

mod lower;
mod source_map;
mod trees;

use std::sync::Arc;

use lower::SurfaceContext;
pub use source_map::*;
pub use trees::*;

use crate::{id::InFile, resolver::ValueGroupId, ResolverDatabase, SourceDatabase};

#[salsa::query_group(SurfaceStorage)]
pub trait SurfaceDatabase: SourceDatabase + ResolverDatabase {
    #[salsa::invoke(SurfaceContext::value_surface_query)]
    fn value_surface(&self, id: InFile<ValueGroupId>) -> Arc<WithArena<ValueGroup>>;

    #[salsa::invoke(SurfaceContext::value_surface_with_source_map_query)]
    fn value_surface_with_source_map(
        &self,
        id: InFile<ValueGroupId>,
    ) -> (Arc<WithArena<ValueGroup>>, Arc<SourceMap>);
}
