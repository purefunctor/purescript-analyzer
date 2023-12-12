//! Database for lowering the CST.

mod lower;
mod source_map;
mod trees;

use std::sync::Arc;

use la_arena::Arena;
use lower::SurfaceContext;
pub use source_map::*;
use syntax::ast;
pub use trees::*;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    ResolverDatabase, SourceDatabase,
};

#[salsa::query_group(SurfaceStorage)]
pub trait SurfaceDatabase: SourceDatabase + ResolverDatabase {
    #[salsa::invoke(lower::data_surface_query)]
    fn data_surface(
        &self,
        id: InFile<AstId<ast::DataDeclaration>>,
    ) -> (Arc<Arena<Type>>, Arc<DataGroup>);

    #[salsa::invoke(SurfaceContext::value_surface_query)]
    fn value_surface(&self, id: InFile<ValueGroupId>) -> Arc<WithArena<ValueGroup>>;

    #[salsa::invoke(SurfaceContext::value_surface_with_source_map_query)]
    fn value_surface_with_source_map(
        &self,
        id: InFile<ValueGroupId>,
    ) -> (Arc<WithArena<ValueGroup>>, Arc<SourceMap>);
}
