//! Queries pertaining to the high-level AST.

mod lower;
pub mod tree;
pub mod visit;

use std::sync::Arc;

use files::FileId;

pub use tree::*;
pub use visit::*;

use analyzer_index::IndexDatabase;
use analyzer_interner::InternerDatabase;
use analyzer_source::SourceDatabase;

#[salsa::query_group(SurfaceStorage)]
pub trait SurfaceDatabase: IndexDatabase + InternerDatabase + SourceDatabase {
    #[salsa::invoke(lower::file_surface_query)]
    fn file_surface(&self, file_id: FileId) -> (Arc<Module>, Arc<SurfaceArena>);

    #[salsa::invoke(lower::file_surface_map_query)]
    fn file_surface_map(&self, file_id: FileId)
        -> (Arc<Module>, Arc<SurfaceArena>, Arc<SourceMap>);
}
