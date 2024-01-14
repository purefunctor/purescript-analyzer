//! Queries pertaining to the high-level AST.

mod lower;
pub mod tree;

use std::sync::Arc;

use files::FileId;

pub use tree::*;

use crate::{IndexDatabase, InternerDatabase, SourceDatabase};

#[salsa::query_group(SurfaceStorage)]
pub trait SurfaceDatabase: IndexDatabase + InternerDatabase + SourceDatabase {
    #[salsa::invoke(lower::file_surface_query)]
    fn file_surface(&self, file_id: FileId) -> (Arc<Module>, Arc<SurfaceArena>);
}
