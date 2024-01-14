//! Queries pertaining to a module's contents.

pub mod nominal;
pub mod positional;

use std::sync::Arc;

use files::FileId;

use crate::{InternerDatabase, SourceDatabase};

pub use positional::PositionalMap;

use self::nominal::NominalMap;

#[salsa::query_group(IndexStorage)]
pub trait IndexDatabase: SourceDatabase + InternerDatabase {
    #[salsa::invoke(positional::positional_map_query)]
    fn positional_map(&self, file_id: FileId) -> Arc<PositionalMap>;

    #[salsa::invoke(nominal::nominal_map_query)]
    fn nominal_map(&self, file_id: FileId) -> Arc<NominalMap>;
}
