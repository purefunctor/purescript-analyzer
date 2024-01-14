//! Queries pertaining to a useful mappings.

pub mod module;
pub mod nominal;
pub mod positional;

use std::sync::Arc;

use files::FileId;

use crate::{InternerDatabase, SourceDatabase};

pub use module::ModuleMap;
pub use nominal::NominalMap;
pub use positional::PositionalMap;

#[salsa::query_group(IndexStorage)]
pub trait IndexDatabase: SourceDatabase + InternerDatabase {
    #[salsa::invoke(positional::positional_map_query)]
    fn positional_map(&self, file_id: FileId) -> Arc<PositionalMap>;

    #[salsa::invoke(nominal::nominal_map_query)]
    fn nominal_map(&self, file_id: FileId) -> Arc<NominalMap>;

    #[salsa::invoke(module::module_map_query)]
    fn module_map(&self) -> Arc<ModuleMap>;
}
