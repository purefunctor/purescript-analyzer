pub mod bracketing;

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::FullIndexedModule;
use lowering::FullLoweredModule;
use resolving::FullResolvedModule;

pub trait External {
    fn indexed(&self, id: FileId) -> QueryResult<Arc<FullIndexedModule>>;

    fn resolved(&self, id: FileId) -> QueryResult<Arc<FullResolvedModule>>;

    fn lowered(&self, id: FileId) -> QueryResult<Arc<FullLoweredModule>>;

    fn prim_id(&self) -> FileId;
}
