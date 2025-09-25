pub mod bracketing;

use std::sync::Arc;

use building_types::QueryResult;
use files::FileId;
use indexing::IndexedModule;
use lowering::LoweredModule;
use resolving::ResolvedModule;

pub trait External {
    fn indexed(&self, id: FileId) -> QueryResult<Arc<IndexedModule>>;

    fn resolved(&self, id: FileId) -> QueryResult<Arc<ResolvedModule>>;

    fn lowered(&self, id: FileId) -> QueryResult<Arc<LoweredModule>>;

    fn prim_id(&self) -> FileId;
}
