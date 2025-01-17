mod algorithm;
mod error;
mod id;
mod indexes;
mod sourcemap;

use std::sync::Arc;

pub use error::*;
pub use id::*;
pub use indexes::*;
pub use sourcemap::*;

use syntax::cst;

pub struct IndexingResult {
    pub source_map: SourceMap,
    pub nominal: NominalIndex,
    pub relational: RelationalIndex,
}

pub type IndexingErrors = Arc<[IndexingError]>;

pub fn index(module: &cst::Module) -> (IndexingResult, IndexingErrors) {
    let (index, errors) = algorithm::index_module(module);
    (index, IndexingErrors::from(errors))
}
