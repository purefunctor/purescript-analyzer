mod algorithm;
mod error;
mod id;
mod indexes;
mod sourcemap;

pub use error::*;
pub use id::*;
pub use indexes::*;
pub use sourcemap::*;

use syntax::cst;

/// The full result of the indexing algorithm.
pub struct FullIndexingResult {
    pub source_map: SourceMap,
    pub nominal: NominalIndex,
    pub relational: RelationalIndex,
    pub errors: Vec<IndexingError>,
}

/// Runs the indexing algorithm on a [`cst::Module`].
pub fn index(module: &cst::Module) -> FullIndexingResult {
    algorithm::index_module(module)
}
