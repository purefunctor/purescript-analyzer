mod algorithm;
mod error;
mod id;
mod indexes;
mod sourcemap;

pub use error::*;
pub use id::*;
pub use indexes::*;
pub use sourcemap::*;

use algorithm::IndexState;
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
    let mut state = IndexState::default();
    algorithm::index_module(&mut state, module);

    let IndexState { source_map, nominal, relational, errors } = state;
    FullIndexingResult { source_map, nominal, relational, errors }
}
