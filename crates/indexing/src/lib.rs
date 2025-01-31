mod algorithm;
mod error;
mod indexes;
mod sourcemap;
mod wellformed;

use std::sync::Arc;

pub use error::*;
pub use indexes::*;
pub use sourcemap::*;

use syntax::cst;

#[derive(Debug)]
pub struct IndexingResult {
    pub source_map: SourceMap,
    pub nominal: NominalIndex,
    pub relational: RelationalIndex,
}

pub type IndexingErrors = Arc<[IndexingError]>;

pub fn index(module: &cst::Module) -> (IndexingResult, IndexingErrors) {
    let (index, errors) = algorithm::index_module(module);
    let errors = wellformed::check_index(&index, errors);
    (index, errors)
}
