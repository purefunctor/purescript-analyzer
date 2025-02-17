mod algorithm;

pub mod index;
pub mod source;

pub use index::*;
pub use source::*;

use syntax::cst;

pub struct FullModuleIndex {
    pub index: Index,
    pub relational: Relational,
    pub source: IndexingSource,
    pub error: Vec<IndexError>,
}

pub fn index_module(module: &cst::Module) -> FullModuleIndex {
    let algorithm::State { index, relational, source, error } = algorithm::index_module(module);
    FullModuleIndex { index, relational, source, error }
}
