use indexing::IndexingResult;
use syntax::cst;

mod algorithm;
mod concrete;
mod sourcemap;

pub use concrete::*;
pub use sourcemap::*;

#[derive(Debug)]
pub struct LoweringResult {
    pub source_map: SourceMap,
    pub lowering_map: LoweringMap,
}

pub fn lower(module: &cst::Module, index: &IndexingResult) -> LoweringResult {
    algorithm::lower_module(module, index)
}
