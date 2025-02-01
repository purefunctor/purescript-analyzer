use indexing::IndexingResult;
use syntax::cst;

mod algorithm;
mod concrete;
mod sourcemap;

pub use concrete::*;
pub use sourcemap::*;

pub fn lower(module: &cst::Module, index: &IndexingResult) {
    algorithm::lower_module(module, &index);
}
