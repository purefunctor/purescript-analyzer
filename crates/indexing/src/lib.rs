mod algorithm;

pub mod index;
pub mod source;

pub use index::*;
pub use source::*;

use syntax::cst;

pub fn index_module(module: &cst::Module) -> (Index, Source, Vec<IndexError>) {
    let algorithm::State { index, source, error } = algorithm::index_module(module);
    (index, source, error)
}
