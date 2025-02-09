mod algorithm;

pub mod index;
pub mod source;

pub use index::*;
pub use source::*;

use syntax::cst;

pub fn index_module(module: &cst::Module) -> (Index, Relational, Source, Vec<IndexError>) {
    let algorithm::State { index, relational, source, error } = algorithm::index_module(module);
    (index, relational, source, error)
}
