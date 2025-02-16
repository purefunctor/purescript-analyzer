mod algorithm;

pub mod intermediate;
pub mod scope;
pub mod source;

pub use intermediate::*;
pub use scope::*;
pub use source::*;

use syntax::cst;

pub fn lower_module(
    module: &cst::Module,
    index: &indexing::Index,
    relational: &indexing::Relational,
    source: &indexing::IndexingSource,
) -> (Intermediate, LoweringSource) {
    let algorithm::State { intermediate, source, graph, .. } =
        algorithm::lower_module(module, index, relational, source);
    dbg!(graph);
    (intermediate, source)
}
