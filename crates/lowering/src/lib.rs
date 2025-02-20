mod algorithm;

pub mod intermediate;
pub mod scope;
pub mod source;

pub use intermediate::*;
pub use scope::*;
pub use source::*;

use syntax::cst;

#[derive(Debug, PartialEq, Eq)]
pub struct FullModuleLower {
    pub intermediate: Intermediate,
    pub source: LoweringSource,
    pub graph: Graph,
}

pub fn lower_module(
    module: &cst::Module,
    index: &indexing::Index,
    relational: &indexing::Relational,
    source: &indexing::IndexingSource,
) -> FullModuleLower {
    let algorithm::State { intermediate, source, graph, .. } =
        algorithm::lower_module(module, index, relational, source);
    FullModuleLower { intermediate, source, graph }
}
