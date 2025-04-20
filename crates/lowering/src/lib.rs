mod algorithm;

pub mod intermediate;
pub mod scope;
pub mod source;

pub use intermediate::*;
pub use scope::*;
pub use source::*;

use indexing::FullIndexedModule;
use syntax::cst;

#[derive(Debug, PartialEq, Eq)]
pub struct FullLoweredModule {
    pub intermediate: Intermediate,
    pub source: LoweringSource,
    pub graph: LoweringGraph,
}

pub fn lower_module(module: &cst::Module, indexed: &FullIndexedModule) -> FullLoweredModule {
    let algorithm::State { intermediate, source, graph, .. } =
        algorithm::lower_module(module, indexed);
    FullLoweredModule { intermediate, source, graph }
}
