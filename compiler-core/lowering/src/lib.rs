mod algorithm;

pub mod intermediate;
pub mod scope;
pub mod source;

pub use intermediate::*;
pub use scope::*;
pub use source::*;

use indexing::FullIndexedModule;
use resolving::FullResolvedModule;
use syntax::cst;

#[derive(Debug, PartialEq, Eq)]
pub struct FullLoweredModule {
    pub intermediate: Intermediate,
    pub source: LoweringSource,
    pub graph: LoweringGraph,
    pub nodes: LoweringGraphNodes,
}

pub fn lower_module(
    module: &cst::Module,
    prim: &FullResolvedModule,
    indexed: &FullIndexedModule,
    resolved: &FullResolvedModule,
) -> FullLoweredModule {
    let algorithm::State { intermediate, source, graph, nodes, .. } =
        algorithm::lower_module(module, prim, indexed, resolved);
    FullLoweredModule { intermediate, source, graph, nodes }
}
