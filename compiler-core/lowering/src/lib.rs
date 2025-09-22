mod algorithm;

pub mod intermediate;
pub mod scope;
pub mod source;

pub use intermediate::*;
pub use scope::*;
pub use source::*;

use indexing::FullIndexedModule;
use resolving::FullResolvedModule;
use stabilize::StabilizedModule;
use syntax::cst;

#[derive(Debug, PartialEq, Eq)]
pub struct FullLoweredModule {
    pub info: LoweringInfo,
    pub graph: LoweringGraph,
    pub nodes: LoweringGraphNodes,
}

pub fn lower_module(
    module: &cst::Module,
    prim: &FullResolvedModule,
    stabilized: &StabilizedModule,
    indexed: &FullIndexedModule,
    resolved: &FullResolvedModule,
) -> FullLoweredModule {
    let algorithm::State { info, graph, nodes, .. } =
        algorithm::lower_module(module, prim, stabilized, indexed, resolved);
    FullLoweredModule { info, graph, nodes }
}
