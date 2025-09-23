mod algorithm;

pub mod intermediate;
pub mod scope;
pub mod source;

pub use intermediate::*;
pub use scope::*;
pub use source::*;

use indexing::IndexedModule;
use resolving::ResolvedModule;
use stabilizing::StabilizedModule;
use syntax::cst;

#[derive(Debug, PartialEq, Eq)]
pub struct LoweredModule {
    pub info: LoweringInfo,
    pub graph: LoweringGraph,
    pub nodes: LoweringGraphNodes,
}

pub fn lower_module(
    module: &cst::Module,
    prim: &ResolvedModule,
    stabilized: &StabilizedModule,
    indexed: &IndexedModule,
    resolved: &ResolvedModule,
) -> LoweredModule {
    let algorithm::State { info, graph, nodes, .. } =
        algorithm::lower_module(module, prim, stabilized, indexed, resolved);
    LoweredModule { info, graph, nodes }
}
