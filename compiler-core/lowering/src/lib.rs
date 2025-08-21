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
    pub graph_info: LoweringGraphInfo,
}

pub fn lower_module(module: &cst::Module, indexed: &FullIndexedModule) -> FullLoweredModule {
    let algorithm::State { intermediate, source, graph, graph_info, .. } =
        algorithm::lower_module(module, indexed);
    FullLoweredModule { intermediate, source, graph, graph_info }
}

/*
 * Quick and easy summary of what we want to change
 *
 * We nede to rethink if we should continue to use DeferredResolution as an
 * abstraction for qualified names, then have a separate QualifiedNameId for
 * the purpose of source-mapping or completely replace it with the latter.
 *
 * I mean ultimately they mean the same thing, just that DeferredResolution
 * contains the extracted contents of a QualifiedName as well as the domain
 * that it was extracted from.
 *
 * Why not just have QualifiedNameId point to that information though?
 *
 * QualifiedNameId => QualifiedNameIr
 *
 * struct QualifiedNameIr {
 *   domain: Domain,
 *   qualifier: Option<SmolStr>,
 *   name: Option<SmolStr>,
 * }
 *
 * DeferredResolution is a redundant abstraction and I reckon we're better
 * off with using QualifiedNameIr for this.
 *
 * */
