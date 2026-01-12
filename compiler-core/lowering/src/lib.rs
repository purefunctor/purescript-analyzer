#[macro_use]
mod recover;

mod algorithm;

pub mod error;
pub mod intermediate;
pub mod scope;
pub mod source;

use std::sync::Arc;

pub use error::*;
pub use intermediate::*;
pub use scope::*;
pub use source::*;

use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use petgraph::algo::tarjan_scc;
use petgraph::graphmap::NodeTrait;
use petgraph::prelude::DiGraphMap;
use resolving::ResolvedModule;
use rustc_hash::FxBuildHasher;
use stabilizing::StabilizedModule;
use syntax::cst;

#[derive(Debug, PartialEq, Eq)]
pub struct LoweredModule {
    pub info: LoweringInfo,
    pub graph: LoweringGraph,
    pub nodes: LoweringGraphNodes,
    pub term_scc: Vec<Scc<TermItemId>>,
    pub type_scc: Vec<Scc<TypeItemId>>,
    pub errors: Vec<LoweringError>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Scc<T> {
    /// Non-recursive
    Base(T),
    /// Recursive
    Recursive(T),
    /// Mutually-recursive
    Mutual(Vec<T>),
}

pub fn lower_module(
    file_id: FileId,
    module: &cst::Module,
    prim: &ResolvedModule,
    stabilized: &StabilizedModule,
    indexed: &IndexedModule,
    resolved: &ResolvedModule,
) -> LoweredModule {
    let algorithm::State { info, graph, nodes, term_graph, type_graph, kind_graph, synonym_graph, mut errors, .. } =
        algorithm::lower_module(file_id, module, prim, stabilized, indexed, resolved);

    let term_scc = tarjan_scc(&term_graph);
    let term_scc = term_scc.into_iter().map(into_scc(&term_graph)).collect();

    let type_scc = tarjan_scc(&type_graph);
    let type_scc = type_scc.into_iter().map(into_scc(&type_graph)).collect();

    let kind_scc = tarjan_scc(&kind_graph);
    for scc in kind_scc {
        match scc.as_slice() {
            [single] if !kind_graph.contains_edge(*single, *single) => {
                continue;
            }
            group => {
                let group = Arc::from(group);
                errors.push(LoweringError::RecursiveKinds(RecursiveGroup { group }));
            }
        }
    }

    let synonym_scc = tarjan_scc(&synonym_graph);
    for scc in synonym_scc {
        match scc.as_slice() {
            [single] if !synonym_graph.contains_edge(*single, *single) => {
                continue;
            }
            group => {
                let group = Arc::from(group);
                errors.push(LoweringError::RecursiveSynonym(RecursiveGroup { group }));
            }
        }
    }

    LoweredModule { info, graph, nodes, term_scc, type_scc, errors }
}

fn into_scc<N, E>(graph: &DiGraphMap<N, E, FxBuildHasher>) -> impl FnMut(Vec<N>) -> Scc<N>
where
    N: NodeTrait,
{
    |scc| match scc[..] {
        [single] if !graph.contains_edge(single, single) => Scc::Base(single),
        [single] => Scc::Recursive(single),
        _ => Scc::Mutual(scc),
    }
}
