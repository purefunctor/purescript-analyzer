#[macro_use]
mod recover;

mod algorithm;

pub mod error;
pub mod intermediate;
pub mod scope;
pub mod source;

use std::sync::Arc;
use std::{hash::Hash, slice};

pub use error::*;
pub use intermediate::*;
pub use scope::*;
pub use source::*;

use files::FileId;
use indexing::{IndexedModule, TermItemId, TypeItemId};
use petgraph::algo::tarjan_scc;
use petgraph::prelude::DiGraphMap;
use resolving::ResolvedModule;
use rustc_hash::{FxBuildHasher, FxHashSet};
use stabilizing::StabilizedModule;
use syntax::cst;

#[derive(Debug, PartialEq, Eq)]
pub struct LoweredModule {
    pub info: LoweringInfo,
    pub graph: LoweringGraph,
    pub nodes: LoweringGraphNodes,
    pub term_edges: FxHashSet<(TermItemId, TermItemId)>,
    pub type_edges: FxHashSet<(TypeItemId, TypeItemId)>,
    pub kind_edges: FxHashSet<(TypeItemId, TypeItemId)>,
    pub synonym_edges: FxHashSet<(TypeItemId, TypeItemId)>,
    pub errors: Vec<LoweringError>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GroupedModule {
    pub term_scc: Vec<Scc<TermItemId>>,
    pub type_scc: Vec<Scc<TypeItemId>>,
    pub cycle_errors: Vec<LoweringError>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scc<T> {
    /// Non-recursive
    Base(T),
    /// Recursive
    Recursive(T),
    /// Mutually-recursive
    Mutual(Vec<T>),
}

impl<T> Scc<T> {
    pub fn as_slice(&self) -> &[T] {
        match self {
            Scc::Base(item) | Scc::Recursive(item) => slice::from_ref(item),
            Scc::Mutual(items) => items.as_slice(),
        }
    }

    pub fn is_recursive(&self) -> bool {
        matches!(self, Scc::Recursive(..) | Scc::Mutual(..))
    }
}

pub fn lower_module(
    file_id: FileId,
    module: &cst::Module,
    prim: &ResolvedModule,
    stabilized: &StabilizedModule,
    indexed: &IndexedModule,
    resolved: &ResolvedModule,
) -> LoweredModule {
    let algorithm::State {
        info,
        graph,
        nodes,
        term_edges,
        type_edges,
        kind_edges,
        synonym_edges,
        errors,
        ..
    } = algorithm::lower_module(file_id, module, prim, stabilized, indexed, resolved);

    LoweredModule { info, graph, nodes, term_edges, type_edges, kind_edges, synonym_edges, errors }
}

pub fn group_module(indexed: &IndexedModule, lowered: &LoweredModule) -> GroupedModule {
    let term_nodes = || indexed.items.iter_terms().map(|(id, _)| id);
    let type_nodes = || indexed.items.iter_types().map(|(id, _)| id);

    let term_scc = compute_scc(term_nodes(), &lowered.term_edges);
    let type_scc = compute_scc(type_nodes(), &lowered.type_edges);

    let kind_cycles = find_cycles(type_nodes(), &lowered.kind_edges);
    let synonym_cycles = find_cycles(type_nodes(), &lowered.synonym_edges);

    let kind_cycles = kind_cycles
        .into_iter()
        .map(|group| LoweringError::RecursiveKinds(RecursiveGroup { group }));

    let synonym_cycles = synonym_cycles
        .into_iter()
        .map(|group| LoweringError::RecursiveSynonym(RecursiveGroup { group }));

    let cycle_errors = kind_cycles.chain(synonym_cycles).collect();
    GroupedModule { term_scc, type_scc, cycle_errors }
}

fn compute_scc<N>(nodes: impl Iterator<Item = N>, edges: &FxHashSet<(N, N)>) -> Vec<Scc<N>>
where
    N: Copy + Ord + Hash,
{
    let graph = build_graph(nodes, edges);
    tarjan_scc(&graph).into_iter().map(|scc| into_scc(&graph, scc)).collect()
}

fn find_cycles<N>(nodes: impl Iterator<Item = N>, edges: &FxHashSet<(N, N)>) -> Vec<Arc<[N]>>
where
    N: Copy + Ord + Hash,
{
    let graph = build_graph(nodes, edges);

    let components = tarjan_scc(&graph).into_iter().filter_map(|scc| match scc.as_slice() {
        [single] if !graph.contains_edge(*single, *single) => None,
        _ => Some(Arc::from(scc)),
    });

    components.collect()
}

fn build_graph<N>(
    nodes: impl Iterator<Item = N>,
    edges: &FxHashSet<(N, N)>,
) -> DiGraphMap<N, (), FxBuildHasher>
where
    N: Copy + Ord + Hash,
{
    let mut graph = DiGraphMap::default();
    for node in nodes {
        graph.add_node(node);
    }
    for &(from, to) in edges {
        graph.add_edge(from, to, ());
    }
    graph
}

fn into_scc<N>(graph: &DiGraphMap<N, (), FxBuildHasher>, scc: Vec<N>) -> Scc<N>
where
    N: Copy + Ord + Hash,
{
    match scc[..] {
        [single] if !graph.contains_edge(single, single) => Scc::Base(single),
        [single] => Scc::Recursive(single),
        _ => Scc::Mutual(scc),
    }
}
