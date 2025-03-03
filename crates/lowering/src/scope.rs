//! Scope Graphs for PureScript
//!
//! This module implements a [scope graph] for PureScript. Scope graphs are
//! a novel take on name resolution which allow resolution semantics to be
//! represented independent of the language using graphs and graph traversals.
//!
//! [scope graph]: https://pl.ewi.tudelft.nl/research/projects/scope-graphs/
use std::{collections::VecDeque, ops, sync::Arc};

use indexmap::IndexMap;
use la_arena::{Arena, Idx};
use rustc_hash::{FxBuildHasher, FxHashMap};
use smol_str::SmolStr;
use syntax::create_association;

use crate::source::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermResolution {
    Root(RootResolutionId),
    Binder(BinderId),
    Let(LetBindingResolution),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetBindingResolution {
    pub signature: Option<LetBindingSignatureId>,
    pub equations: Arc<[LetBindingEquationId]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeVariableResolution {
    Forall(TypeVariableBindingId),
    Instance { binding: bool, node: GraphNodeId, index: usize },
}

#[derive(Debug, PartialEq, Eq)]
pub enum GraphNode {
    Binder {
        parent: Option<GraphNodeId>,
        bindings: FxHashMap<SmolStr, BinderId>,
    },
    Forall {
        parent: Option<GraphNodeId>,
        bindings: FxHashMap<SmolStr, TypeVariableBindingId>,
    },
    Let {
        parent: Option<GraphNodeId>,
        bindings: FxHashMap<SmolStr, LetBindingResolution>,
    },
    Constraint {
        parent: Option<GraphNodeId>,
        collecting: bool,
        bindings: IndexMap<SmolStr, Vec<TypeId>, FxBuildHasher>,
    },
}

pub type GraphNodeId = Idx<GraphNode>;

#[derive(Debug, PartialEq, Eq)]
pub enum ResolutionDomain {
    Term,
    Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct RootResolution {
    pub domain: ResolutionDomain,
    pub qualifier: Option<SmolStr>,
    pub name: Option<SmolStr>,
}

pub type RootResolutionId = Idx<RootResolution>;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Graph {
    pub(crate) inner: Arena<GraphNode>,
    pub(crate) root: Arena<RootResolution>,
}

impl Graph {
    pub(crate) fn traverse(&self, id: GraphNodeId) -> GraphIter<'_> {
        let inner = &self.inner;
        let queue = VecDeque::from([id]);
        GraphIter { inner, queue }
    }
}

impl ops::Index<GraphNodeId> for Graph {
    type Output = GraphNode;

    fn index(&self, index: GraphNodeId) -> &Self::Output {
        &self.inner[index]
    }
}

create_association! {
    pub struct GraphNodeInfo {
        bd: BinderId => GraphNodeId,
        ex: ExpressionId => GraphNodeId,
        ty: TypeId => GraphNodeId,
        ds: DoStatementId => GraphNodeId,
    }
}

pub(crate) struct GraphIter<'a> {
    inner: &'a Arena<GraphNode>,
    queue: VecDeque<GraphNodeId>,
}

impl<'a> Iterator for GraphIter<'a> {
    type Item = (Idx<GraphNode>, &'a GraphNode);

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.queue.pop_back()?;
        let item = &self.inner[id];
        match &item {
            GraphNode::Binder { parent, .. }
            | GraphNode::Forall { parent, .. }
            | GraphNode::Let { parent, .. }
            | GraphNode::Constraint { parent, .. } => {
                parent.map(|id| {
                    self.queue.push_front(id);
                });
            }
        };
        Some((id, item))
    }
}
