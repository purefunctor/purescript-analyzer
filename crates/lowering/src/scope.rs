use std::{collections::VecDeque, sync::Arc};

use indexing::{DeriveId, InstanceId};
use la_arena::{Arena, Idx};
use rustc_hash::{FxHashMap, FxHashSet};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstanceKind {
    Instance(InstanceId),
    Derive(DeriveId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeVariableResolution {
    Forall(TypeVariableBindingId),
    Instance(InstanceKind),
    InstanceBinder,
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
        bindings: FxHashSet<SmolStr>,
        id: InstanceKind,
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
    type Item = &'a GraphNode;

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
        Some(item)
    }
}
