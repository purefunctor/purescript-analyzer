//! Scope Graphs for PureScript
//!
//! This module implements a [scope graph] for PureScript. Scope graphs are
//! a novel take on name resolution which allow resolution semantics to be
//! represented independent of the language using graphs and graph traversals.
//!
//! The scope graph is built during lowering from the CST to the intermediate
//! representation. Local name resolution is also performed eagerly, which
//! enriches the IR with resolution information that simplifies associating
//! information to resolved nodes. For instance, knowing the type of a variable
//! can be as easy as obtaining the type of a [`BinderId`].
//!
//! Names that cannot be resolved locally become [root resolutions]—they depend
//! on the module-level context in order to be resolved. For instance, knowing
//! the type of an imported value depends on type checking that module first,
//! then associating the type to the [`RootResolutionId`].
//!
//! [scope graph]: https://pl.ewi.tudelft.nl/research/projects/scope-graphs/
//! [root resolutions]: RootResolution
use std::{collections::VecDeque, ops, sync::Arc};

use indexmap::IndexMap;
use la_arena::{Arena, Idx};
use rustc_hash::{FxBuildHasher, FxHashMap};
use smol_str::SmolStr;
use syntax::create_association;

use crate::source::*;

/// A resolution for term names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermResolution {
    Root(RootResolutionId),
    Binder(BinderId),
    Let(LetBindingResolution),
}

/// A resolution to a `let`-bound name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetBindingResolution {
    pub signature: Option<LetBindingSignatureId>,
    pub equations: Arc<[LetBindingEquationId]>,
}

/// A resolution for type variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeVariableResolution {
    Forall(TypeVariableBindingId),
    Instance { binding: bool, node: GraphNodeId, index: usize },
}

/// A node in the [`Graph`].
#[derive(Debug, PartialEq, Eq)]
pub enum GraphNode {
    /// Names bound by patterns.
    Binder { parent: Option<GraphNodeId>, bindings: FxHashMap<SmolStr, BinderId> },
    /// Explicitly quantified type variabbles.
    Forall { parent: Option<GraphNodeId>, bindings: FxHashMap<SmolStr, TypeVariableBindingId> },
    /// Names bound by `let`.
    Let { parent: Option<GraphNodeId>, bindings: FxHashMap<SmolStr, LetBindingResolution> },
    /// Implicitly quantified type variables.
    Constraint {
        parent: Option<GraphNodeId>,
        collecting: bool,
        bindings: IndexMap<SmolStr, Vec<TypeId>, FxBuildHasher>,
    },
}

pub type GraphNodeId = Idx<GraphNode>;

/// The domain of a root resolution.
#[derive(Debug, PartialEq, Eq)]
pub enum ResolutionDomain {
    Term,
    Type,
}

/// A resolution to a non-local binding.
#[derive(Debug, PartialEq, Eq)]
pub struct RootResolution {
    pub domain: ResolutionDomain,
    pub qualifier: Option<SmolStr>,
    pub name: Option<SmolStr>,
}

pub type RootResolutionId = Idx<RootResolution>;

/// A scope graph for PureScript.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Graph {
    pub(crate) inner: Arena<GraphNode>,
    pub(crate) root: Arena<RootResolution>,
}

impl Graph {
    /// Initialise a traversal starting from a [`GraphNodeId`].
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
    /// Tracks [`GraphNodeId`] for IR nodes.
    pub struct GraphNodeInfo {
        bd: BinderId => GraphNodeId,
        ex: ExpressionId => GraphNodeId,
        ty: TypeId => GraphNodeId,
        ds: DoStatementId => GraphNodeId,
    }
}

/// An iterator that traverses the [`Graph`].
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
