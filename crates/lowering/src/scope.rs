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
//! Names that cannot be resolved locally become [deferred resolutions]—they
//! depend on the module-level context in order to be resolved. For instance,
//! knowing the type of an imported value depends on type checking that module
//! first, then associating the type to the [`DeferredResolutionId`].
//!
//! [scope graph]: https://pl.ewi.tudelft.nl/research/projects/scope-graphs/
//! [deferred resolutions]: DeferredResolution
use std::{collections::VecDeque, ops, sync::Arc};

use indexmap::IndexMap;
use la_arena::{Arena, Idx, RawIdx};
use rustc_hash::{FxBuildHasher, FxHashMap};
use smol_str::SmolStr;
use syntax::create_association;

use crate::source::*;

/// A resolution for term names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermResolution {
    Deferred(DeferredResolutionId),
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
    Implicit { binding: bool, node: GraphNodeId, id: ImplicitTypeVariableBindingId },
}

/// See documentation for [`GraphNode::Implicit`].
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ImplicitBindings {
    inner: IndexMap<SmolStr, Vec<TypeId>, FxBuildHasher>,
}

pub type ImplicitTypeVariableBindingId = Idx<SmolStr>;

impl ImplicitBindings {
    pub(crate) fn bind(&mut self, name: &str, id: TypeId) -> ImplicitTypeVariableBindingId {
        let name = SmolStr::from(name);
        let entry = self.inner.entry(name);
        let index = entry.index();
        entry.or_default().push(id);
        Idx::from_raw(RawIdx::from_u32(index as u32))
    }

    pub fn get(&self, name: &str) -> Option<ImplicitTypeVariableBindingId> {
        let (index, _, _) = self.inner.get_full(name)?;
        Some(Idx::from_raw(RawIdx::from_u32(index as u32)))
    }

    pub fn get_index(&self, index: ImplicitTypeVariableBindingId) -> Option<(&str, &[TypeId])> {
        let index = index.into_raw().into_u32() as usize;
        let (name, ids) = self.inner.get_index(index)?;
        Some((name, ids))
    }
}

/// A node in the [`LoweringGraph`].
#[derive(Debug, PartialEq, Eq)]
pub enum GraphNode {
    /// Names bound by patterns.
    Binder { parent: Option<GraphNodeId>, bindings: FxHashMap<SmolStr, BinderId> },
    /// Explicitly quantified type variabbles.
    Forall { parent: Option<GraphNodeId>, bindings: FxHashMap<SmolStr, TypeVariableBindingId> },
    /// Names bound by `let`.
    Let { parent: Option<GraphNodeId>, bindings: FxHashMap<SmolStr, LetBindingResolution> },
    /// Implicitly quantified type variables.
    Implicit {
        parent: Option<GraphNodeId>,
        /// If this implicit scope is collecting type variables.
        collecting: bool,
        /// Mapping from names to the type variables that introduced them.
        ///
        /// Implicitly quantified type variables do not have an intrinsic ID
        /// which uniquely identifies them. Instead, we use an [`IndexMap`]
        /// to allocate stable IDs to the [`SmolStr`] bindings in scope.
        /// Additionally, we also track the [`TypeId`] of the type variables
        /// that introduced these names in scope.
        ///
        /// In PureScript, implicit type variables currently only appear in
        /// instance declarations, like the following:
        ///
        /// ```text
        /// instance Eq a => Ord a
        /// ```
        ///
        /// This would create the binding when traversing `Ord a`:
        ///
        /// ```text
        /// "a" / SmolStrId(0) => [TypeId(0)]
        /// ```
        ///
        /// Subsequently, traversing `Eq a` would create the resolution:
        ///
        /// ```text
        /// TypeId(1) => GraphNodeId(0) + SmolStrId(0)
        /// ```
        bindings: ImplicitBindings,
    },
}

pub type GraphNodeId = Idx<GraphNode>;

/// The domain of a root resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolutionDomain {
    Term,
    Type,
}

/// A resolution to a non-local binding.
#[derive(Debug, PartialEq, Eq)]
pub struct DeferredResolution {
    pub domain: ResolutionDomain,
    pub qualifier: Option<SmolStr>,
    pub name: Option<SmolStr>,
}

pub type DeferredResolutionId = Idx<DeferredResolution>;

/// A scope graph for PureScript.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct LoweringGraph {
    pub(crate) inner: Arena<GraphNode>,
    pub(crate) deferred: Arena<DeferredResolution>,
}

impl LoweringGraph {
    /// Initialise a traversal starting from a [`GraphNodeId`].
    pub fn traverse(&self, id: GraphNodeId) -> GraphIter<'_> {
        let inner = &self.inner;
        let queue = VecDeque::from([id]);
        GraphIter { inner, queue }
    }

    /// An iterator over the current set of [`DeferredResolution`].
    pub fn deferred(&self) -> impl Iterator<Item = (DeferredResolutionId, &DeferredResolution)> {
        self.deferred.iter()
    }
}

impl ops::Index<GraphNodeId> for LoweringGraph {
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

/// An iterator that traverses the [`LoweringGraph`].
pub struct GraphIter<'a> {
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
            | GraphNode::Implicit { parent, .. } => {
                parent.map(|id| {
                    self.queue.push_front(id);
                });
            }
        };
        Some((id, item))
    }
}
