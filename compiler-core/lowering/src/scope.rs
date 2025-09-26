//! Scope graphs for local resolution.
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
//! [scope graph]: https://pl.ewi.tudelft.nl/research/projects/scope-graphs/
use std::{collections::VecDeque, ops, sync::Arc};

use files::FileId;
use indexing::TermItemId;
use indexmap::IndexMap;
use la_arena::{Arena, Idx, RawIdx};
use rustc_hash::{FxBuildHasher, FxHashMap};
use smol_str::SmolStr;

use crate::source::*;

/// The result of resolving a term variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermVariableResolution {
    Binder(BinderId),
    Let(LetBound),
    Reference(FileId, TermItemId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetBound {
    pub signature: Option<LetBindingSignatureId>,
    pub equations: Arc<[LetBindingEquationId]>,
}

/// The result of resolving a type variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeVariableResolution {
    Forall(TypeVariableBindingId),
    Implicit(ImplicitTypeVariable),
}

/// A locator for implicit type variables.
/// 
/// Unlike explicitly-quantified type variables, implicitly quantified
/// type variables do not have an [`AstId`] to anchor to. Instead, the
/// type variables themselves introduce the name into the scope.
///
/// Implicit type variables have multiple definition positions, much 
/// like value signatures and equations. For example, both occurrences
/// of `f` here are equally valid definition positions:
///
/// ```purescript
/// instance Coerce (f a) (f b)
/// ```
///
/// We use the [`GraphNode::Implicit`] node to group these positions 
/// together using the [`ImplicitBindings`], with the condition that
/// that we must store the [`GraphNodeId`] in this structure.
///
/// Rather than store [`SmolStr`] names directly, we intern them first
/// through the [`ImplicitBindings`] to keep the representation light
/// enough for use in the type checker.
///
/// [`AstId`]: stabilizing::AstId
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplicitTypeVariable {
    /// Determines the 'position' of an implicit type variable.
    ///
    /// If [`true`], this type variable introduced the name into
    /// the scope. If [`false`], this is what the name resolves to.
    pub binding: bool,
    /// The id for the [`GraphNode::Implicit`] node.
    pub node: GraphNodeId,
    /// The id for the [`ImplicitBindings`] map.
    pub id: ImplicitBindingId,
}

/// See documentation for [`ImplicitTypeVariable`].
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ImplicitBindings {
    inner: IndexMap<SmolStr, Vec<TypeId>, FxBuildHasher>,
}

pub type ImplicitBindingId = Idx<SmolStr>;

impl ImplicitBindings {
    pub(crate) fn bind(&mut self, name: &str, id: TypeId) -> ImplicitBindingId {
        let name = SmolStr::from(name);
        let entry = self.inner.entry(name);
        let index = entry.index();
        entry.or_default().push(id);
        Idx::from_raw(RawIdx::from_u32(index as u32))
    }

    pub fn get(&self, name: &str) -> Option<ImplicitBindingId> {
        let (index, _, _) = self.inner.get_full(name)?;
        Some(Idx::from_raw(RawIdx::from_u32(index as u32)))
    }

    pub fn get_index(&self, index: ImplicitBindingId) -> Option<(&str, &[TypeId])> {
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
    /// Explicitly quantified type variables.
    Forall { parent: Option<GraphNodeId>, bindings: FxHashMap<SmolStr, TypeVariableBindingId> },
    /// Names bound by `let`.
    Let { parent: Option<GraphNodeId>, bindings: FxHashMap<SmolStr, LetBound> },
    /// Implicitly quantified type variables.
    Implicit {
        parent: Option<GraphNodeId>,
        /// If this implicit scope is collecting type variables.
        collecting: bool,
        /// See documentation for [`ImplicitTypeVariable`].
        bindings: ImplicitBindings,
    },
}

pub type GraphNodeId = Idx<GraphNode>;

/// A scope graph for PureScript.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct LoweringGraph {
    pub(crate) inner: Arena<GraphNode>,
}

impl LoweringGraph {
    /// Initialise a traversal starting from a [`GraphNodeId`].
    pub fn traverse(&self, id: GraphNodeId) -> GraphIter<'_> {
        let inner = &self.inner;
        let queue = VecDeque::from([id]);
        GraphIter { inner, queue }
    }
}

impl ops::Index<GraphNodeId> for LoweringGraph {
    type Output = GraphNode;

    fn index(&self, index: GraphNodeId) -> &Self::Output {
        &self.inner[index]
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct LoweringGraphNodes {
    pub(crate) binder_node: FxHashMap<BinderId, GraphNodeId>,
    pub(crate) expression_node: FxHashMap<ExpressionId, GraphNodeId>,
    pub(crate) type_node: FxHashMap<TypeId, GraphNodeId>,
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
