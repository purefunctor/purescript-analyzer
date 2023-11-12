//! ADTs for scope information and name resolution.
use std::ops;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::AstId,
    surface::{BinderId, ExprId, LetNameGroupId},
};

/// Scope information as a graph node.
///
/// We store scope information in the form of a directed acyclic graph
/// allocated through an index-based arena. Aside from names, scope nodes
/// can also introduce information such as "thunk" contexts.
/// 
/// For example, the following declarations:
///
/// ```haskell
/// f x = 0
/// g _ = 0
/// h = 0
/// ```
///
/// Would yield the following scopes:
/// ```haskell
/// f = [ Root, Binders({ 'x' }, Thunk) ]
/// g = [ Root, Binders({     }, Thunk) ]
/// h = [ Root, Binders({     }, NoThunk) ]
/// ```
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ScopeData {
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
}

/// The kind of scope information.
#[derive(Debug, Default, PartialEq, Eq)]
pub enum ScopeKind {
    #[default]
    Root,
    /// Names introduced by [`Binders`].
    ///
    /// For example:
    /// ```haskell
    /// identity x = x
    /// ```
    ///
    /// [`Binders`]: crate::surface::Binder
    Binders(FxHashMap<SmolStr, BinderId>, BinderKind),
    /// Names introduced by [`LetBindings`].
    ///
    /// For example:
    /// ```haskell
    /// nil = let x = 0 in x
    /// zero = x where x = 0
    /// ```
    ///
    /// [`LetBindings`]: crate::surface::LetBinding
    LetBound(FxHashMap<SmolStr, LetNameGroupId>, LetKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinderKind {
    Thunk,
    NoThunk,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LetKind {
    LetIn,
    Where,
}

pub type ScopeId = Idx<ScopeData>;

/// A value associated with scope data.
#[derive(Debug, PartialEq, Eq)]
pub struct WithScope<T> {
    scope_arena: Arena<ScopeData>,
    value: T,
}

type ScopePerExpr = FxHashMap<ExprId, ScopeId>;

/// Scope information for a [`ValueGroupId`].
///
/// [`ValueGroupId`]: crate::resolver::ValueGroupId
#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroupScope {
    per_equation: FxHashMap<AstId<ast::ValueEquationDeclaration>, ScopePerExpr>,
}

impl ScopeData {
    pub fn new(parent: ScopeId, kind: ScopeKind) -> ScopeData {
        ScopeData { parent: Some(parent), kind }
    }
}

impl<T> WithScope<T> {
    pub fn new(scope_arena: Arena<ScopeData>, value: T) -> WithScope<T> {
        WithScope { scope_arena, value }
    }
}

impl<T> ops::Index<ScopeId> for WithScope<T> {
    type Output = ScopeData;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scope_arena[index]
    }
}

impl ValueGroupScope {
    pub(crate) fn new(
        per_equation: FxHashMap<AstId<ast::ValueEquationDeclaration>, ScopePerExpr>,
    ) -> ValueGroupScope {
        ValueGroupScope { per_equation }
    }
}
