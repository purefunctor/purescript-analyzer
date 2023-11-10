//! ADTs for scope information and name resolution.
use std::ops;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::AstId,
    surface::{BinderId, ExprId, LetBindingId},
};

/// Scope information as a linked list node.
///
/// We store scope information in the form of a linked list allocated through
/// an index-based arena. Syntactic constructs such as binders or let-bindings
/// allocate scopes regardless if they introduce names or not.
///
/// For example, the following declarations:
/// ```haskell
/// f x = unit
/// g _ = unit
/// ```
///
/// Would have the following scopes:
/// ```text
/// f = [ Root, Binders({ x }), LetBound({ }) ]
/// g = [ Root, Binders({   }), LetBound({ }) ]
/// ```
///
/// The only difference being that `f` introduces `x` into the scope, while
/// `g` does not. Also note how an empty `LetBound` scope is introduced as
/// both declarations do not bind names through `where`.
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
    Binders(FxHashMap<SmolStr, BinderId>),
    /// Names introduced by [`LetBindings`].
    ///
    /// For example:
    /// ```haskell
    /// nil = let x = 0 in x
    /// zero = x where x = 0
    /// ```
    ///
    /// [`LetBindings`]: crate::surface::LetBinding
    LetBound(FxHashMap<SmolStr, LetBindingId>),
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
