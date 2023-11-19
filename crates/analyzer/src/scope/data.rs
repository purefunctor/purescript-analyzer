//! ADTs for scope information and name resolution.
use std::{iter, ops};

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    surface::{BinderId, ExprId, LetNameId},
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
/// h = [ Root, Binders({   }, NoThunk) ]
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
    LetBound(FxHashMap<SmolStr, LetNameId>, LetKind),
}

/// The kind of a binder.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinderKind {
    Thunk,
    NoThunk,
}

/// The kind of a let binding.
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

/// The result of name resultion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolutionKind {
    Binder(BinderId),
    LetName(LetNameId),
    Global(InFile<ValueGroupId>),
}

type ResolutionPerExpr = FxHashMap<ExprId, ResolutionKind>;

/// Name resolution information for a [`ValueGroupId`].
#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroupResolutions {
    /// A mapping from [`Expr::Variable`] IDs to their resolutions.
    ///
    /// [`Expr::Variable`]: crate::surface::Expr::Variable
    resolutions: FxHashMap<AstId<ast::ValueEquationDeclaration>, ResolutionPerExpr>,
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

impl WithScope<ValueGroupScope> {
    pub(crate) fn expr_scope(
        &self,
        equation_id: AstId<ast::ValueEquationDeclaration>,
        expr_id: ExprId,
    ) -> ScopeId {
        let per_expr = self.value.per_equation.get(&equation_id).unwrap_or_else(|| {
            unreachable!("invariant violated: equation should have been assigned a per_expr.")
        });
        let scope_id = per_expr.get(&expr_id).unwrap_or_else(|| {
            unreachable!("invariant violated: expression should have been assigned a scope.")
        });
        *scope_id
    }
}

impl ValueGroupResolutions {
    pub(crate) fn new(
        resolutions: FxHashMap<AstId<ast::ValueEquationDeclaration>, ResolutionPerExpr>,
    ) -> ValueGroupResolutions {
        ValueGroupResolutions { resolutions }
    }

    pub fn get(
        &self,
        equation_id: AstId<ast::ValueEquationDeclaration>,
        expr_id: ExprId,
    ) -> Option<ResolutionKind> {
        Some(*self.resolutions.get(&equation_id)?.get(&expr_id)?)
    }
}

impl<T> WithScope<T> {
    pub(crate) fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &ScopeData> {
        iter::successors(Some(scope_id), |&i| self[i].parent).map(|i| &self[i])
    }
}
