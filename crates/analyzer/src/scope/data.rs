//! ADTs for scope information and name resolution.
use std::{iter, ops};

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::{DataGroupId, ValueGroupId},
    surface::{BinderId, ExprId, LetNameId, TypeId},
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

impl ScopeData {
    pub fn new(parent: ScopeId, kind: ScopeKind) -> ScopeData {
        ScopeData { parent: Some(parent), kind }
    }
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
#[derive(Debug, Default, PartialEq, Eq)]
pub struct WithScope<T> {
    scope_arena: Arena<ScopeData>,
    value: T,
}

impl<T> WithScope<T> {
    pub fn new(scope_arena: Arena<ScopeData>, value: T) -> WithScope<T> {
        WithScope { scope_arena, value }
    }

    pub(crate) fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &ScopeData> {
        iter::successors(Some(scope_id), |&i| self[i].parent).map(|i| &self[i])
    }
}

impl<T> ops::Index<ScopeId> for WithScope<T> {
    type Output = ScopeData;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scope_arena[index]
    }
}

/// Scope information for a [`ValueGroupId`].
///
/// [`ValueGroupId`]: crate::resolver::ValueGroupId
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ValueGroupScope {
    per_expr: FxHashMap<ExprId, ScopeId>,
}

impl ValueGroupScope {
    pub(crate) fn new(per_expr: FxHashMap<ExprId, ScopeId>) -> ValueGroupScope {
        ValueGroupScope { per_expr }
    }
}

impl WithScope<ValueGroupScope> {
    pub(crate) fn expr_scope(&self, expr_id: ExprId) -> ScopeId {
        let scope_id = self.value.per_expr.get(&expr_id).unwrap_or_else(|| {
            unreachable!("invariant violated: expression should have been assigned a scope.")
        });
        *scope_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstructorResolution {
    pub data_id: InFile<DataGroupId>,
    pub constructor_id: AstId<ast::DataConstructor>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeResolution {
    Data(InFile<DataGroupId>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariableResolution {
    pub thunked: bool,
    pub kind: VariableResolutionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableResolutionKind {
    Binder(BinderId),
    LetName(LetNameId),
    Local(ValueGroupId),
}

/// Name resolution information for a [`ValueGroupId`].
#[derive(Debug, PartialEq, Eq)]
pub struct Resolutions {
    /// A mapping from [`Expr::Constructor`] IDs to their resolutions.
    ///
    /// [`Expr::Constructor`]: crate::surface::Expr::Constructor
    per_constructor_expr: FxHashMap<ExprId, ConstructorResolution>,
    /// A mapping from [`Binder::Constructor`] IDs to their resolutions.
    ///
    /// [`Binder::Constructor`]: crate::surface::Binder::Constructor
    per_constructor_binder: FxHashMap<BinderId, ConstructorResolution>,
    /// A mapping from [`Type::Constructor`] IDs to their resolutions.
    ///
    /// [`Type::Constructor`]: crate::surface::Type::Constructor
    per_type_type: FxHashMap<TypeId, TypeResolution>,
    /// A mapping from [`Expr::Variable`] IDs to their resolutions.
    ///
    /// [`Expr::Variable`]: crate::surface::Expr::Variable
    per_variable: FxHashMap<ExprId, VariableResolution>,
}

impl Resolutions {
    pub(crate) fn new(
        per_constructor_expr: FxHashMap<ExprId, ConstructorResolution>,
        per_constructor_binder: FxHashMap<BinderId, ConstructorResolution>,
        per_type_type: FxHashMap<TypeId, TypeResolution>,
        per_variable: FxHashMap<ExprId, VariableResolution>,
    ) -> Resolutions {
        Resolutions { per_constructor_expr, per_constructor_binder, per_type_type, per_variable }
    }

    pub fn get_constructor_expr(&self, expr_id: ExprId) -> Option<ConstructorResolution> {
        self.per_constructor_expr.get(&expr_id).copied()
    }

    pub fn get_constructor_binder(&self, binder_id: BinderId) -> Option<ConstructorResolution> {
        self.per_constructor_binder.get(&binder_id).copied()
    }

    pub fn get_type_type(&self, type_id: TypeId) -> Option<TypeResolution> {
        self.per_type_type.get(&type_id).copied()
    }

    pub fn get_variable(&self, expr_id: ExprId) -> Option<VariableResolution> {
        self.per_variable.get(&expr_id).copied()
    }
}
