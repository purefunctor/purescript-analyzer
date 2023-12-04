//! ADTs for scope information and name resolution.
use std::{iter, ops};

use la_arena::{Arena, Idx};
use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;

use crate::{
    id::InFile,
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
#[derive(Debug, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq)]
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

/// The result of name resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Resolution {
    pub thunked: bool,
    pub kind: ResolutionKind,
}

/// The kind that a name resolves to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolutionKind {
    Binder(BinderId),
    LetName(LetNameId),
    Global(InFile<ValueGroupId>),
}

/// Name resolution information for a [`ValueGroupId`].
#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroupResolutions {
    /// A mapping from [`Expr::Variable`] IDs to their resolutions.
    ///
    /// [`Expr::Variable`]: crate::surface::Expr::Variable
    resolutions: FxHashMap<ExprId, Resolution>,
}

impl ValueGroupResolutions {
    pub(crate) fn new(resolutions: FxHashMap<ExprId, Resolution>) -> ValueGroupResolutions {
        ValueGroupResolutions { resolutions }
    }

    pub fn get(&self, expr_id: ExprId) -> Option<Resolution> {
        self.resolutions.get(&expr_id).copied()
    }
}

/// Information about recursive let bindings.
#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroupRecursiveLets {
    /// Non-recursive let bindings.
    normal: FxHashSet<LetNameId>,
    /// Self-recursive let bindings.
    recursive: FxHashSet<LetNameId>,
    /// Groups of mutually recursive bindings.
    mutual_groups: Vec<Vec<LetNameId>>,
    /// Identifies which group a let binding belongs to.
    group_indices: FxHashMap<LetNameId, usize>,
}

impl ValueGroupRecursiveLets {
    pub(crate) fn new(
        normal: FxHashSet<LetNameId>,
        recursive: FxHashSet<LetNameId>,
        mutual_groups: Vec<Vec<LetNameId>>,
        group_indices: FxHashMap<LetNameId, usize>,
    ) -> ValueGroupRecursiveLets {
        ValueGroupRecursiveLets { normal, recursive, mutual_groups, group_indices }
    }

    pub fn is_normal(&self, id: LetNameId) -> bool {
        self.normal.contains(&id)
    }

    pub fn is_recursive(&self, id: LetNameId) -> bool {
        self.recursive.contains(&id)
    }

    pub fn mutual_group(&self, id: LetNameId) -> Option<&[LetNameId]> {
        let index = *self.group_indices.get(&id)?;
        assert!(index < self.mutual_groups.len());
        Some(&self.mutual_groups[index])
    }
}

/// The kind of module-level binding groups.
#[derive(Debug, PartialEq, Eq)]
pub enum ValueBindingGroup {
    /// A non-recursive, singular [`ValueGroupId`].
    Singular(ValueGroupId),
    /// A self-recursive, singular [`ValueGroupId`].
    Recursive(ValueGroupId),
    /// A mutually-recursive collection of [`ValueGroupId`]s.
    MutuallyRecursive(Vec<ValueGroupId>),
}

pub type ValueBindingGroupId = Idx<ValueBindingGroup>;

/// Collects binding groups within a module.
#[derive(Debug, PartialEq, Eq)]
pub struct BindingGroups {
    inner: Arena<ValueBindingGroup>,
}

impl BindingGroups {
    pub(crate) fn new(inner: Arena<ValueBindingGroup>) -> BindingGroups {
        BindingGroups { inner }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ValueBindingGroupId, &ValueBindingGroup)> {
        self.inner.iter()
    }

    pub fn binding_group_id(&self, id: ValueGroupId) -> ValueBindingGroupId {
        // FIXME: O(n) -> O(1)
        self.iter()
            .find_map(|(value_binding_group_id, value_binding_group)| match value_binding_group {
                ValueBindingGroup::Singular(current_id)
                | ValueBindingGroup::Recursive(current_id) => {
                    if id == *current_id {
                        Some(value_binding_group_id)
                    } else {
                        None
                    }
                }
                ValueBindingGroup::MutuallyRecursive(current_group) => {
                    if current_group.contains(&id) {
                        Some(value_binding_group_id)
                    } else {
                        None
                    }
                }
            })
            .unwrap()
    }

    pub fn binding_group_data(&self, id: ValueBindingGroupId) -> &ValueBindingGroup {
        &self.inner[id]
    }
}
