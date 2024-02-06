//! Queries pertaining to name resolution.

mod collect;
mod resolve;

use std::{iter, sync::Arc};

use files::FileId;
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    index::nominal::{DataGroupId, ValueGroupId},
    surface::{BinderId, ExprId, LetNameId, Name, TypeId},
    SurfaceDatabase,
};

/// Scope information as a graph node.
///
/// Scope information is represented as a directed acyclic graph stored in an
/// index-based arena. Aside from names, these nodes can also hold information
/// such as where thunks are introduced.
#[derive(Debug, PartialEq, Eq)]
pub struct ScopeData {
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
}

pub type ScopeId = Idx<ScopeData>;

/// The kind of the scope being introduced.
#[derive(Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Root,
    Binders(Option<FxHashMap<Name, BinderId>>),
    LetBound(FxHashMap<Name, LetNameId>),
}

/// Associates surface IDs to scope information.
#[derive(Debug, PartialEq, Eq)]
pub struct ScopeInfo {
    pub scope_data: Arena<ScopeData>,
    pub per_expr: FxHashMap<ExprId, ScopeId>,
}

impl ScopeInfo {
    pub fn expr_scope(&self, expr_id: ExprId) -> ScopeId {
        let scope_id = self.per_expr.get(&expr_id).unwrap_or_else(|| {
            unreachable!("invariant violated: expression should have been assigned a scope.");
        });
        *scope_id
    }

    pub fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &ScopeData> {
        iter::successors(Some(scope_id), |&i| self.scope_data[i].parent)
            .map(|i| &self.scope_data[i])
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstructorResolution {
    file_id: FileId,
    data_id: DataGroupId,
    constructor_id: AstId<ast::DataConstructor>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeConstructorResolution {
    pub file_id: FileId,
    pub kind: TypeConstructorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeConstructorKind {
    Data(DataGroupId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum VariableResolution {
    Binder(BinderId),
    Imported(InFile<ValueGroupId>),
    LetName(LetNameId),
    Local(ValueGroupId),
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ResolveInfo {
    pub per_constructor_binder: FxHashMap<BinderId, ConstructorResolution>,
    pub per_constructor_expr: FxHashMap<ExprId, ConstructorResolution>,
    pub per_type_type: FxHashMap<TypeId, TypeConstructorResolution>,
    pub per_variable_expr: FxHashMap<ExprId, VariableResolution>,
}

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: SurfaceDatabase {
    #[salsa::invoke(collect::file_scope_query)]
    fn file_scope(&self, file_id: FileId) -> Arc<ScopeInfo>;

    #[salsa::invoke(resolve::file_resolve_query)]
    fn file_resolve(&self, file_id: FileId) -> Arc<ResolveInfo>;
}
