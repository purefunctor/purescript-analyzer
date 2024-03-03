//! Queries pertaining to name resolution.
//!
//! In particular, this module is responsible for collecting the [`ScopeInfo`]
//! and [`ResolveInfo`] structs by traversing the surface AST.
//!
//! [`ScopeInfo`] builds a directed acyclic graph of scope information, and
//! associates surface IDs to corresponding [`ScopeData`] nodes.
//!
//! [`ResolveInfo`] maps surface IDs to what they actually resolve to based
//! on the scope information provided by the [`ScopeInfo`].

mod collect;
mod resolve;

use std::{iter, sync::Arc};

use files::FileId;
use la_arena::{Arena, Idx};
use rustc_hash::{FxHashMap, FxHashSet};
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    index::nominal::{ClassGroupId, DataGroupId, ValueGroupId},
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
    TypeVariable(FxHashSet<Name>, TypeVariableKind),
}

/// Associates surface IDs to scope information.
#[derive(Debug, PartialEq, Eq)]
pub struct ScopeInfo {
    pub scope_data: Arena<ScopeData>,
    pub per_expr: FxHashMap<ExprId, ScopeId>,
    pub per_type: FxHashMap<TypeId, ScopeId>,
}

impl ScopeInfo {
    pub fn expr_scope(&self, expr_id: ExprId) -> ScopeId {
        let scope_id = self.per_expr.get(&expr_id).unwrap_or_else(|| {
            unreachable!("invariant violated: expression should have been assigned a scope.");
        });
        *scope_id
    }

    pub fn type_scope(&self, type_id: TypeId) -> ScopeId {
        let scope_id = self.per_type.get(&type_id).unwrap_or_else(|| {
            unreachable!("impossible: type should have been assigned a scope.");
        });
        *scope_id
    }

    pub fn ancestors(&self, scope_id: ScopeId) -> impl Iterator<Item = &ScopeData> {
        iter::successors(Some(scope_id), |&i| self.scope_data[i].parent)
            .map(|i| &self.scope_data[i])
    }
}

/// Resolution information for a data constructor.
#[derive(Debug, PartialEq, Eq)]
pub struct ConstructorResolution {
    pub file_id: FileId,
    pub data_id: DataGroupId,
    pub constructor_id: AstId<ast::DataConstructor>,
}

/// Resolution information for a type constructor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeConstructorResolution {
    pub file_id: FileId,
    pub kind: TypeConstructorKind,
}

/// Determines the kind of the type constructor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeConstructorKind {
    Class(ClassGroupId),
    Data(DataGroupId),
}

/// Resolution information for a type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeVariableResolution {
    pub file_id: FileId,
    pub kind: TypeVariableKind,
}

/// Determines the provenance of a type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeVariableKind {
    Class(ClassGroupId),
    Data(DataGroupId),
    Type(TypeId),
}

/// Resolution information for a variable expression.
#[derive(Debug, PartialEq, Eq)]
pub enum VariableResolution {
    Binder(BinderId),
    LetName(LetNameId),
    ValueImported(InFile<ValueGroupId>),
    ValueLocal(ValueGroupId),
}

/// Associates surface IDs to resolution information.
#[derive(Debug, PartialEq, Eq)]
pub struct ResolveInfo {
    pub imports: Vec<FileId>,
    pub per_constructor_binder: FxHashMap<BinderId, ConstructorResolution>,
    pub per_constructor_expr: FxHashMap<ExprId, ConstructorResolution>,
    pub per_type_type: FxHashMap<TypeId, TypeConstructorResolution>,
    pub per_variable_type: FxHashMap<TypeId, TypeVariableResolution>,
    pub per_variable_expr: FxHashMap<ExprId, VariableResolution>,
}

impl ResolveInfo {
    pub fn new(imports: Vec<FileId>) -> ResolveInfo {
        let per_constructor_binder = FxHashMap::default();
        let per_constructor_expr = FxHashMap::default();
        let per_type_type = FxHashMap::default();
        let per_variable_type = FxHashMap::default();
        let per_variable_expr = FxHashMap::default();
        ResolveInfo {
            imports,
            per_constructor_binder,
            per_constructor_expr,
            per_type_type,
            per_variable_type,
            per_variable_expr,
        }
    }
}

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: SurfaceDatabase {
    #[salsa::invoke(collect::file_scope_query)]
    fn file_scope(&self, file_id: FileId) -> Arc<ScopeInfo>;

    #[salsa::invoke(resolve::file_resolve_query)]
    fn file_resolve(&self, file_id: FileId) -> Arc<ResolveInfo>;
}
