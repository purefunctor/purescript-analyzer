use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::{lower::ExprId, FxIndexSet};

/// Scope information as a linked list.
#[derive(Debug, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    pub(crate) kind: ScopeKind,
}

impl ScopeData {
    pub(crate) fn new_root() -> ScopeData {
        ScopeData { parent: None, kind: ScopeKind::Root }
    }

    pub(crate) fn new(parent: ScopeId, kind: ScopeKind) -> ScopeData {
        ScopeData { parent: Some(parent), kind }
    }
}

pub type ScopeId = Idx<ScopeData>;

/// The kind of scope information.
#[derive(Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Root,
    Binders(FxIndexSet<SmolStr>),
    LetBound(FxIndexSet<SmolStr>),
}

/// Scope information within a value declaration.
#[derive(Debug, PartialEq, Eq)]
pub struct ValueDeclarationScope {
    scope_arena: Arena<ScopeData>,
    scope_per_expr: FxHashMap<ExprId, ScopeId>,
}

impl ValueDeclarationScope {
    pub(crate) fn new(
        scope_arena: Arena<ScopeData>,
        scope_per_expr: FxHashMap<ExprId, ScopeId>,
    ) -> ValueDeclarationScope {
        ValueDeclarationScope { scope_arena, scope_per_expr }
    }

    pub fn expr_scope(&self, expr_id: ExprId) -> &ScopeData {
        dbg!(&self.scope_arena);
        dbg!(&self.scope_per_expr);
        if let Some(scope_id) = self.scope_per_expr.get(&expr_id) {
            &self.scope_arena[*scope_id]
        } else {
            panic!("Invariant violated, ExprId was not assigned a ScopeId");
        }
    }
}
