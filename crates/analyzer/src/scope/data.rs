use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::AstId,
    surface::{BinderId, ExprId, LetBindingId},
};

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
    Binders(FxHashMap<SmolStr, BinderId>),
    LetBound(FxHashMap<SmolStr, LetBindingId>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroupScope {
    scope_arena: Arena<ScopeData>,
    scope_per_equation: FxHashMap<AstId<ast::ValueEquationDeclaration>, FxHashMap<ExprId, ScopeId>>,
}

impl ValueGroupScope {
    pub(crate) fn new(
        scope_arena: Arena<ScopeData>,
        scope_per_equation: FxHashMap<
            AstId<ast::ValueEquationDeclaration>,
            FxHashMap<ExprId, ScopeId>,
        >,
    ) -> ValueGroupScope {
        ValueGroupScope { scope_arena, scope_per_equation }
    }

    pub fn expr_scope(
        &self,
        equation_id: AstId<ast::ValueEquationDeclaration>,
        expr_id: ExprId,
    ) -> ScopeId {
        if let Some(scope_id) = self
            .scope_per_equation
            .get(&equation_id)
            .and_then(|scope_per_expr| scope_per_expr.get(&expr_id))
        {
            *scope_id
        } else {
            panic!("Invariant violated, ExprId was not assigned a ScopeId");
        }
    }

    pub fn scope_data(&self, scope_id: ScopeId) -> &ScopeData {
        &self.scope_arena[scope_id]
    }

    pub fn resolve(&self, scope_id: ScopeId, name: impl AsRef<str>) -> Option<ScopeId> {
        let name = name.as_ref();
        let mut current = self.scope_data(scope_id);
        loop {
            match &current.kind {
                ScopeKind::Root => return None,
                ScopeKind::Binders(binders) => {
                    if binders.contains_key(name) {
                        return Some(scope_id);
                    }
                }
                ScopeKind::LetBound(let_bound) => {
                    if let_bound.contains_key(name) {
                        return Some(scope_id);
                    }
                }
            }
            if let Some(parent_scope_id) = current.parent {
                current = self.scope_data(parent_scope_id);
            } else {
                return None;
            }
        }
    }
}
