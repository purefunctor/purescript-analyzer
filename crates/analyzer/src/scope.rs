//! Database for local scope information.

use std::sync::Arc;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    lower::{self, BinderId, ExprId},
    FxIndexSet, LowerDatabase,
};

/// Scope information as a linked list.
#[derive(Debug, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    kind: ScopeKind,
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
    LetIn(FxHashMap<SmolStr, ExprId>),
}

/// Scope information within a value declaration.
#[derive(Debug, PartialEq, Eq)]
pub struct ValueDeclarationScope {
    inner: Arena<ScopeData>,
    scope_per_expr: FxHashMap<ExprId, ScopeId>,
}

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: LowerDatabase {
    #[salsa::invoke(ValueDeclarationScope::value_declaration_scope_query)]
    fn value_declaration_scope(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> Arc<ValueDeclarationScope>;
}

impl ValueDeclarationScope {
    pub(crate) fn value_declaration_scope_query(
        db: &dyn ScopeDatabase,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> Arc<ValueDeclarationScope> {
        let value_declaration = db.lower_value_declaration(id);
        let mut context = ScopeCollectContext::new(
            &value_declaration.expr_arena,
            &value_declaration.binder_arena,
        );
        let root_scope_id = context.alloc_scope(ScopeData::new_root());
        context.collect_value_declaration(&value_declaration, root_scope_id);
        Arc::new(context.into_value_declaration_scope())
    }

    pub fn expr_scope(&self, expr_id: ExprId) -> &ScopeData {
        if let Some(scope_id) = self.scope_per_expr.get(&expr_id) {
            &self.inner[*scope_id]
        } else {
            panic!("Invariant violated, ExprId was not assigned a ScopeId");
        }
    }

    pub fn resolve(&self, scope: &ScopeData, name: impl AsRef<str>) -> bool {
        let name = name.as_ref();

        let in_current = match &scope.kind {
            ScopeKind::Root => false,
            ScopeKind::Binders(binders) => binders.contains(name),
            ScopeKind::LetIn(let_in) => let_in.contains_key(name),
        };

        if in_current {
            return true;
        }

        if let Some(parent_id) = scope.parent {
            let parent_scope = &self.inner[parent_id];
            self.resolve(parent_scope, name)
        } else {
            false
        }
    }
}

struct ScopeCollectContext<'a> {
    inner: Arena<ScopeData>,
    scope_per_expr: FxHashMap<ExprId, ScopeId>,
    expr_arena: &'a Arena<lower::Expr>,
    binder_arena: &'a Arena<lower::Binder>,
}

impl<'a> ScopeCollectContext<'a> {
    fn new(
        expr_arena: &'a Arena<lower::Expr>,
        binder_arena: &'a Arena<lower::Binder>,
    ) -> ScopeCollectContext<'a> {
        let inner = Arena::default();
        let scope_per_expr = FxHashMap::default();
        ScopeCollectContext { inner, scope_per_expr, expr_arena, binder_arena }
    }

    fn into_value_declaration_scope(self) -> ValueDeclarationScope {
        ValueDeclarationScope { inner: self.inner, scope_per_expr: self.scope_per_expr }
    }

    fn alloc_scope(&mut self, scope_data: ScopeData) -> ScopeId {
        self.inner.alloc(scope_data)
    }

    fn insert_scope(&mut self, expr_id: ExprId, scope_id: ScopeId) {
        self.scope_per_expr.insert(expr_id, scope_id);
    }

    // FIXME: Extract this to a struct.
    fn collect_binder_names(&self, binder_id: BinderId) -> FxIndexSet<SmolStr> {
        match &self.binder_arena[binder_id] {
            lower::Binder::Constructor { fields, .. } => {
                fields.iter().copied().flat_map(|field| self.collect_binder_names(field)).collect()
            }
            lower::Binder::Literal(literal) => match literal {
                lower::Literal::Array(items) => {
                    items.iter().flat_map(|item| self.collect_binder_names(*item)).collect()
                }
                lower::Literal::Record(items) => items
                    .iter()
                    .flat_map(|item| match item {
                        lower::RecordItem::RecordPun(pun) => {
                            let mut names = FxIndexSet::default();
                            names.insert(pun.clone());
                            names
                        }
                        lower::RecordItem::RecordField(_, value) => {
                            self.collect_binder_names(*value)
                        }
                    })
                    .collect(),
                lower::Literal::Int(_) => FxIndexSet::default(),
                lower::Literal::Number(_) => FxIndexSet::default(),
                lower::Literal::String(_) => FxIndexSet::default(),
                lower::Literal::Char(_) => FxIndexSet::default(),
                lower::Literal::Boolean(_) => FxIndexSet::default(),
            },
            lower::Binder::Negative(_) => FxIndexSet::default(),
            lower::Binder::Parenthesized(binder_id) => self.collect_binder_names(*binder_id),
            lower::Binder::Variable(variable) => {
                let mut names = FxIndexSet::default();
                names.insert(variable.as_ref().into());
                names
            }
            lower::Binder::Wildcard => FxIndexSet::default(),
        }
    }

    fn alloc_binders(&mut self, parent: ScopeId, binders: &[BinderId]) -> ScopeId {
        let kind = ScopeKind::Binders(
            binders.iter().flat_map(|binder_id| self.collect_binder_names(*binder_id)).collect(),
        );
        self.alloc_scope(ScopeData::new(parent, kind))
    }

    fn collect_value_declaration(
        &mut self,
        value_declaration: &lower::ValueDeclarationData,
        scope_id: ScopeId,
    ) {
        let scope_id = self.alloc_binders(scope_id, &value_declaration.binders);
        self.collect_binding(&value_declaration.binding, scope_id);
    }

    fn collect_binding(&mut self, binding: &lower::Binding, scope_id: ScopeId) {
        match binding {
            lower::Binding::Unconditional { where_expr } => {
                self.collect_where_expr(where_expr, scope_id);
            }
        }
    }

    fn collect_where_expr(&mut self, where_expr: &lower::WhereExpr, scope_id: ScopeId) {
        self.collect_expr(where_expr.expr_id, scope_id)
    }

    fn collect_expr(&mut self, expr_id: ExprId, scope_id: ScopeId) {
        self.insert_scope(expr_id, scope_id);
        match &self.expr_arena[expr_id] {
            lower::Expr::Literal(literal) => match literal {
                lower::Literal::Array(elements) => {
                    for expr_id in elements.iter() {
                        self.collect_expr(*expr_id, scope_id);
                    }
                }
                lower::Literal::Record(items) => {
                    for item in items.iter() {
                        match item {
                            lower::RecordItem::RecordPun(_) => (),
                            lower::RecordItem::RecordField(_, expr_id) => {
                                self.collect_expr(*expr_id, scope_id);
                            }
                        }
                    }
                }
                _ => (),
            },
            _ => (),
        }
    }
}
