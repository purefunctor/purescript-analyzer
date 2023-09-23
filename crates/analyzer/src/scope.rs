//! Database for local scope information.

use std::sync::Arc;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    lower::{self, ExprId},
    names::Name,
    LowerDatabase,
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
    Binders(Vec<Name>),
    LetIn(FxHashMap<Name, ExprId>),
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
        let mut context = ScopeCollectContext::new(&value_declaration.expr_arena);
        let root_scope_id = context.alloc_scope(ScopeData::new_root());
        context.collect_value_declaration(&value_declaration, root_scope_id);
        Arc::new(context.as_value_declaration_scope())
    }
}

struct ScopeCollectContext<'a> {
    inner: Arena<ScopeData>,
    scope_per_expr: FxHashMap<ExprId, ScopeId>,
    expr_arena: &'a Arena<lower::Expr>,
}

impl<'a> ScopeCollectContext<'a> {
    fn new(expr_arena: &'a Arena<lower::Expr>) -> ScopeCollectContext<'a> {
        let inner = Arena::default();
        let scope_per_expr = FxHashMap::default();
        ScopeCollectContext { inner, scope_per_expr, expr_arena }
    }

    fn as_value_declaration_scope(self) -> ValueDeclarationScope {
        ValueDeclarationScope { inner: self.inner, scope_per_expr: self.scope_per_expr }
    }

    fn alloc_scope(&mut self, scope_data: ScopeData) -> ScopeId {
        self.inner.alloc(scope_data)
    }

    fn insert_scope(&mut self, expr_id: ExprId, scope_id: ScopeId) {
        self.scope_per_expr.insert(expr_id, scope_id);
    }

    fn alloc_binders(&mut self, parent: ScopeId, binders: &[lower::Binder]) -> ScopeId {
        let kind = ScopeKind::Binders(
            binders
                .iter()
                .map(|binder| match binder {
                    lower::Binder::Variable(variable) => variable.clone(),
                })
                .collect(),
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
            lower::Expr::LetIn { let_bindings, in_expr_id } => {
                let let_in = let_bindings
                    .iter()
                    .map(|let_binding| match let_binding {
                        lower::LetBinding::Name { name, binding } => match binding {
                            lower::Binding::Unconditional { where_expr } => {
                                (name.clone(), where_expr.expr_id)
                            }
                        },
                    })
                    .collect();

                let scope_id = self.alloc_scope(ScopeData::new(scope_id, ScopeKind::LetIn(let_in)));

                for let_binding in let_bindings.iter() {
                    match let_binding {
                        lower::LetBinding::Name { binding, .. } => {
                            self.collect_binding(binding, scope_id);
                        }
                    }
                }

                self.collect_expr(*in_expr_id, scope_id);
            }
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
