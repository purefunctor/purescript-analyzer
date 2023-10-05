//! Database for local scope information.
mod data;

use std::sync::Arc;

use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

pub use data::{ScopeData, ScopeId, ScopeKind, ValueDeclarationScope};

use crate::{
    id::{AstId, InFile},
    lower::{
        visitor::{
            default_visit_binder, default_visit_expr, default_visit_value_declaration, Visitor,
        },
        Binder, BinderId, Expr, ExprId, ValueDeclarationData,
    },
    FxIndexSet, LowerDatabase,
};

#[salsa::query_group(ScopeStorage)]
pub trait ScopeDatabase: LowerDatabase {
    #[salsa::invoke(value_declaration_scope_query)]
    fn value_declaration_scope(
        &self,
        id: InFile<AstId<ast::ValueDeclaration>>,
    ) -> Arc<ValueDeclarationScope>;
}

fn value_declaration_scope_query(
    db: &dyn ScopeDatabase,
    id: InFile<AstId<ast::ValueDeclaration>>,
) -> Arc<ValueDeclarationScope> {
    let value_declaration = db.lower_value_declaration(id);

    let mut context =
        ScopeCollectorContext::new(&value_declaration.expr_arena, &value_declaration.binder_arena);

    context.visit_value_declaration(&value_declaration);

    Arc::new(context.into_value_declaration_scope())
}

struct ScopeCollectorContext<'a> {
    expr_arena: &'a Arena<Expr>,
    binder_arena: &'a Arena<Binder>,
    scope_arena: Arena<ScopeData>,
    scope_per_expr: FxHashMap<ExprId, ScopeId>,
    current_scope: ScopeId,
}

impl<'a> ScopeCollectorContext<'a> {
    fn new(
        expr_arena: &'a Arena<Expr>,
        binder_arena: &'a Arena<Binder>,
    ) -> ScopeCollectorContext<'a> {
        let mut scope_arena = Arena::default();
        let scope_per_expr = FxHashMap::default();
        let current_scope = scope_arena.alloc(ScopeData::new_root());
        ScopeCollectorContext {
            expr_arena,
            binder_arena,
            scope_arena,
            scope_per_expr,
            current_scope,
        }
    }

    fn into_value_declaration_scope(self) -> ValueDeclarationScope {
        ValueDeclarationScope::new(self.scope_arena, self.scope_per_expr)
    }
}

impl<'a> Visitor<'a> for ScopeCollectorContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        self.expr_arena
    }

    fn binder_arena(&self) -> &'a Arena<Binder> {
        self.binder_arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        self.scope_per_expr.insert(expr_id, self.current_scope);
        default_visit_expr(self, expr_id);
    }

    fn visit_binder(&mut self, binder_id: BinderId) {
        match &self.binder_arena[binder_id] {
            Binder::Variable(variable) => match &mut self.scope_arena[self.current_scope].kind {
                ScopeKind::Binders(binders) => {
                    binders.insert(variable.as_ref().into());
                }
                _ => unreachable!("Invalid traversal state!"),
            },
            _ => {
                default_visit_binder(self, binder_id);
            }
        }
    }

    fn visit_value_declaration(&mut self, value_declaration: &'a ValueDeclarationData) {
        let scope_parent = self.current_scope;
        let scope_kind = ScopeKind::Binders(FxIndexSet::default());
        self.current_scope = self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));
        default_visit_value_declaration(self, value_declaration);
    }
}
