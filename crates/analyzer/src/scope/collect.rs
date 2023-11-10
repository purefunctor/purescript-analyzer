//! Implements the scope collection algorithm.

use std::sync::Arc;

use la_arena::Arena;
use rustc_hash::FxHashMap;

use crate::{
    id::InFile,
    resolver::ValueGroupId,
    scope::ScopeKind,
    surface::{
        visitor::{
            default_visit_binder, default_visit_expr, default_visit_value_equation, Visitor,
        },
        Binder, Expr, ExprId, LetBinding, Type,
    },
    ScopeDatabase,
};

use super::{LetKind, ScopeData, ScopeId, ValueGroupScope, WithScope};

pub(crate) struct CollectContext<'a> {
    // Environment
    expr_arena: &'a Arena<Expr>,
    let_binding_arena: &'a Arena<LetBinding>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    // Accumulators
    scope_arena: Arena<ScopeData>,
    per_expr: FxHashMap<ExprId, ScopeId>,
    // State
    current_scope: ScopeId,
    root_scope: ScopeId,
}

impl<'a> CollectContext<'a> {
    fn take_per_expr(&mut self) -> FxHashMap<ExprId, ScopeId> {
        std::mem::take(&mut self.per_expr)
    }
}

impl<'a> CollectContext<'a> {
    pub(crate) fn new(
        expr_arena: &'a Arena<Expr>,
        let_binding_arena: &'a Arena<LetBinding>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
    ) -> Self {
        let mut scope_arena = Arena::default();
        let per_expr = FxHashMap::default();

        let current_scope = scope_arena.alloc(ScopeData::default());
        let root_scope = current_scope;

        Self {
            expr_arena,
            let_binding_arena,
            binder_arena,
            type_arena,
            scope_arena,
            per_expr,
            current_scope,
            root_scope,
        }
    }

    pub(crate) fn value_scope_query(
        db: &dyn ScopeDatabase,
        id: InFile<ValueGroupId>,
    ) -> Arc<WithScope<ValueGroupScope>> {
        let group_data = db.value_surface(id);

        let mut collector_context = CollectContext::new(
            &group_data.expr_arena,
            &group_data.let_binding_arena,
            &group_data.binder_arena,
            &group_data.type_arena,
        );

        let per_equation = group_data
            .value
            .equations
            .iter()
            .map(|(value_equation_id, value_equation_ast)| {
                collector_context.visit_value_equation(value_equation_ast);
                let per_expr = collector_context.take_per_expr();
                (*value_equation_id, per_expr)
            })
            .collect();

        Arc::new(WithScope::new(collector_context.scope_arena, ValueGroupScope::new(per_equation)))
    }
}

impl<'a> Visitor<'a> for CollectContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        self.expr_arena
    }

    fn let_binding_arena(&self) -> &'a Arena<LetBinding> {
        self.let_binding_arena
    }

    fn binder_arena(&self) -> &'a Arena<Binder> {
        self.binder_arena
    }

    fn type_arena(&self) -> &'a Arena<Type> {
        self.type_arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        self.per_expr.insert(expr_id, self.current_scope);

        match &self.expr_arena[expr_id] {
            Expr::LetIn(let_bindings, let_body) => {
                let let_bound = let_bindings
                    .iter()
                    .map(|let_binding_id| match &self.let_binding_arena[*let_binding_id] {
                        LetBinding::Name { name, .. } => (name.as_ref().into(), *let_binding_id),
                    })
                    .collect();

                let scope_parent = self.current_scope;
                let scope_kind = ScopeKind::LetBound(let_bound, LetKind::LetIn);
                self.current_scope =
                    self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));

                let_bindings.iter().for_each(|let_binding_id| {
                    match &self.let_binding_arena[*let_binding_id] {
                        LetBinding::Name { binding, .. } => {
                            self.visit_binding(binding);
                        }
                    }
                });
                self.visit_expr(*let_body);

                // `visit_binding` and `visit_expr` might have pushed scopes;
                // as such, we do a rollback to prevent them from leaking.
                self.current_scope = scope_parent;
            }
            _ => default_visit_expr(self, expr_id),
        }
    }

    fn visit_binder(&mut self, binder_id: crate::surface::BinderId) {
        match &self.binder_arena[binder_id] {
            Binder::Variable(variable) => match &mut self.scope_arena[self.current_scope].kind {
                ScopeKind::Binders(binders) => {
                    binders.insert(variable.as_ref().into(), binder_id);
                }
                _ => unreachable!("Invalid traversal state!"),
            },
            _ => {
                default_visit_binder(self, binder_id);
            }
        }
    }

    fn visit_where_expr(&mut self, where_expr: &'a crate::surface::WhereExpr) {
        let let_bound = where_expr
            .let_bindings
            .iter()
            .map(|let_binding_id| match &self.let_binding_arena[*let_binding_id] {
                LetBinding::Name { name, .. } => (name.as_ref().into(), *let_binding_id),
            })
            .collect();

        let scope_parent = self.current_scope;
        let scope_kind = ScopeKind::LetBound(let_bound, LetKind::Where);
        self.current_scope = self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));

        where_expr.let_bindings.iter().for_each(|let_binding_id| {
            match &self.let_binding_arena[*let_binding_id] {
                LetBinding::Name { binding, .. } => {
                    self.visit_binding(binding);
                }
            }
        });
        self.visit_expr(where_expr.expr_id);

        // `visit_binding` and `visit_expr` might have pushed scopes;
        // as such, we do a rollback to prevent them from leaking.
        self.current_scope = scope_parent;
    }

    fn visit_value_equation(&mut self, value_equation: &'a crate::surface::ValueEquation) {
        assert!(self.current_scope == self.root_scope);

        let scope_parent = self.current_scope;
        let scope_kind = ScopeKind::Binders(FxHashMap::default());
        self.current_scope = self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));

        default_visit_value_equation(self, value_equation);

        self.current_scope = scope_parent;
    }
}
