use la_arena::{Arena, Idx, RawIdx};
use rustc_hash::FxHashMap;

use crate::surface::{
    visitor::{default_visit_binder, default_visit_expr, default_visit_value_equation, Visitor},
    Binder, BinderId, Expr, ExprId, LetBinding, Type, ValueEquation, WhereExpr,
};

use super::{ScopeData, ScopeId, ScopeKind};

pub(crate) struct CollectorContext<'a> {
    expr_arena: &'a Arena<Expr>,
    let_binding_arena: &'a Arena<LetBinding>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,

    pub(crate) scope_arena: Arena<ScopeData>,
    scope_per_expr: FxHashMap<ExprId, ScopeId>,

    current_scope: ScopeId,
    root_scope: ScopeId,
}

impl<'a> CollectorContext<'a> {
    pub(crate) fn new(
        expr_arena: &'a Arena<Expr>,
        let_binding_arena: &'a Arena<LetBinding>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
    ) -> CollectorContext<'a> {
        let mut scope_arena = Arena::default();
        let scope_per_expr = FxHashMap::default();
        let current_scope = scope_arena.alloc(ScopeData::new_root());
        let root_scope = current_scope;
        CollectorContext {
            expr_arena,
            let_binding_arena,
            binder_arena,
            type_arena,
            scope_per_expr,
            scope_arena,
            current_scope,
            root_scope,
        }
    }

    pub(crate) fn take_equation(&mut self) -> FxHashMap<ExprId, ScopeId> {
        self.current_scope = self.root_scope;
        std::mem::take(&mut self.scope_per_expr)
    }
}

impl<'a> Visitor<'a> for CollectorContext<'a> {
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
        self.scope_per_expr.insert(expr_id, self.current_scope);

        match &self.expr_arena[expr_id] {
            Expr::LetIn(let_bindings, let_body) => {
                // What we want:
                //
                // Create binding groups for declarations
                // To not duplicate the scoping logic
                //
                // What we can do:
                //
                // Defer solving binding groups until we need them e.g.
                // in the name resolution step. For example, the following
                // would report the same scope information: LetBindings({ "f", "g" })
                //
                // f = g
                // g = f
                //
                // However, it's not until we resolve `g` that we emit
                // the errors for circularity. Since scope information
                // is already computed at this point, we don't need to
                // duplicate scope-tracking when trying to figure out
                // which names are being used.

                let mut let_bound = FxHashMap::default();
                for let_binding_id in let_bindings.iter() {
                    match &self.let_binding_arena[*let_binding_id] {
                        LetBinding::Name { name, .. } => {
                            let_bound.insert(name.as_ref().into(), *let_binding_id);
                        }
                    }
                }

                let scope_parent = self.current_scope;
                let scope_kind = ScopeKind::LetBound(let_bound);
                self.current_scope =
                    self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));

                for let_binding in let_bindings.iter() {
                    match &self.let_binding_arena[*let_binding] {
                        LetBinding::Name { binding, .. } => {
                            self.visit_binding(binding);
                        }
                    }
                }
                self.visit_expr(*let_body);
            }
            _ => default_visit_expr(self, expr_id),
        }
    }

    fn visit_binder(&mut self, binder_id: BinderId) {
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

    fn visit_value_equation(&mut self, value_equation: &'a ValueEquation) {
        let scope_parent = self.current_scope;
        let scope_kind = ScopeKind::Binders(FxHashMap::default());
        self.current_scope = self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));
        default_visit_value_equation(self, value_equation);
    }

    fn visit_where_expr(&mut self, where_expr: &'a WhereExpr) {
        let mut let_bound = FxHashMap::default();
        for let_binding_id in where_expr.let_bindings.iter() {
            match &self.let_binding_arena[*let_binding_id] {
                LetBinding::Name { name, .. } => {
                    let_bound.insert(name.as_ref().into(), *let_binding_id);
                }
            }
        }

        let scope_parent = self.current_scope;
        let scope_kind = ScopeKind::LetBound(let_bound);
        self.current_scope = self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));

        for let_binding in where_expr.let_bindings.iter() {
            match &self.let_binding_arena[*let_binding] {
                LetBinding::Name { binding, .. } => {
                    self.visit_binding(binding);
                }
            }
        }
        self.visit_expr(where_expr.expr_id);
    }
}
