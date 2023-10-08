use la_arena::Arena;
use rustc_hash::FxHashMap;

use crate::{
    lower::{
        visitor::{
            default_visit_binder, default_visit_expr, default_visit_value_declaration, Visitor,
        },
        Binder, BinderId, Expr, ExprId, ValueDeclarationData,
    },
    FxIndexSet,
};

use super::{ScopeData, ScopeId, ScopeKind, ValueDeclarationScope};

pub(crate) struct ScopeCollectorContext<'a> {
    expr_arena: &'a Arena<Expr>,
    binder_arena: &'a Arena<Binder>,
    scope_arena: Arena<ScopeData>,
    scope_per_expr: FxHashMap<ExprId, ScopeId>,
    current_scope: ScopeId,
}

impl<'a> ScopeCollectorContext<'a> {
    pub(crate) fn new(
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

    pub(crate) fn into_value_declaration_scope(self) -> ValueDeclarationScope {
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

                let mut let_bound = FxIndexSet::default();
                for let_binding in let_bindings.iter() {
                    match let_binding {
                        crate::lower::LetBinding::Name { name, .. } => {
                            let_bound.insert(name.as_ref().into());
                        }
                    }
                }

                let scope_parent = self.current_scope;
                let scope_kind = ScopeKind::LetBound(let_bound);
                self.current_scope =
                    self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));

                for let_binding in let_bindings.iter() {
                    match let_binding {
                        crate::lower::LetBinding::Name { binding, .. } => match binding {
                            crate::lower::Binding::Unconditional { where_expr } => {
                                self.visit_expr(where_expr.expr_id);
                            }
                        },
                    }
                }
                self.visit_expr(*let_body);
            }
            _ => default_visit_expr(self, expr_id),
        }

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
