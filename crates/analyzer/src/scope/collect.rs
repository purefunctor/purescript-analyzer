//! Implements the scope collection algorithm.

use std::{mem, sync::Arc};

use la_arena::Arena;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::{
    id::InFile,
    resolver::ValueGroupId,
    scope::{BinderKind, ScopeKind},
    surface::{
        visitor::{
            default_visit_binder, default_visit_expr, default_visit_value_equation, Visitor,
        },
        Binder, Expr, ExprId, LetBinding, LetBindingId, Type,
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

    // Suggested algorithm
    //
    // There are two cases that we want to handle while collecting
    // scope information with let-bound names, in particular:
    // 1. Exclusively LetBindingName
    // 2. Interspersed LetBindingPattern
    //
    // With "Exclusively LetBindingName", the idea is to allocate
    // all names as part of a LetBound-scope which is visible to
    // both the body, as well as the rhs of each binding. The
    // responsibility of checking the validity of such name is
    // done down the line instead.
    //
    // With "Interpersed LetBindingPattern", the scope resolution
    // proceeds as normal until a LetBindingPattern is encountered,
    // which acts as a breakpoint and disallows names from referencing
    // each other.
    //
    // For example:
    //
    // f _ = g unit
    // [a, b] = [0, 1]
    // g _ = f unit
    //
    // Per @natefaubion
    // [..], The compiler has a heuristic that if a given binding group has a pattern, it does not topsort it.
    //
    // LetBindingPattern effectively serves as boundaries between let-bound
    // scopes. Note that this is fair game, as such:
    //
    // c = 0
    // [a, b] = [0, 1]
    // f _ = g [a, c]
    // g _ = f [b, c]
    fn visit_let_bindings(
        &mut self,
        let_bindings: impl Iterator<Item = LetBindingId>,
        let_kind: LetKind,
    ) {
        let mut let_bindings = let_bindings;
        let mut current_let_bound = FxHashMap::default();

        let finish_current_let_bound =
            |this: &mut Self, current: &mut FxHashMap<SmolStr, LetBindingId>| {
                if current.is_empty() {
                    return;
                }

                let let_bound = mem::take(current);
                let to_visit: Vec<_> = let_bound
                    .values()
                    .map(|let_binding_id| match &this.let_binding_arena[*let_binding_id] {
                        LetBinding::Name { binding, .. } => binding,
                        LetBinding::Pattern { .. } => unreachable!(),
                    })
                    .collect();

                let scope_parent = this.current_scope;
                let scope_kind = ScopeKind::LetBound(let_bound, let_kind);
                this.current_scope =
                    this.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));

                for binding in to_visit {
                    this.with_reverting_scope(|this| {
                        this.visit_binding(binding);
                    })
                }
            };

        while let Some(let_binding_id) = let_bindings.next() {
            match &self.let_binding_arena[let_binding_id] {
                LetBinding::Name { name, .. } => {
                    current_let_bound.insert(SmolStr::from(name.as_ref()), let_binding_id);
                }
                LetBinding::Pattern { binder, where_expr } => {
                    finish_current_let_bound(self, &mut current_let_bound);

                    let scope_parent = self.current_scope;
                    let scope_kind = ScopeKind::Binders(FxHashMap::default(), BinderKind::NoThunk);
                    self.current_scope =
                        self.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));

                    self.with_reverting_scope(|this| {
                        this.visit_binder(*binder);
                    });

                    self.with_reverting_scope(|this| {
                        this.visit_where_expr(where_expr);
                    });
                }
            }
        }

        finish_current_let_bound(self, &mut current_let_bound);
    }

    fn with_reverting_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let previous = self.current_scope;
        let result = f(self);
        self.current_scope = previous;
        result
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
                (*value_equation_id, collector_context.take_per_expr())
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
                let let_bindings = let_bindings.iter().copied();
                self.visit_let_bindings(let_bindings, LetKind::LetIn);
                self.with_reverting_scope(|this| {
                    this.visit_expr(*let_body);
                });
            }
            _ => default_visit_expr(self, expr_id),
        }
    }

    fn visit_binder(&mut self, binder_id: crate::surface::BinderId) {
        match &self.binder_arena[binder_id] {
            Binder::Variable(variable) => match &mut self.scope_arena[self.current_scope].kind {
                ScopeKind::Binders(binders, _) => {
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
        let let_bindings = where_expr.let_bindings.iter().copied();
        self.visit_let_bindings(let_bindings, LetKind::Where);
        self.with_reverting_scope(|this| {
            this.visit_expr(where_expr.expr_id);
        });
    }

    fn visit_value_equation(&mut self, value_equation: &'a crate::surface::ValueEquation) {
        assert!(self.current_scope == self.root_scope);
        self.with_reverting_scope(|this| {
            let binder_kind = if value_equation.binders.is_empty() {
                BinderKind::NoThunk
            } else {
                BinderKind::Thunk
            };
            let scope_parent = this.current_scope;
            let scope_kind = ScopeKind::Binders(FxHashMap::default(), binder_kind);
            this.current_scope = this.scope_arena.alloc(ScopeData::new(scope_parent, scope_kind));
            default_visit_value_equation(this, value_equation)
        });
    }
}
