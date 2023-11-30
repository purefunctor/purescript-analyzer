//! Implements the name resolution algorithm.

use la_arena::Arena;
use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::{
    id::InFile,
    resolver::ValueGroupId,
    scope::{BinderKind, ResolutionKind, ScopeKind},
    surface::{
        visitor::{default_visit_expr, Visitor},
        Binder, Expr, ExprId, LetName, Type,
    },
    ScopeDatabase,
};

use super::{Resolution, ValueGroupResolutions, ValueGroupScope, WithScope};

pub(crate) struct ResolveContext<'a> {
    expr_arena: &'a Arena<Expr>,
    let_name_arena: &'a Arena<LetName>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    value_scope: &'a WithScope<ValueGroupScope>,
    per_expr: FxHashMap<ExprId, Resolution>,
}

impl<'a> ResolveContext<'a> {
    pub(crate) fn new(
        expr_arena: &'a Arena<Expr>,
        let_name_arena: &'a Arena<LetName>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
        value_scope: &'a WithScope<ValueGroupScope>,
    ) -> ResolveContext<'a> {
        let per_expr = FxHashMap::default();
        ResolveContext {
            expr_arena,
            let_name_arena,
            binder_arena,
            type_arena,
            value_scope,
            per_expr,
        }
    }

    pub(crate) fn value_resolve_query(
        db: &dyn ScopeDatabase,
        id: InFile<ValueGroupId>,
    ) -> Arc<ValueGroupResolutions> {
        let value_surface = db.value_surface(id);
        let value_scope = db.value_scope(id);

        let mut resolve_context = ResolveContext::new(
            &value_surface.expr_arena,
            &value_surface.let_name_arena,
            &value_surface.binder_arena,
            &value_surface.type_arena,
            &value_scope,
        );

        value_surface.value.equations.iter().for_each(|(_, value_equation)| {
            resolve_context.visit_value_equation(value_equation);
        });

        Arc::new(ValueGroupResolutions::new(resolve_context.per_expr))
    }

    fn resolve_expr(&mut self, expr_id: ExprId, name: impl AsRef<str>) {
        let name = name.as_ref();
        let expr_scope_id = self.value_scope.expr_scope(expr_id);

        let mut thunked = false;
        let kind =
            self.value_scope.ancestors(expr_scope_id).find_map(|scope_data| {
                match &scope_data.kind {
                    ScopeKind::Root => None,
                    ScopeKind::Binders(names, kind) => {
                        if let BinderKind::Thunk = kind {
                            thunked = true;
                        }
                        Some(ResolutionKind::Binder(*names.get(name)?))
                    }
                    ScopeKind::LetBound(names, _) => {
                        Some(ResolutionKind::LetName(*names.get(name)?))
                    }
                }
            });

        if let Some(kind) = kind {
            self.per_expr.insert(expr_id, Resolution { thunked, kind });
        }
    }
}

impl<'a> Visitor<'a> for ResolveContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        self.expr_arena
    }

    fn let_name_arena(&self) -> &'a Arena<LetName> {
        self.let_name_arena
    }

    fn binder_arena(&self) -> &'a Arena<Binder> {
        self.binder_arena
    }

    fn type_arena(&self) -> &'a Arena<Type> {
        self.type_arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        match &self.expr_arena[expr_id] {
            Expr::Variable(variable) => {
                self.resolve_expr(expr_id, &variable.value);
            }
            _ => default_visit_expr(self, expr_id),
        }
    }
}
