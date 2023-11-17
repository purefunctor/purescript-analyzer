//! Implements the name resolution algorithm.

use std::{mem, sync::Arc};

use la_arena::Arena;
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    surface::{
        visitor::{default_visit_expr, Visitor},
        Binder, Expr, ExprId, LetNameGroup, Type,
    },
    ScopeDatabase,
};

use super::{ResolutionKind, ScopeKind, ValueGroupResolutions, ValueGroupScope, WithScope};

pub(crate) struct ResolveContext<'a> {
    // Environment
    expr_arena: &'a Arena<Expr>,
    let_name_group_arena: &'a Arena<LetNameGroup>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    value_scope: &'a WithScope<ValueGroupScope>,
    // Accumulators
    per_expr: FxHashMap<ExprId, ResolutionKind>,
    // State
    on_equation_id: Option<AstId<ast::ValueEquationDeclaration>>,
}

impl<'a> ResolveContext<'a> {
    pub(crate) fn new(
        expr_arena: &'a Arena<Expr>,
        let_name_group_arena: &'a Arena<LetNameGroup>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
        value_scope: &'a WithScope<ValueGroupScope>,
    ) -> ResolveContext<'a> {
        let per_expr = FxHashMap::default();
        let on_equation_id = None;
        ResolveContext {
            expr_arena,
            let_name_group_arena,
            binder_arena,
            type_arena,
            value_scope,
            per_expr,
            on_equation_id,
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
            &value_surface.let_name_group_arena,
            &value_surface.binder_arena,
            &value_surface.type_arena,
            &value_scope,
        );

        let per_equation = value_surface
            .value
            .equations
            .iter()
            .map(|(value_equation_id, value_equation_ast)| {
                resolve_context.on_equation_id = Some(*value_equation_id);
                resolve_context.visit_value_equation(value_equation_ast);
                (*value_equation_id, resolve_context.take_per_expr())
            })
            .collect();

        Arc::new(ValueGroupResolutions::new(per_equation))
    }

    fn take_per_expr(&mut self) -> FxHashMap<ExprId, ResolutionKind> {
        mem::take(&mut self.per_expr)
    }

    fn resolve_expr(&mut self, expr_id: ExprId, name: impl AsRef<str>) {
        let name = name.as_ref();
        let equation_id = self.on_equation_id.unwrap_or_else(|| {
            unreachable!("invariant violated: caller did not set on_equation_id");
        });
        let expr_scope_id = self.value_scope.expr_scope(equation_id, expr_id);
        let local_resolution = self.value_scope.ancestors(expr_scope_id).find_map(|scope_data| {
            match &scope_data.kind {
                ScopeKind::Root => None,
                ScopeKind::Binders(names, _) => Some(ResolutionKind::Binder(*names.get(name)?)),
                ScopeKind::LetBound(names, _) => Some(ResolutionKind::LetName(*names.get(name)?)),
            }
        });
        if let Some(local_resolution) = local_resolution {
            self.per_expr.insert(expr_id, local_resolution);
        }
    }
}

impl<'a> Visitor<'a> for ResolveContext<'a> {
    fn expr_arena(&self) -> &'a Arena<Expr> {
        self.expr_arena
    }

    fn let_name_group_arena(&self) -> &'a Arena<LetNameGroup> {
        self.let_name_group_arena
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
