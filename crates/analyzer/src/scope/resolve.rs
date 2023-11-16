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

use super::{ResolutionKind, ValueGroupResolutions, ValueGroupScope, WithScope};

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
        let equation_id = self.on_equation_id.unwrap_or_else(|| {
            unreachable!("invariant violated: caller did not set on_equation_id");
        });

        match &self.expr_arena[expr_id] {
            Expr::Variable(variable) => {
                let resolution_kind =
                    self.value_scope.resolve_name(equation_id, expr_id, &variable.value);
                self.per_expr.insert(expr_id, resolution_kind);
            }
            _ => default_visit_expr(self, expr_id),
        }
    }
}
