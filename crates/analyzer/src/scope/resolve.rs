//! Implements the name resolution algorithm.

use std::{mem, sync::Arc};

use la_arena::Arena;
use petgraph::graphmap::DiGraphMap;
use rustc_hash::FxHashMap;

use crate::{
    id::InFile,
    resolver::ValueGroupId,
    surface::{
        visitor::{default_visit_expr, Visitor},
        Binder, Expr, ExprId, LetBinding, LetName, LetNameId, Type, WhereExpr,
    },
    ScopeDatabase,
};

use super::{
    BinderKind, ResolutionKind, ScopeKind, ValueGroupResolutions, ValueGroupScope, WithScope,
};

pub(crate) struct ResolveContext<'a> {
    // Environment
    expr_arena: &'a Arena<Expr>,
    let_name_arena: &'a Arena<LetName>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    value_scope: &'a WithScope<ValueGroupScope>,
    // Accumulators
    per_expr: FxHashMap<ExprId, ResolutionKind>,
    let_name_graph: DiGraphMap<LetNameId, BinderKind>,
    // State
    on_let_name_id: Option<LetNameId>,
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
        let let_name_graph = DiGraphMap::default();
        let on_let_name_id = None;
        ResolveContext {
            expr_arena,
            let_name_arena,
            binder_arena,
            type_arena,
            value_scope,
            per_expr,
            let_name_graph,
            on_let_name_id,
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

        let mut binder_kind = BinderKind::NoThunk;
        let local_resolution = self.value_scope.ancestors(expr_scope_id).find_map(|scope_data| {
            match &scope_data.kind {
                ScopeKind::Root => None,
                ScopeKind::Binders(names, kind) => {
                    if let BinderKind::Thunk = kind {
                        binder_kind = *kind;
                    }
                    Some(ResolutionKind::Binder(*names.get(name)?))
                }
                ScopeKind::LetBound(names, _) => Some(ResolutionKind::LetName(*names.get(name)?)),
            }
        });

        if let Some(local_resolution @ ResolutionKind::LetName(dependency)) = local_resolution {
            let dependent = self.on_let_name_id.unwrap_or_else(|| unreachable!("Impossible."));
            self.let_name_graph.add_edge(dependent, dependency, binder_kind);
            self.per_expr.insert(expr_id, local_resolution);
        }
    }

    fn visit_let_bindings(&mut self, let_bindings: &'a [LetBinding]) {
        for let_binding in let_bindings.iter() {
            match let_binding {
                LetBinding::Name { id } => {
                    self.let_name_graph.add_node(*id);

                    let previous_let_name_id = mem::replace(&mut self.on_let_name_id, Some(*id));

                    let let_name = &self.let_name_arena[*id];
                    let_name.equations.iter().for_each(|equation| {
                        equation.binders.iter().for_each(|binder| {
                            self.visit_binder(*binder);
                        });
                        self.visit_binding(&equation.binding);
                    });

                    self.on_let_name_id = previous_let_name_id;
                }
                LetBinding::Pattern { binder, where_expr } => {
                    self.visit_binder(*binder);
                    self.visit_where_expr(where_expr);
                }
            }
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
            Expr::LetIn(let_bindings, let_body) => {
                self.visit_let_bindings(let_bindings);
                self.visit_expr(*let_body);
            }
            Expr::Variable(variable) => {
                self.resolve_expr(expr_id, &variable.value);
            }
            _ => default_visit_expr(self, expr_id),
        }
    }

    fn visit_where_expr(&mut self, where_expr: &'a WhereExpr) {
        self.visit_let_bindings(&where_expr.let_bindings);
        self.visit_expr(where_expr.expr_id);
    }
}
