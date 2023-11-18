//! Implements the name resolution algorithm.

use std::{mem, sync::Arc};

use la_arena::Arena;
use petgraph::{algo::kosaraju_scc, graphmap::DiGraphMap};
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    resolver::ValueGroupId,
    surface::{
        visitor::{default_visit_expr, Visitor},
        Binder, Expr, ExprId, LetBinding, LetNameGroup, LetNameGroupId, Type, WhereExpr,
    },
    ScopeDatabase,
};

use super::{
    BinderKind, ResolutionKind, ScopeKind, ValueGroupResolutions, ValueGroupScope, WithScope,
};

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
    on_let_name_group: Option<(LetNameGroupId, Vec<(LetNameGroupId, BinderKind)>)>,
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
        let on_let_name_group = None;
        ResolveContext {
            expr_arena,
            let_name_group_arena,
            binder_arena,
            type_arena,
            value_scope,
            per_expr,
            on_equation_id,
            on_let_name_group,
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

        if let Some(local_resolution) = local_resolution {
            if let ResolutionKind::LetName(dependency) = local_resolution {
                if let Some((_, dependencies)) = &mut self.on_let_name_group {
                    dependencies.push((dependency, binder_kind));
                }
            }
            self.per_expr.insert(expr_id, local_resolution);
        }
    }

    fn visit_let_bindings(&mut self, let_bindings: &'a [LetBinding]) {
        let mut graph = DiGraphMap::new();

        for let_binding in let_bindings.iter() {
            match let_binding {
                LetBinding::NameGroup { id } => {
                    let previous_state =
                        mem::replace(&mut self.on_let_name_group, Some((*id, vec![])));

                    let let_name_group = &self.let_name_group_arena[*id];
                    let_name_group.equations.iter().for_each(|equation| {
                        equation.binders.iter().for_each(|binder| {
                            self.visit_binder(*binder);
                        });
                        self.visit_binding(&equation.binding);
                    });

                    if let Some((dependent, dependencies)) =
                        mem::replace(&mut self.on_let_name_group, previous_state)
                    {
                        dependencies.into_iter().for_each(|(dependency, binder_kind)| {
                            graph.add_edge(dependent, dependency, binder_kind);
                        });
                    } else {
                        unreachable!("invariant violated: this should not be none, at all.");
                    }
                }
                LetBinding::Pattern { binder, where_expr } => {
                    self.visit_binder(*binder);
                    self.visit_where_expr(where_expr);
                }
            }
        }

        let all_thunk = kosaraju_scc(&graph)
            .into_iter()
            .filter(|components| components.len() > 1)
            .flat_map(|components| {
                components.into_iter().flat_map(|component| graph.edges(component))
            })
            .all(|(_, _, binder_kind)| matches!(binder_kind, BinderKind::Thunk));

        if graph.node_count() != 0 {
            dbg!(all_thunk, graph);
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
