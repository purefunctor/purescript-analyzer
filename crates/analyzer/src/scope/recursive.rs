//! Collects information about recursive things.

use std::{mem, sync::Arc};

use la_arena::Arena;
use petgraph::{algo::tarjan_scc, graphmap::DiGraphMap};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    id::InFile,
    resolver::ValueGroupId,
    surface::{
        visitor::{default_visit_expr, Visitor},
        Binder, Expr, ExprId, LetBinding, LetName, LetNameId, Type, WhereExpr,
    },
    ScopeDatabase,
};

use super::{ResolutionKind, ValueGroupRecursiveLets, ValueGroupResolutions};

pub(crate) struct RecursiveLetsContext<'a> {
    expr_arena: &'a Arena<Expr>,
    let_name_arena: &'a Arena<LetName>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    resolutions: &'a ValueGroupResolutions,
    let_name_graph: DiGraphMap<LetNameId, bool>,
    on_let_name_id: Option<LetNameId>,
}

impl<'a> RecursiveLetsContext<'a> {
    pub(crate) fn new(
        expr_arena: &'a Arena<Expr>,
        let_name_arena: &'a Arena<LetName>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
        resolutions: &'a ValueGroupResolutions,
    ) -> RecursiveLetsContext<'a> {
        let let_name_graph = DiGraphMap::default();
        let on_let_name_id = None;
        RecursiveLetsContext {
            expr_arena,
            let_name_arena,
            binder_arena,
            type_arena,
            resolutions,
            let_name_graph,
            on_let_name_id,
        }
    }

    pub(crate) fn value_recursive_lets_query(
        db: &dyn ScopeDatabase,
        id: InFile<ValueGroupId>,
    ) -> Arc<ValueGroupRecursiveLets> {
        let value_surface = db.value_surface(id);
        let value_resolutions = db.value_resolved(id);

        let mut recursive_let_context = RecursiveLetsContext::new(
            &value_surface.expr_arena,
            &value_surface.let_name_arena,
            &value_surface.binder_arena,
            &value_surface.type_arena,
            &value_resolutions,
        );

        value_surface.value.equations.iter().for_each(|(_, value_equation)| {
            recursive_let_context.visit_value_equation(value_equation);
        });

        let graph = recursive_let_context.let_name_graph;

        let mut normal = FxHashSet::default();
        let mut recursive = FxHashSet::default();
        let mut mutual_groups = vec![];
        let mut group_indices = FxHashMap::default();

        tarjan_scc(&graph).into_iter().for_each(|components| match &components[..] {
            [id] => {
                let is_recursive = graph.edge_weight(*id, *id).is_some_and(|x| *x);
                if is_recursive {
                    recursive.insert(*id);
                } else {
                    normal.insert(*id);
                }
            }
            _ => {
                let index = mutual_groups.len();
                components.iter().for_each(|&id| {
                    group_indices.insert(id, index);
                });
                mutual_groups.push(components);
            }
        });

        Arc::new(ValueGroupRecursiveLets::new(normal, recursive, mutual_groups, group_indices))
    }

    fn visit_let_bindings(&mut self, let_bindings: &'a [LetBinding]) {
        for let_binding in let_bindings {
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

impl<'a> Visitor<'a> for RecursiveLetsContext<'a> {
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
                self.visit_let_bindings(&let_bindings);
                self.visit_expr(*let_body);
            }
            Expr::Variable(_) => {
                if let Some(dependent) = self.on_let_name_id {
                    if let Some(resolution) = self.resolutions.get(expr_id) {
                        if let ResolutionKind::LetName(dependency) = resolution.kind {
                            self.let_name_graph.add_edge(dependent, dependency, resolution.thunked);
                        }
                    }
                }
            }
            _ => default_visit_expr(self, expr_id),
        }
    }

    fn visit_where_expr(&mut self, where_expr: &'a WhereExpr) {
        self.visit_let_bindings(&where_expr.let_bindings);
        self.visit_expr(where_expr.expr_id);
    }
}
