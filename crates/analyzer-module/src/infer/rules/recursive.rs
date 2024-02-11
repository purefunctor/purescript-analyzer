//! Implements grouping for recursive declarations in a module.

use itertools::Itertools;
use petgraph::{algo::kosaraju_scc, graphmap::DiGraphMap};

use crate::{
    index::nominal::{DataGroupId, ValueGroupId},
    scope::{ResolveInfo, TypeConstructorKind, VariableResolution},
    surface::{tree::*, visit::*},
};

struct AnalyzeRecursiveGroupCtx<'ast, 'env> {
    arena: &'ast SurfaceArena,
    resolve: &'env ResolveInfo,
    dependent: Option<NodeKind>,
    graph: DiGraphMap<NodeKind, ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum NodeKind {
    DataGroupId(DataGroupId),
    ValueGroupId(ValueGroupId),
}

impl<'ast, 'env> AnalyzeRecursiveGroupCtx<'ast, 'env> {
    fn new(
        arena: &'ast SurfaceArena,
        resolve: &'env ResolveInfo,
    ) -> AnalyzeRecursiveGroupCtx<'ast, 'env> {
        let dependent = None;
        let graph = DiGraphMap::default();
        AnalyzeRecursiveGroupCtx { arena, resolve, dependent, graph }
    }

    fn with_dependent(&mut self, dependent: NodeKind) {
        self.graph.add_node(dependent);
        self.dependent = Some(dependent);
    }
}

impl<'ast> Visitor<'ast> for AnalyzeRecursiveGroupCtx<'ast, '_> {
    fn arena(&self) -> &'ast SurfaceArena {
        self.arena
    }

    fn visit_expr(&mut self, expr_id: ExprId) {
        let Some(dependent) = self.dependent else {
            unreachable!("impossible: dependent is unset!");
        };
        match &self.arena[expr_id] {
            Expr::Variable(_) => {
                if let Some(VariableResolution::Local(value_id)) =
                    self.resolve.per_variable_expr.get(&expr_id)
                {
                    let dependency = NodeKind::ValueGroupId(*value_id);
                    self.graph.add_edge(dependent, dependency, ());
                }
            }
            _ => default_visit_expr(self, expr_id),
        }
    }

    fn visit_type(&mut self, type_id: TypeId) {
        let Some(dependent) = self.dependent else {
            unreachable!("impossible: dependent is unset!");
        };
        match &self.arena[type_id] {
            Type::Constructor(_) => {
                if let Some(type_constructor) = self.resolve.per_type_type.get(&type_id) {
                    let dependency = match type_constructor.kind {
                        TypeConstructorKind::Data(data_id) => NodeKind::DataGroupId(data_id),
                    };
                    self.graph.add_edge(dependent, dependency, ());
                }
            }
            _ => default_visit_type(self, type_id),
        }
    }
}

pub(super) fn recursive_data_groups<'ast, 'env>(
    arena: &'ast SurfaceArena,
    resolve: &'env ResolveInfo,
    data_declarations: impl Iterator<Item = &'ast DataDeclaration>,
) -> Vec<Vec<DataGroupId>> {
    let mut ctx = AnalyzeRecursiveGroupCtx::new(arena, resolve);
    for data_declaration in data_declarations {
        ctx.with_dependent(NodeKind::DataGroupId(data_declaration.id));
        for data_constructor in data_declaration.constructors.values() {
            for field in &data_constructor.fields {
                ctx.visit_type(*field);
            }
        }
    }
    kosaraju_scc(&ctx.graph)
        .into_iter()
        .map(|components| {
            components
                .into_iter()
                .map(|node_kind| {
                    if let NodeKind::DataGroupId(data_group_id) = node_kind {
                        data_group_id
                    } else {
                        unreachable!("impossible: invalid node_kind!")
                    }
                })
                .collect_vec()
        })
        .collect_vec()
}

pub(super) fn recursive_value_groups<'ast, 'env>(
    arena: &'ast SurfaceArena,
    resolve: &'env ResolveInfo,
    value_declarations: impl Iterator<Item = &'ast ValueDeclaration>,
) -> Vec<Vec<ValueGroupId>> {
    let mut ctx = AnalyzeRecursiveGroupCtx::new(arena, resolve);
    for value_declaration in value_declarations {
        ctx.with_dependent(NodeKind::ValueGroupId(value_declaration.id));
        // TODO: Should this be a visitor method instead?
        for equation in &value_declaration.equations {
            match &equation.binding {
                Binding::Unconditional { where_expr } => {
                    ctx.visit_let_bindings(&where_expr.let_bindings);
                    ctx.visit_expr(where_expr.expr_id);
                }
            }
        }
    }
    kosaraju_scc(&ctx.graph)
        .into_iter()
        .map(|components| {
            components
                .into_iter()
                .map(|node_kind| {
                    if let NodeKind::ValueGroupId(value_group_id) = node_kind {
                        value_group_id
                    } else {
                        unreachable!("impossible: invalid node_kind!")
                    }
                })
                .collect_vec()
        })
        .collect_vec()
}
