//! Implements grouping for recursive declarations in a module.

use itertools::Itertools;
use petgraph::{algo::kosaraju_scc, graphmap::DiGraphMap};

use crate::{
    index::nominal::{ClassGroupId, DataGroupId, ValueGroupId},
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
    ClassGroup(ClassGroupId),
    DataGroup(DataGroupId),
    LetName(LetNameId),
    ValueGroup(ValueGroupId),
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
                if let Some(resolution) = self.resolve.per_variable_expr.get(&expr_id) {
                    match resolution {
                        VariableResolution::LetName(let_id) => {
                            let dependency = NodeKind::LetName(*let_id);
                            self.graph.add_edge(dependent, dependency, ());
                        }
                        VariableResolution::ValueLocal(value_id) => {
                            let dependency = NodeKind::ValueGroup(*value_id);
                            self.graph.add_edge(dependent, dependency, ());
                        }
                        _ => (),
                    }
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
                        TypeConstructorKind::Class(class_id) => NodeKind::ClassGroup(class_id),
                        TypeConstructorKind::Data(data_id) => NodeKind::DataGroup(data_id),
                    };
                    self.graph.add_edge(dependent, dependency, ());
                }
            }
            _ => default_visit_type(self, type_id),
        }
    }
}

pub(super) fn recursive_type_groups<'ast, 'env>(
    arena: &'ast SurfaceArena,
    resolve: &'env ResolveInfo,
    class_declarations: impl Iterator<Item = &'ast ClassDeclaration>,
    data_declarations: impl Iterator<Item = &'ast DataDeclaration>,
) -> Vec<Vec<TypeConstructorKind>> {
    let mut ctx = AnalyzeRecursiveGroupCtx::new(arena, resolve);

    for class_declaration in class_declarations {
        ctx.with_dependent(NodeKind::ClassGroup(class_declaration.id));
        if let Some(signature) = class_declaration.signature {
            ctx.visit_type(signature);
        }
        for member in class_declaration.members.values() {
            ctx.visit_type(member.ty);
        }
    }

    for data_declaration in data_declarations {
        ctx.with_dependent(NodeKind::DataGroup(data_declaration.id));
        if let Some(annotation) = data_declaration.annotation {
            ctx.visit_type(annotation);
        }
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
                .filter_map(|node_kind| match node_kind {
                    NodeKind::ClassGroup(class_id) => Some(TypeConstructorKind::Class(class_id)),
                    NodeKind::DataGroup(data_id) => Some(TypeConstructorKind::Data(data_id)),
                    _ => None,
                })
                .collect_vec()
        })
        .filter(|components| !components.is_empty())
        .collect_vec()
}

pub(super) fn recursive_value_groups<'ast, 'env>(
    arena: &'ast SurfaceArena,
    resolve: &'env ResolveInfo,
    value_declarations: impl Iterator<Item = &'ast ValueDeclaration>,
) -> Vec<Vec<ValueGroupId>> {
    let mut ctx = AnalyzeRecursiveGroupCtx::new(arena, resolve);
    for value_declaration in value_declarations {
        ctx.with_dependent(NodeKind::ValueGroup(value_declaration.id));
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
                .filter_map(|node_kind| {
                    if let NodeKind::ValueGroup(value_group_id) = node_kind {
                        Some(value_group_id)
                    } else {
                        None
                    }
                })
                .collect_vec()
        })
        .filter(|components| !components.is_empty())
        .collect_vec()
}

pub(super) fn recursive_let_names<'ast, 'env>(
    arena: &'ast SurfaceArena,
    resolve: &'env ResolveInfo,
    let_names: impl Iterator<Item = &'ast LetNameId>,
) -> Vec<Vec<LetNameId>> {
    let mut ctx = AnalyzeRecursiveGroupCtx::new(arena, resolve);
    for let_name in let_names {
        ctx.with_dependent(NodeKind::LetName(*let_name));
        let let_name = &arena[*let_name];
        for equation in &let_name.equations {
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
                .filter_map(|node_kind| {
                    if let NodeKind::LetName(let_name_id) = node_kind {
                        Some(let_name_id)
                    } else {
                        None
                    }
                })
                .collect_vec()
        })
        .filter(|components| !components.is_empty())
        .collect_vec()
}
