//! Information about binding groups.

use std::{mem, ops, sync::Arc};

use files::FileId;
use la_arena::{Arena, Idx};
use petgraph::{algo::kosaraju_scc, graphmap::DiGraphMap};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    id::InFile,
    resolver::ValueGroupId,
    scope::{ResolutionKind, ValueGroupResolutions},
    surface::{
        visitor::{default_visit_expr, Visitor},
        Binder, Expr, ExprId, LetBinding, LetName, LetNameId, Type, WhereExpr,
    },
};

use super::SugarDatabase;

/// The kind of module-level binding groups.
#[derive(Debug, PartialEq, Eq)]
pub enum BindingGroup {
    /// A non-recursive, singular [`ValueGroupId`].
    Singular(ValueGroupId),
    /// A self-recursive, singular [`ValueGroupId`].
    Recursive(ValueGroupId),
    /// A mutually-recursive collection of [`ValueGroupId`]s.
    MutuallyRecursive(Vec<ValueGroupId>),
}

pub type BindingGroupId = Idx<BindingGroup>;

/// The binding groups at the module-level.
#[derive(Debug, PartialEq, Eq)]
pub struct BindingGroups {
    file_id: FileId,
    inner: Arena<BindingGroup>,
}

impl BindingGroups {
    pub(crate) fn new(file_id: FileId, inner: Arena<BindingGroup>) -> BindingGroups {
        BindingGroups { file_id, inner }
    }

    pub fn iter(&self) -> impl Iterator<Item = (InFile<BindingGroupId>, &BindingGroup)> {
        self.inner.iter().map(|(id, data)| (InFile { file_id: self.file_id, value: id }, data))
    }

    pub fn binding_group_id(&self, id: ValueGroupId) -> InFile<BindingGroupId> {
        // FIXME: O(n) -> O(1)
        self.iter()
            .find_map(|(value_binding_group_id, value_binding_group)| match value_binding_group {
                BindingGroup::Singular(current_id) | BindingGroup::Recursive(current_id) => {
                    if id == *current_id {
                        Some(value_binding_group_id)
                    } else {
                        None
                    }
                }
                BindingGroup::MutuallyRecursive(current_group) => {
                    if current_group.contains(&id) {
                        Some(value_binding_group_id)
                    } else {
                        None
                    }
                }
            })
            .unwrap()
    }

    pub fn binding_group_data(&self, id: BindingGroupId) -> &BindingGroup {
        &self.inner[id]
    }
}

impl ops::Index<BindingGroupId> for BindingGroups {
    type Output = BindingGroup;

    fn index(&self, index: BindingGroupId) -> &Self::Output {
        &self.inner[index]
    }
}

pub(crate) struct BindingGroupsContext<'a> {
    expr_arena: &'a Arena<Expr>,
    let_name_arena: &'a Arena<LetName>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    resolutions: &'a ValueGroupResolutions,
    value_graph: &'a mut DiGraphMap<ValueGroupId, bool>,
    value_group_id: ValueGroupId,
}

impl<'a> BindingGroupsContext<'a> {
    pub(crate) fn new(
        expr_arena: &'a Arena<Expr>,
        let_name_arena: &'a Arena<LetName>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
        resolutions: &'a ValueGroupResolutions,
        value_graph: &'a mut DiGraphMap<ValueGroupId, bool>,
        value_group_id: ValueGroupId,
    ) -> BindingGroupsContext<'a> {
        BindingGroupsContext {
            expr_arena,
            let_name_arena,
            binder_arena,
            type_arena,
            resolutions,
            value_graph,
            value_group_id,
        }
    }

    pub(crate) fn binding_groups_query(
        db: &dyn SugarDatabase,
        file_id: FileId,
    ) -> Arc<BindingGroups> {
        let nominal_map = db.nominal_map(file_id);

        let mut value_graph = DiGraphMap::default();

        for (id, _) in nominal_map.value_groups() {
            let value_surface = db.value_surface(id);
            let value_resolutions = db.value_resolved(id);
            let value_group_id = id.value;
            value_graph.add_node(value_group_id);

            let mut context = BindingGroupsContext::new(
                &value_surface.expr_arena,
                &value_surface.let_name_arena,
                &value_surface.binder_arena,
                &value_surface.type_arena,
                &value_resolutions,
                &mut value_graph,
                value_group_id,
            );

            value_surface.value.equations.iter().for_each(|(_, value_equation)| {
                context.visit_value_equation(value_equation);
            });
        }

        let components = kosaraju_scc(&value_graph);
        let mut inner = Arena::with_capacity(components.len());

        for component in components {
            match &component[..] {
                [id] => {
                    let is_recursive =
                        value_graph.edge_weight(*id, *id).is_some_and(|thunked| *thunked);
                    if is_recursive {
                        inner.alloc(BindingGroup::Recursive(*id));
                    } else {
                        inner.alloc(BindingGroup::Singular(*id));
                    }
                }
                _ => {
                    inner.alloc(BindingGroup::MutuallyRecursive(component));
                }
            }
        }

        Arc::new(BindingGroups::new(file_id, inner))
    }
}

impl<'a> Visitor<'a> for BindingGroupsContext<'a> {
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
            Expr::Variable(_) => {
                if let Some(resolution) = self.resolutions.get(expr_id) {
                    if let ResolutionKind::Local(dependency) = resolution.kind {
                        self.value_graph.add_edge(
                            self.value_group_id,
                            dependency,
                            resolution.thunked,
                        );
                    }
                }
            }
            _ => default_visit_expr(self, expr_id),
        }
    }
}

/// The binding groups at the expression level.
#[derive(Debug, PartialEq, Eq)]
pub struct LetBindingGroups {
    /// Non-recursive let bindings.
    normal: FxHashSet<LetNameId>,
    /// Self-recursive let bindings.
    recursive: FxHashSet<LetNameId>,
    /// Groups of mutually recursive bindings.
    mutual_groups: Vec<Vec<LetNameId>>,
    /// Identifies which group a let binding belongs to.
    group_indices: FxHashMap<LetNameId, usize>,
}

impl LetBindingGroups {
    pub(crate) fn new(
        normal: FxHashSet<LetNameId>,
        recursive: FxHashSet<LetNameId>,
        mutual_groups: Vec<Vec<LetNameId>>,
        group_indices: FxHashMap<LetNameId, usize>,
    ) -> LetBindingGroups {
        LetBindingGroups { normal, recursive, mutual_groups, group_indices }
    }

    pub fn is_normal(&self, id: LetNameId) -> bool {
        self.normal.contains(&id)
    }

    pub fn is_recursive(&self, id: LetNameId) -> bool {
        self.recursive.contains(&id)
    }

    pub fn mutual_group(&self, id: LetNameId) -> Option<&[LetNameId]> {
        let index = *self.group_indices.get(&id)?;
        assert!(index < self.mutual_groups.len());
        Some(&self.mutual_groups[index])
    }
}

pub(crate) struct LetBindingGroupsContext<'a> {
    expr_arena: &'a Arena<Expr>,
    let_name_arena: &'a Arena<LetName>,
    binder_arena: &'a Arena<Binder>,
    type_arena: &'a Arena<Type>,
    resolutions: &'a ValueGroupResolutions,
    let_name_graph: DiGraphMap<LetNameId, bool>,
    on_let_name_id: Option<LetNameId>,
}

impl<'a> LetBindingGroupsContext<'a> {
    pub(crate) fn new(
        expr_arena: &'a Arena<Expr>,
        let_name_arena: &'a Arena<LetName>,
        binder_arena: &'a Arena<Binder>,
        type_arena: &'a Arena<Type>,
        resolutions: &'a ValueGroupResolutions,
    ) -> LetBindingGroupsContext<'a> {
        let let_name_graph = DiGraphMap::default();
        let on_let_name_id = None;
        LetBindingGroupsContext {
            expr_arena,
            let_name_arena,
            binder_arena,
            type_arena,
            resolutions,
            let_name_graph,
            on_let_name_id,
        }
    }

    pub(crate) fn let_binding_groups_query(
        db: &dyn SugarDatabase,
        id: InFile<ValueGroupId>,
    ) -> Arc<LetBindingGroups> {
        let value_surface = db.value_surface(id);
        let value_resolutions = db.value_resolved(id);

        let mut context = LetBindingGroupsContext::new(
            &value_surface.expr_arena,
            &value_surface.let_name_arena,
            &value_surface.binder_arena,
            &value_surface.type_arena,
            &value_resolutions,
        );

        value_surface.value.equations.iter().for_each(|(_, value_equation)| {
            context.visit_value_equation(value_equation);
        });

        let graph = context.let_name_graph;

        let mut normal = FxHashSet::default();
        let mut recursive = FxHashSet::default();
        let mut mutual_groups = vec![];
        let mut group_indices = FxHashMap::default();

        kosaraju_scc(&graph).into_iter().for_each(|components| match &components[..] {
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

        Arc::new(LetBindingGroups::new(normal, recursive, mutual_groups, group_indices))
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

impl<'a> Visitor<'a> for LetBindingGroupsContext<'a> {
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
