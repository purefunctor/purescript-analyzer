mod recursive;

use std::{mem, sync::Arc};

use indexing::{Index, Relational, TermItem, TermItemId, TypeItem, TypeItemId, TypeRoleId};
use itertools::Itertools;
use recursive::lower_equation_like;
use rowan::ast::AstNode;
use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;
use syntax::cst;

use crate::*;

#[derive(Default)]
pub(super) struct State {
    pub(super) intermediate: Intermediate,
    pub(super) source: LoweringSource,
    pub(super) graph: Graph,
    pub(super) graph_info: GraphNodeInfo,
    pub(super) graph_scope: Option<GraphNodeId>,
}

struct Environment<'e> {
    module: &'e cst::Module,
    index: &'e Index,
    relational: &'e Relational,
    source: &'e indexing::IndexingSource,
}

impl State {
    fn with_scope<T>(&mut self, mut f: impl FnMut(&mut State) -> T) -> T {
        let graph_scope = self.graph_scope;
        let result = f(self);
        self.graph_scope = graph_scope;
        result
    }

    fn associate_binder_info(&mut self, id: BinderId, kind: BinderKind) {
        self.intermediate.insert_binder_kind(id, kind);
        let Some(node) = self.graph_scope else { return };
        self.graph_info.insert_bd(id, node);
    }

    fn associate_type_info(&mut self, id: TypeId, kind: TypeKind) {
        self.intermediate.insert_type_kind(id, kind);
        let Some(node) = self.graph_scope else { return };
        self.graph_info.insert_ty(id, node);
    }

    fn insert_binder(&mut self, name: &str, id: BinderId) {
        let Some(node) = self.graph_scope else { return };
        let GraphNode::Binder { bindings, .. } = &mut self.graph.inner[node] else { return };

        let name = SmolStr::from(name);
        bindings.insert(name, id);
    }

    fn insert_bound_variable(&mut self, name: &str, id: TypeVariableBindingId) {
        let Some(node) = self.graph_scope else { return };
        let GraphNode::Forall { bindings, .. } = &mut self.graph.inner[node] else { return };

        let name = SmolStr::from(name);
        bindings.insert(name, id);
    }

    fn push_binder_scope(&mut self) -> Option<GraphNodeId> {
        let parent = mem::take(&mut self.graph_scope);
        let bindings = FxHashMap::default();
        let id = self.graph.inner.alloc(GraphNode::Binder { parent, bindings });
        mem::replace(&mut self.graph_scope, Some(id))
    }

    fn push_forall_scope(&mut self) -> Option<GraphNodeId> {
        let parent = mem::take(&mut self.graph_scope);
        let bindings = FxHashMap::default();
        let id = self.graph.inner.alloc(GraphNode::Forall { parent, bindings });
        mem::replace(&mut self.graph_scope, Some(id))
    }

    fn push_constraint_scope(&mut self, id: InstanceKind) -> Option<GraphNodeId> {
        let parent = mem::take(&mut self.graph_scope);
        let collecting = true;
        let bindings = FxHashSet::default();
        let id = self.graph.inner.alloc(GraphNode::Constraint { parent, collecting, bindings, id });
        mem::replace(&mut self.graph_scope, Some(id))
    }

    fn finish_constraint_scope(&mut self) {
        let Some(id) = self.graph_scope else { return };
        let GraphNode::Constraint { collecting, .. } = &mut self.graph.inner[id] else { return };
        *collecting = false;
    }

    fn resolve_root(
        &mut self,
        domain: ResolutionDomain,
        qualifier: Option<SmolStr>,
        name: Option<SmolStr>,
    ) -> RootResolutionId {
        self.graph.root.alloc(RootResolution { domain, qualifier, name })
    }

    fn resolve_term(
        &mut self,
        qualifier: Option<SmolStr>,
        name: Option<SmolStr>,
    ) -> Option<TermResolution> {
        if qualifier.is_some() {
            let r = self.resolve_root(ResolutionDomain::Term, qualifier, name);
            Some(TermResolution::Root(r))
        } else {
            let name = name?;
            self.resolve_term_local(&name).or_else(|| {
                let r = self.resolve_root(ResolutionDomain::Term, None, Some(name));
                Some(TermResolution::Root(r))
            })
        }
    }

    fn resolve_term_local(&self, name: &str) -> Option<TermResolution> {
        let id = self.graph_scope?;
        self.graph.traverse(id).find_map(|graph| match graph {
            GraphNode::Binder { bindings, .. } => {
                let r = *bindings.get(name)?;
                Some(TermResolution::Binder(r))
            }
            GraphNode::Let { bindings, .. } => {
                let r = bindings.get(name)?.clone();
                Some(TermResolution::Let(r))
            }
            _ => None,
        })
    }

    fn resolve_type_variable(&mut self, name: &str) -> Option<TypeVariableResolution> {
        let id = self.graph_scope?;
        if let GraphNode::Constraint { collecting, bindings, id, .. } = &mut self.graph.inner[id] {
            if *collecting {
                let name = SmolStr::from(name);
                bindings.insert(name);
                Some(TypeVariableResolution::InstanceBinder)
            } else if bindings.contains(name) {
                Some(TypeVariableResolution::Instance(*id))
            } else {
                None
            }
        } else {
            self.graph.traverse(id).find_map(|graph| match graph {
                GraphNode::Forall { bindings, .. } => {
                    bindings.get(name).copied().map(TypeVariableResolution::Forall)
                }
                GraphNode::Constraint { bindings, id, .. } => {
                    if bindings.contains(name) {
                        Some(TypeVariableResolution::Instance(*id))
                    } else {
                        None
                    }
                }
                _ => None,
            })
        }
    }
}

pub(super) fn lower_module(
    module: &cst::Module,
    index: &indexing::Index,
    relational: &indexing::Relational,
    source: &indexing::IndexingSource,
) -> State {
    let mut state = State::default();
    let environment = Environment { module, index, relational, source };

    for (id, item) in environment.index.iter_term_item() {
        state.with_scope(|state| {
            lower_term_item(state, &environment, id, item);
        });
    }

    for (id, item) in environment.index.iter_type_item() {
        state.with_scope(|state| {
            lower_type_item(state, &environment, id, item);
        })
    }

    state
}

fn lower_term_item(s: &mut State, e: &Environment, item_id: TermItemId, item: &TermItem) {
    let root = e.module.syntax();
    match item {
        TermItem::ClassMember { .. } => (), // See lower_type_item
        TermItem::Constructor { .. } => (), // See lower_type_item
        TermItem::Derive { id } => {
            let cst = &e.source[*id].to_node(root);

            let (qualifier, name) = cst
                .instance_head()
                .and_then(|cst| {
                    cst.qualified()
                        .map(|cst| recursive::lower_qualified_name(&cst, cst::QualifiedName::upper))
                })
                .unwrap_or_default();

            let resolution = s.resolve_root(ResolutionDomain::Type, qualifier, name);

            let arguments = cst
                .instance_head()
                .map(|cst| {
                    s.push_constraint_scope(InstanceKind::Derive(*id));
                    cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect()
                })
                .unwrap_or_default();

            let constraints = cst
                .instance_constraints()
                .map(|cst| {
                    s.finish_constraint_scope();
                    cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect()
                })
                .unwrap_or_default();

            let kind = TermItemIr::Derive { resolution, constraints, arguments };
            s.intermediate.insert_term_item(item_id, kind);
        }
        TermItem::Foreign { id } => {
            let cst = &e.source[*id].to_node(root);
            let signature = cst.r#type().map(|t| recursive::lower_type(s, e, &t));
            let kind = TermItemIr::Foreign { signature };
            s.intermediate.insert_term_item(item_id, kind);
        }
        TermItem::Instance { id } => {
            let cst = &e.source[*id].to_node(root);

            let (qualifier, name) = cst
                .instance_head()
                .and_then(|cst| {
                    cst.qualified()
                        .map(|cst| recursive::lower_qualified_name(&cst, cst::QualifiedName::upper))
                })
                .unwrap_or_default();

            let resolution = s.resolve_root(ResolutionDomain::Type, qualifier, name);

            let arguments = cst
                .instance_head()
                .map(|cst| {
                    s.push_constraint_scope(InstanceKind::Instance(*id));
                    cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect()
                })
                .unwrap_or_default();

            let constraints = cst
                .instance_constraints()
                .map(|cst| {
                    s.finish_constraint_scope();
                    cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect()
                })
                .unwrap_or_default();

            let members = cst
                .instance_statements()
                .map(|cst| lower_instance_statements(s, e, &cst))
                .unwrap_or_default();

            let kind = TermItemIr::Instance { resolution, constraints, arguments, members };
            s.intermediate.insert_term_item(item_id, kind);
        }
        TermItem::Operator { id } => {
            let cst = &e.source[*id].to_node(root);

            let (qualifier, name) = cst
                .qualified()
                .map(|q| recursive::lower_qualified_name(&q, cst::QualifiedName::lower))
                .unwrap_or_default();

            let resolution = s.resolve_root(ResolutionDomain::Term, qualifier, name);
            let precedence = cst.precedence().and_then(|t| t.text().parse().ok());

            let kind = TermItemIr::Operator { resolution, precedence };
            s.intermediate.insert_term_item(item_id, kind);
        }
        TermItem::Value { signature, equations } => {
            let signature = signature.and_then(|id| {
                let cst = e.source[id].to_node(root);
                cst.signature().map(|t| recursive::lower_forall(s, e, &t))
            });
            let equations = equations
                .iter()
                .map(|id| {
                    let cst = e.source[*id].to_node(root);
                    recursive::lower_equation_like(
                        s,
                        e,
                        cst,
                        cst::ValueEquation::function_binders,
                        cst::ValueEquation::guarded_expression,
                    )
                })
                .collect();
            let kind = TermItemIr::ValueGroup { signature, equations };
            s.intermediate.insert_term_item(item_id, kind);
        }
    }
}

fn lower_type_item(s: &mut State, e: &Environment, item_id: TypeItemId, item: &TypeItem) {
    let root = e.module.syntax();
    match item {
        TypeItem::Data { signature, equation, role } => {
            let signature = signature.and_then(|id| {
                let cst = &e.source[id].to_node(root);
                s.push_forall_scope();
                cst.r#type().map(|t| recursive::lower_forall(s, e, &t))
            });

            let data = equation.map(|id| {
                let cst = &e.source[id].to_node(root);

                s.push_forall_scope();
                let variables = cst
                    .type_variables()
                    .map(|t| recursive::lower_type_variable_binding(s, e, &t))
                    .collect();

                DataIr { variables }
            });

            let roles = role.map(|id| lower_roles(e, id)).unwrap_or_default();

            let kind = TypeItemIr::DataGroup { signature, data, roles };
            s.intermediate.insert_type_item(item_id, kind);

            lower_constructors(s, e, item_id);
        }
        TypeItem::Newtype { signature, equation, role } => {
            let signature = signature.and_then(|id| {
                let cst = &e.source[id].to_node(root);
                s.push_forall_scope();
                cst.r#type().map(|t| recursive::lower_forall(s, e, &t))
            });

            let newtype = equation.map(|id| {
                let cst = &e.source[id].to_node(root);

                s.push_forall_scope();
                let variables = cst
                    .type_variables()
                    .map(|t| recursive::lower_type_variable_binding(s, e, &t))
                    .collect();

                NewtypeIr { variables }
            });

            let roles = role.map(|id| lower_roles(e, id)).unwrap_or_default();

            let kind = TypeItemIr::NewtypeGroup { signature, newtype, roles };
            s.intermediate.insert_type_item(item_id, kind);

            lower_constructors(s, e, item_id);
        }
        TypeItem::Synonym { signature, equation } => {
            let signature = signature.and_then(|id| {
                let cst = &e.source[id].to_node(root);
                s.push_forall_scope();
                cst.r#type().map(|t| recursive::lower_forall(s, e, &t))
            });

            let synonym = equation.map(|id| {
                let cst = &e.source[id].to_node(root);

                s.push_forall_scope();
                let variables = cst
                    .children()
                    .map(|cst| recursive::lower_type_variable_binding(s, e, &cst))
                    .collect();

                let r#type = cst.r#type().map(|cst| recursive::lower_type(s, e, &cst));

                SynonymIr { variables, r#type }
            });

            let kind = TypeItemIr::SynonymGroup { signature, synonym };
            s.intermediate.insert_type_item(item_id, kind);
        }
        TypeItem::Class { signature, declaration } => {
            let signature = signature.and_then(|id| {
                let cst = &e.source[id].to_node(root);
                s.push_forall_scope();
                cst.r#type().map(|t| recursive::lower_forall(s, e, &t))
            });

            let class = declaration.map(|id| {
                let cst = &e.source[id].to_node(root);

                s.push_forall_scope();
                let variables = cst
                    .class_head()
                    .map(|cst| {
                        cst.children()
                            .map(|cst| recursive::lower_type_variable_binding(s, e, &cst))
                            .collect()
                    })
                    .unwrap_or_default();

                let constraints = cst
                    .class_constraints()
                    .map(|cst| {
                        cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect()
                    })
                    .unwrap_or_default();

                ClassIr { constraints, variables }
            });

            let kind = TypeItemIr::ClassGroup { signature, class };
            s.intermediate.insert_type_item(item_id, kind);

            lower_class_members(s, e, item_id);
        }
        TypeItem::Foreign { id } => {
            let cst = &e.source[*id].to_node(root);
            let signature = cst.r#type().map(|t| recursive::lower_type(s, e, &t));

            let kind = TypeItemIr::Foreign { signature };
            s.intermediate.insert_type_item(item_id, kind);
        }
        TypeItem::Operator { id } => {
            let cst = &e.source[*id].to_node(root);

            let (qualifier, name) = cst
                .qualified()
                .map(|q| recursive::lower_qualified_name(&q, cst::QualifiedName::upper))
                .unwrap_or_default();

            let resolution = s.resolve_root(ResolutionDomain::Type, qualifier, name);
            let precedence = cst.precedence().and_then(|t| t.text().parse().ok());

            let kind = TypeItemIr::Operator { resolution, precedence };
            s.intermediate.insert_type_item(item_id, kind);
        }
    }
}

fn lower_constructors(s: &mut State, e: &Environment, id: TypeItemId) {
    let root = e.module.syntax();
    for item_id in e.relational.constructors_of(id) {
        let TermItem::Constructor { id } = e.index[item_id] else {
            unreachable!("invariant violated: expected TermItem::Constructor");
        };

        let cst = &e.source[id].to_node(root);
        let arguments = cst.children().map(|t| recursive::lower_type(s, e, &t)).collect();

        let kind = TermItemIr::Constructor { arguments };
        s.intermediate.insert_term_item(item_id, kind);
    }
}

fn lower_class_members(s: &mut State, e: &Environment, id: TypeItemId) {
    let root = e.module.syntax();
    for item_id in e.relational.class_members_of(id) {
        let TermItem::ClassMember { id } = e.index[item_id] else {
            unreachable!("invariant violated: expected TermItem::ClassMember");
        };

        let cst = &e.source[id].to_node(root);
        let signature = cst.r#type().map(|t| recursive::lower_type(s, e, &t));

        let kind = TermItemIr::ClassMember { signature };
        s.intermediate.insert_term_item(item_id, kind);
    }
}

fn lower_instance_statements(
    s: &mut State,
    e: &Environment,
    cst: &cst::InstanceStatements,
) -> Arc<[InstanceMemberGroup]> {
    let children = cst.children().chunk_by(|statement| match statement {
        cst::InstanceMemberStatement::InstanceSignatureStatement(s) => s.name_token().map(|t| {
            let text = t.text();
            SmolStr::from(text)
        }),
        cst::InstanceMemberStatement::InstanceEquationStatement(e) => e.name_token().map(|t| {
            let text = t.text();
            SmolStr::from(text)
        }),
    });

    let mut in_scope = FxHashMap::default();
    for (name, mut children) in children.into_iter() {
        let mut signature = None;
        let mut equations = vec![];

        if let Some(statement) = children.next() {
            match statement {
                cst::InstanceMemberStatement::InstanceSignatureStatement(cst) => {
                    let id = s.source.allocate_is(&cst);
                    signature = Some(id);
                }
                cst::InstanceMemberStatement::InstanceEquationStatement(cst) => {
                    let id = s.source.allocate_ie(&cst);
                    equations.push(id);
                }
            }
        }

        children.for_each(|statement| {
            if let cst::InstanceMemberStatement::InstanceEquationStatement(cst) = statement {
                let id = s.source.allocate_ie(&cst);
                equations.push(id);
            }
        });

        if let Some(name) = name {
            in_scope.insert(name, (signature, equations));
        }
    }

    let root = e.module.syntax();
    in_scope
        .into_iter()
        .map(|(_, (signature, equations))| {
            s.with_scope(|s| {
                s.push_forall_scope();
                let signature = signature.and_then(|id| {
                    let cst = s.source[id].to_node(root);
                    cst.r#type().map(|t| recursive::lower_forall(s, e, &t))
                });
                let equations = equations
                    .iter()
                    .map(|&id| {
                        let cst = s.source[id].to_node(root);
                        lower_equation_like(
                            s,
                            e,
                            cst,
                            cst::InstanceEquationStatement::function_binders,
                            cst::InstanceEquationStatement::guarded_expression,
                        )
                    })
                    .collect();
                InstanceMemberGroup { signature, equations }
            })
        })
        .collect()
}

fn lower_roles(e: &Environment, id: TypeRoleId) -> Arc<[Role]> {
    let root = e.module.syntax();
    let cst = &e.source[id].to_node(root);
    cst.children()
        .map(|cst| {
            if cst.nominal().is_some() {
                Role::Nominal
            } else if cst.representational().is_some() {
                Role::Representational
            } else if cst.phantom().is_some() {
                Role::Phantom
            } else {
                Role::Unknown
            }
        })
        .collect()
}
