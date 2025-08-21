mod recursive;

use std::{mem, sync::Arc};

use indexing::{
    FullIndexedModule, TermItem, TermItemId, TermItemKind, TypeItem, TypeItemId, TypeItemKind,
    TypeRoleId,
};
use itertools::Itertools;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::cst;

use crate::*;

#[derive(Default)]
pub(crate) struct State {
    pub(crate) intermediate: Intermediate,
    pub(crate) source: LoweringSource,
    pub(crate) graph: LoweringGraph,
    pub(crate) graph_info: LoweringGraphInfo,
    pub(crate) graph_scope: Option<GraphNodeId>,
}

struct Environment<'e> {
    module: &'e cst::Module,
    indexed: &'e FullIndexedModule,
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
        self.graph_scope.replace(id)
    }

    fn push_forall_scope(&mut self) -> Option<GraphNodeId> {
        let parent = mem::take(&mut self.graph_scope);
        let bindings = FxHashMap::default();
        let id = self.graph.inner.alloc(GraphNode::Forall { parent, bindings });
        self.graph_scope.replace(id)
    }

    fn push_implicit_scope(&mut self) -> Option<GraphNodeId> {
        let parent = mem::take(&mut self.graph_scope);
        let collecting = true;
        let bindings = ImplicitBindings::default();
        let id = self.graph.inner.alloc(GraphNode::Implicit { parent, collecting, bindings });
        self.graph_scope.replace(id)
    }

    fn finish_implicit_scope(&mut self) {
        let Some(id) = self.graph_scope else { return };
        let GraphNode::Implicit { collecting, .. } = &mut self.graph.inner[id] else { return };
        *collecting = false;
    }

    fn resolve_deferred(
        &mut self,
        domain: Domain,
        qualifier: Option<SmolStr>,
        name: Option<SmolStr>,
    ) -> DeferredResolutionId {
        self.graph.deferred.alloc(DeferredResolution { domain, qualifier, name })
    }

    fn resolve_term(
        &mut self,
        qualifier: Option<SmolStr>,
        name: Option<SmolStr>,
    ) -> Option<TermResolution> {
        if qualifier.is_some() {
            let r = self.resolve_deferred(Domain::Term, qualifier, name);
            Some(TermResolution::Deferred(r))
        } else {
            let name = name?;
            self.resolve_term_local(&name).or_else(|| {
                let r = self.resolve_deferred(Domain::Term, None, Some(name));
                Some(TermResolution::Deferred(r))
            })
        }
    }

    fn resolve_term_local(&self, name: &str) -> Option<TermResolution> {
        let id = self.graph_scope?;
        self.graph.traverse(id).find_map(|(_, graph)| match graph {
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

    fn resolve_type_variable(&mut self, id: TypeId, name: &str) -> Option<TypeVariableResolution> {
        let node = self.graph_scope?;
        if let GraphNode::Implicit { collecting, bindings, .. } = &mut self.graph.inner[node] {
            if *collecting {
                let id = bindings.bind(name, id);
                Some(TypeVariableResolution::Implicit { binding: true, node, id })
            } else {
                let id = bindings.get(name)?;
                Some(TypeVariableResolution::Implicit { binding: false, node, id })
            }
        } else {
            self.graph.traverse(node).find_map(|(node, graph)| match graph {
                GraphNode::Forall { bindings, .. } => {
                    bindings.get(name).copied().map(TypeVariableResolution::Forall)
                }
                GraphNode::Implicit { bindings, .. } => {
                    let id = bindings.get(name)?;
                    Some(TypeVariableResolution::Implicit { binding: false, node, id })
                }
                _ => None,
            })
        }
    }
}

pub(super) fn lower_module(module: &cst::Module, indexed: &FullIndexedModule) -> State {
    let mut state = State::default();
    let environment = Environment { module, indexed };

    for (id, item) in environment.indexed.items.iter_terms() {
        state.with_scope(|state| {
            lower_term_item(state, &environment, id, item);
        });
    }

    for (id, item) in environment.indexed.items.iter_types() {
        state.with_scope(|state| {
            lower_type_item(state, &environment, id, item);
        })
    }

    state
}

fn lower_term_item(s: &mut State, e: &Environment, item_id: TermItemId, item: &TermItem) {
    let root = e.module.syntax();
    match &item.kind {
        TermItemKind::ClassMember { .. } => (), // See lower_type_item
        TermItemKind::Constructor { .. } => (), // See lower_type_item
        TermItemKind::Derive { id } => {
            let cst = &e.indexed.source[*id].to_node(root);

            let Some((id, qualifier, name)) = cst.instance_head().and_then(|cst| {
                let cst = cst.qualified()?;
                Some(recursive::lower_qualified_name(
                    s,
                    Domain::Type,
                    &cst,
                    cst::QualifiedName::upper,
                ))
            }) else {
                todo!("figure out");
            };

            let resolution = s.resolve_deferred(Domain::Type, qualifier, name);

            let arguments = cst
                .instance_head()
                .map(|cst| {
                    s.push_implicit_scope();
                    let arguments =
                        cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect();
                    s.finish_implicit_scope();
                    arguments
                })
                .unwrap_or_default();

            let constraints = cst
                .instance_constraints()
                .map(|cst| cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect())
                .unwrap_or_default();

            let kind = TermItemIr::Derive { resolution, id, constraints, arguments };
            s.intermediate.insert_term_item(item_id, kind);
        }
        TermItemKind::Foreign { id } => {
            let cst = &e.indexed.source[*id].to_node(root);
            let signature = cst.ty().map(|t| recursive::lower_type(s, e, &t));
            let kind = TermItemIr::Foreign { signature };
            s.intermediate.insert_term_item(item_id, kind);
        }
        TermItemKind::Instance { id } => {
            let cst = &e.indexed.source[*id].to_node(root);

            let Some((id, qualifier, name)) = cst.instance_head().and_then(|cst| {
                let cst = cst.qualified()?;
                Some(recursive::lower_qualified_name(
                    s,
                    Domain::Type,
                    &cst,
                    cst::QualifiedName::upper,
                ))
            }) else {
                todo!("figureout")
            };

            let resolution = s.resolve_deferred(Domain::Type, qualifier, name);

            let arguments = cst
                .instance_head()
                .map(|cst| {
                    s.push_implicit_scope();
                    let arguments: Arc<[_]> =
                        cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect();
                    s.finish_implicit_scope();
                    arguments
                })
                .unwrap_or_default();

            let constraints = cst
                .instance_constraints()
                .map(|cst| cst.children().map(|cst| recursive::lower_type(s, e, &cst)).collect())
                .unwrap_or_default();

            let members = cst
                .instance_statements()
                .map(|cst| lower_instance_statements(s, e, &cst))
                .unwrap_or_default();

            let kind = TermItemIr::Instance { resolution, id, constraints, arguments, members };
            s.intermediate.insert_term_item(item_id, kind);
        }
        TermItemKind::Operator { id } => {
            let cst = &e.indexed.source[*id].to_node(root);

            let Some((id, qualifier, name)) = cst.qualified().map(|cst| {
                recursive::lower_qualified_name(s, Domain::Term, &cst, cst::QualifiedName::lower)
            }) else {
                todo!("figureout")
            };

            let resolution = s.resolve_deferred(Domain::Term, qualifier, name);

            let associativity = cst
                .infix()
                .map(|_| Associativity::None)
                .or_else(|| cst.infixl().map(|_| Associativity::Left))
                .or_else(|| cst.infixr().map(|_| Associativity::Right));
            let precedence = cst.precedence().and_then(|t| t.text().parse().ok());

            let kind = TermItemIr::Operator { resolution, id, associativity, precedence };
            s.intermediate.insert_term_item(item_id, kind);
        }
        TermItemKind::Value { signature, equations } => {
            let signature = signature.and_then(|id| {
                let cst = e.indexed.source[id].to_node(root);
                cst.signature().map(|t| recursive::lower_forall(s, e, &t))
            });
            let equations = equations
                .iter()
                .map(|id| {
                    let cst = e.indexed.source[*id].to_node(root);
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
    match &item.kind {
        TypeItemKind::Data { signature, equation, role } => {
            let signature = signature.and_then(|id| {
                let cst = &e.indexed.source[id].to_node(root);
                s.push_forall_scope();
                cst.ty().map(|t| recursive::lower_forall(s, e, &t))
            });

            let data = equation.map(|id| {
                let cst = &e.indexed.source[id].to_node(root);

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
        TypeItemKind::Newtype { signature, equation, role } => {
            let signature = signature.and_then(|id| {
                let cst = &e.indexed.source[id].to_node(root);
                s.push_forall_scope();
                cst.ty().map(|t| recursive::lower_forall(s, e, &t))
            });

            let newtype = equation.map(|id| {
                let cst = &e.indexed.source[id].to_node(root);

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
        TypeItemKind::Synonym { signature, equation } => {
            let signature = signature.and_then(|id| {
                let cst = &e.indexed.source[id].to_node(root);
                s.push_forall_scope();
                cst.ty().map(|t| recursive::lower_forall(s, e, &t))
            });

            let synonym = equation.map(|id| {
                let cst = &e.indexed.source[id].to_node(root);

                s.push_forall_scope();
                let variables = cst
                    .children()
                    .map(|cst| recursive::lower_type_variable_binding(s, e, &cst))
                    .collect();

                let ty = cst.ty().map(|cst| recursive::lower_type(s, e, &cst));

                SynonymIr { variables, ty }
            });

            let kind = TypeItemIr::SynonymGroup { signature, synonym };
            s.intermediate.insert_type_item(item_id, kind);
        }
        TypeItemKind::Class { signature, declaration } => {
            let signature = signature.and_then(|id| {
                let cst = &e.indexed.source[id].to_node(root);
                s.push_forall_scope();
                cst.ty().map(|t| recursive::lower_forall(s, e, &t))
            });

            let class = declaration.map(|id| {
                let cst = &e.indexed.source[id].to_node(root);

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
        TypeItemKind::Foreign { id } => {
            let cst = &e.indexed.source[*id].to_node(root);
            let signature = cst.ty().map(|t| recursive::lower_type(s, e, &t));

            let kind = TypeItemIr::Foreign { signature };
            s.intermediate.insert_type_item(item_id, kind);
        }
        TypeItemKind::Operator { id } => {
            let cst = &e.indexed.source[*id].to_node(root);

            let Some((id, qualifier, name)) = cst.qualified().map(|cst| {
                recursive::lower_qualified_name(s, Domain::Type, &cst, cst::QualifiedName::upper)
            }) else {
                todo!("figureout")
            };

            let resolution = s.resolve_deferred(Domain::Type, qualifier, name);

            let associativity = cst
                .infix()
                .map(|_| Associativity::None)
                .or_else(|| cst.infixl().map(|_| Associativity::Left))
                .or_else(|| cst.infixr().map(|_| Associativity::Right));
            let precedence = cst.precedence().and_then(|t| t.text().parse().ok());

            let kind = TypeItemIr::Operator { resolution, id, associativity, precedence };
            s.intermediate.insert_type_item(item_id, kind);
        }
    }
}

fn lower_constructors(s: &mut State, e: &Environment, id: TypeItemId) {
    let root = e.module.syntax();
    for item_id in e.indexed.pairs.data_constructors(id) {
        let TermItemKind::Constructor { id } = e.indexed.items[item_id].kind else {
            unreachable!("invariant violated: expected TermItemKind::Constructor");
        };

        let cst = &e.indexed.source[id].to_node(root);
        let arguments = cst.children().map(|t| recursive::lower_type(s, e, &t)).collect();

        let kind = TermItemIr::Constructor { arguments };
        s.intermediate.insert_term_item(item_id, kind);
    }
}

fn lower_class_members(s: &mut State, e: &Environment, id: TypeItemId) {
    let root = e.module.syntax();
    for item_id in e.indexed.pairs.class_members(id) {
        let TermItemKind::ClassMember { id } = e.indexed.items[item_id].kind else {
            unreachable!("invariant violated: expected TermItemKind::ClassMember");
        };

        let cst = &e.indexed.source[id].to_node(root);
        let signature = cst.ty().map(|t| recursive::lower_type(s, e, &t));

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
                    cst.ty().map(|t| recursive::lower_forall(s, e, &t))
                });
                let equations = equations
                    .iter()
                    .map(|&id| {
                        let cst = s.source[id].to_node(root);
                        recursive::lower_equation_like(
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
    let cst = &e.indexed.source[id].to_node(root);
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
