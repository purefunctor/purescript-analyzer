mod recursive;

use std::{mem, sync::Arc};

use files::FileId;
use indexing::{
    FullIndexedModule, TermItem, TermItemId, TermItemKind, TypeItem, TypeItemId, TypeItemKind,
    TypeRoleId,
};
use itertools::Itertools;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::cst;

use crate::{ImplicitTypeVariable, *};

#[derive(Default)]
pub(crate) struct State {
    pub(crate) intermediate: Intermediate,
    pub(crate) source: LoweringSource,
    pub(crate) graph: LoweringGraph,
    pub(crate) nodes: LoweringGraphNodes,
    pub(crate) graph_scope: Option<GraphNodeId>,
}

struct Context<'c> {
    module: &'c cst::Module,
    prim: &'c FullResolvedModule,
    indexed: &'c FullIndexedModule,
    resolved: &'c FullResolvedModule,
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
        self.nodes.insert_binder(id, node);
    }

    fn associate_expression_info(&mut self, id: ExpressionId, kind: ExpressionKind) {
        self.intermediate.insert_expression_kind(id, kind);
        let Some(node) = self.graph_scope else { return };
        self.nodes.insert_expression(id, node);
    }

    fn associate_type_info(&mut self, id: TypeId, kind: TypeKind) {
        self.intermediate.insert_type_kind(id, kind);
        let Some(node) = self.graph_scope else { return };
        self.nodes.insert_type_(id, node);
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

    fn resolve_term_reference(
        &self,
        context: &Context,
        qualifier: Option<&str>,
        name: &str,
    ) -> Option<TermVariableResolution> {
        let resolve_reference = || {
            let (file_id, term_id) = context.lookup_term(qualifier, name)?;
            Some(TermVariableResolution::Reference(file_id, term_id))
        };

        if qualifier.is_some() {
            resolve_reference()
        } else {
            self.resolve_term_local(name).or_else(resolve_reference)
        }
    }

    fn resolve_term_local(&self, name: &str) -> Option<TermVariableResolution> {
        let id = self.graph_scope?;
        self.graph.traverse(id).find_map(|(_, graph)| match graph {
            GraphNode::Binder { bindings, .. } => {
                let r = *bindings.get(name)?;
                Some(TermVariableResolution::Binder(r))
            }
            GraphNode::Let { bindings, .. } => {
                let r = bindings.get(name)?.clone();
                Some(TermVariableResolution::Let(r))
            }
            _ => None,
        })
    }

    fn resolve_type_variable(&mut self, id: TypeId, name: &str) -> Option<TypeVariableResolution> {
        let node = self.graph_scope?;
        if let GraphNode::Implicit { collecting, bindings, .. } = &mut self.graph.inner[node] {
            if *collecting {
                let id = bindings.bind(name, id);
                Some(TypeVariableResolution::Implicit(ImplicitTypeVariable {
                    binding: true,
                    node,
                    id,
                }))
            } else {
                let id = bindings.get(name)?;
                Some(TypeVariableResolution::Implicit(ImplicitTypeVariable {
                    binding: false,
                    node,
                    id,
                }))
            }
        } else {
            self.graph.traverse(node).find_map(|(node, graph)| match graph {
                GraphNode::Forall { bindings, .. } => {
                    bindings.get(name).copied().map(TypeVariableResolution::Forall)
                }
                GraphNode::Implicit { bindings, .. } => {
                    let id = bindings.get(name)?;
                    Some(TypeVariableResolution::Implicit(ImplicitTypeVariable {
                        binding: false,
                        node,
                        id,
                    }))
                }
                _ => None,
            })
        }
    }
}

impl Context<'_> {
    fn lookup_term<Q, N>(&self, qualifier: Option<Q>, name: N) -> Option<(FileId, TermItemId)>
    where
        Q: AsRef<str>,
        N: AsRef<str>,
    {
        let qualifier = qualifier.as_ref().map(Q::as_ref);
        let name = name.as_ref();
        self.resolved.lookup_term(self.prim, qualifier, name)
    }

    fn lookup_type<Q, N>(&self, qualifier: Option<Q>, name: N) -> Option<(FileId, TypeItemId)>
    where
        Q: AsRef<str>,
        N: AsRef<str>,
    {
        let qualifier = qualifier.as_ref().map(Q::as_ref);
        let name = name.as_ref();
        self.resolved.lookup_type(self.prim, qualifier, name)
    }
}

pub(super) fn lower_module(
    module: &cst::Module,
    prim: &FullResolvedModule,
    indexed: &FullIndexedModule,
    resolved: &FullResolvedModule,
) -> State {
    let mut state = State::default();
    let environment = Context { module, prim, indexed, resolved };

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

fn lower_term_item(state: &mut State, context: &Context, item_id: TermItemId, item: &TermItem) {
    let root = context.module.syntax();
    match &item.kind {
        TermItemKind::ClassMember { .. } => (), // See lower_type_item
        TermItemKind::Constructor { .. } => (), // See lower_type_item
        TermItemKind::Derive { id } => {
            let cst = &context.indexed.source[*id].to_node(root);

            let arguments = cst
                .instance_head()
                .map(|cst| {
                    state.push_implicit_scope();
                    let arguments = cst
                        .children()
                        .map(|cst| recursive::lower_type(state, context, &cst))
                        .collect();
                    state.finish_implicit_scope();
                    arguments
                })
                .unwrap_or_default();

            let constraints = cst
                .instance_constraints()
                .map(|cst| {
                    cst.children().map(|cst| recursive::lower_type(state, context, &cst)).collect()
                })
                .unwrap_or_default();

            let kind = TermItemIr::Derive { constraints, arguments };
            state.intermediate.insert_term_item(item_id, kind);
        }
        TermItemKind::Foreign { id } => {
            let cst = &context.indexed.source[*id].to_node(root);
            let signature = cst.type_().map(|t| recursive::lower_type(state, context, &t));
            let kind = TermItemIr::Foreign { signature };
            state.intermediate.insert_term_item(item_id, kind);
        }
        TermItemKind::Instance { id } => {
            let cst = &context.indexed.source[*id].to_node(root);

            let arguments = cst
                .instance_head()
                .map(|cst| {
                    state.push_implicit_scope();
                    let arguments: Arc<[_]> = cst
                        .children()
                        .map(|cst| recursive::lower_type(state, context, &cst))
                        .collect();
                    state.finish_implicit_scope();
                    arguments
                })
                .unwrap_or_default();

            let constraints = cst
                .instance_constraints()
                .map(|cst| {
                    cst.children().map(|cst| recursive::lower_type(state, context, &cst)).collect()
                })
                .unwrap_or_default();

            let members = cst
                .instance_statements()
                .map(|cst| lower_instance_statements(state, context, &cst))
                .unwrap_or_default();

            let kind = TermItemIr::Instance { constraints, arguments, members };
            state.intermediate.insert_term_item(item_id, kind);
        }
        TermItemKind::Operator { id } => {
            let cst = &context.indexed.source[*id].to_node(root);

            let associativity = cst
                .infix()
                .map(|_| Associativity::None)
                .or_else(|| cst.infixl().map(|_| Associativity::Left))
                .or_else(|| cst.infixr().map(|_| Associativity::Right));
            let precedence = cst.precedence().and_then(|t| t.text().parse().ok());

            let kind = TermItemIr::Operator { associativity, precedence };
            state.intermediate.insert_term_item(item_id, kind);
        }
        TermItemKind::Value { signature, equations } => {
            let signature = signature.and_then(|id| {
                let cst = context.indexed.source[id].to_node(root);
                cst.signature().map(|t| recursive::lower_forall(state, context, &t))
            });
            let equations = equations
                .iter()
                .map(|id| {
                    let cst = context.indexed.source[*id].to_node(root);
                    recursive::lower_equation_like(
                        state,
                        context,
                        cst,
                        cst::ValueEquation::function_binders,
                        cst::ValueEquation::guarded_expression,
                    )
                })
                .collect();
            let kind = TermItemIr::ValueGroup { signature, equations };
            state.intermediate.insert_term_item(item_id, kind);
        }
    }
}

fn lower_type_item(state: &mut State, context: &Context, item_id: TypeItemId, item: &TypeItem) {
    let root = context.module.syntax();
    match &item.kind {
        TypeItemKind::Data { signature, equation, role } => {
            let signature = signature.and_then(|id| {
                let cst = &context.indexed.source[id].to_node(root);
                state.push_forall_scope();
                cst.type_().map(|t| recursive::lower_forall(state, context, &t))
            });

            let data = equation.map(|id| {
                let cst = &context.indexed.source[id].to_node(root);

                state.push_forall_scope();
                let variables = cst
                    .type_variables()
                    .map(|t| recursive::lower_type_variable_binding(state, context, &t))
                    .collect();

                DataIr { variables }
            });

            let roles = role.map(|id| lower_roles(context, id)).unwrap_or_default();

            let kind = TypeItemIr::DataGroup { signature, data, roles };
            state.intermediate.insert_type_item(item_id, kind);

            lower_constructors(state, context, item_id);
        }
        TypeItemKind::Newtype { signature, equation, role } => {
            let signature = signature.and_then(|id| {
                let cst = &context.indexed.source[id].to_node(root);
                state.push_forall_scope();
                cst.type_().map(|t| recursive::lower_forall(state, context, &t))
            });

            let newtype = equation.map(|id| {
                let cst = &context.indexed.source[id].to_node(root);

                state.push_forall_scope();
                let variables = cst
                    .type_variables()
                    .map(|t| recursive::lower_type_variable_binding(state, context, &t))
                    .collect();

                NewtypeIr { variables }
            });

            let roles = role.map(|id| lower_roles(context, id)).unwrap_or_default();

            let kind = TypeItemIr::NewtypeGroup { signature, newtype, roles };
            state.intermediate.insert_type_item(item_id, kind);

            lower_constructors(state, context, item_id);
        }
        TypeItemKind::Synonym { signature, equation } => {
            let signature = signature.and_then(|id| {
                let cst = &context.indexed.source[id].to_node(root);
                state.push_forall_scope();
                cst.type_().map(|t| recursive::lower_forall(state, context, &t))
            });

            let synonym = equation.map(|id| {
                let cst = &context.indexed.source[id].to_node(root);

                state.push_forall_scope();
                let variables = cst
                    .children()
                    .map(|cst| recursive::lower_type_variable_binding(state, context, &cst))
                    .collect();

                let type_ = cst.type_().map(|cst| recursive::lower_type(state, context, &cst));

                SynonymIr { variables, type_ }
            });

            let kind = TypeItemIr::SynonymGroup { signature, synonym };
            state.intermediate.insert_type_item(item_id, kind);
        }
        TypeItemKind::Class { signature, declaration } => {
            let signature = signature.and_then(|id| {
                let cst = &context.indexed.source[id].to_node(root);
                state.push_forall_scope();
                cst.type_().map(|t| recursive::lower_forall(state, context, &t))
            });

            let class = declaration.map(|id| {
                let cst = &context.indexed.source[id].to_node(root);

                state.push_forall_scope();
                let variables = cst
                    .class_head()
                    .map(|cst| {
                        cst.children()
                            .map(|cst| recursive::lower_type_variable_binding(state, context, &cst))
                            .collect()
                    })
                    .unwrap_or_default();

                let constraints = cst
                    .class_constraints()
                    .map(|cst| {
                        cst.children()
                            .map(|cst| recursive::lower_type(state, context, &cst))
                            .collect()
                    })
                    .unwrap_or_default();

                ClassIr { constraints, variables }
            });

            let kind = TypeItemIr::ClassGroup { signature, class };
            state.intermediate.insert_type_item(item_id, kind);

            lower_class_members(state, context, item_id);
        }
        TypeItemKind::Foreign { id } => {
            let cst = &context.indexed.source[*id].to_node(root);
            let signature = cst.type_().map(|t| recursive::lower_type(state, context, &t));

            let kind = TypeItemIr::Foreign { signature };
            state.intermediate.insert_type_item(item_id, kind);
        }
        TypeItemKind::Operator { id } => {
            let cst = &context.indexed.source[*id].to_node(root);

            let associativity = cst
                .infix()
                .map(|_| Associativity::None)
                .or_else(|| cst.infixl().map(|_| Associativity::Left))
                .or_else(|| cst.infixr().map(|_| Associativity::Right));
            let precedence = cst.precedence().and_then(|t| t.text().parse().ok());

            let kind = TypeItemIr::Operator { associativity, precedence };
            state.intermediate.insert_type_item(item_id, kind);
        }
    }
}

fn lower_constructors(state: &mut State, context: &Context, id: TypeItemId) {
    let root = context.module.syntax();
    for item_id in context.indexed.pairs.data_constructors(id) {
        let TermItemKind::Constructor { id } = context.indexed.items[item_id].kind else {
            unreachable!("invariant violated: expected TermItemKind::Constructor");
        };

        let cst = &context.indexed.source[id].to_node(root);
        let arguments = cst.children().map(|t| recursive::lower_type(state, context, &t)).collect();

        let kind = TermItemIr::Constructor { arguments };
        state.intermediate.insert_term_item(item_id, kind);
    }
}

fn lower_class_members(state: &mut State, context: &Context, id: TypeItemId) {
    let root = context.module.syntax();
    for item_id in context.indexed.pairs.class_members(id) {
        let TermItemKind::ClassMember { id } = context.indexed.items[item_id].kind else {
            unreachable!("invariant violated: expected TermItemKind::ClassMember");
        };

        let cst = &context.indexed.source[id].to_node(root);
        let signature = cst.type_().map(|t| recursive::lower_type(state, context, &t));

        let kind = TermItemIr::ClassMember { signature };
        state.intermediate.insert_term_item(item_id, kind);
    }
}

fn lower_instance_statements(
    state: &mut State,
    context: &Context,
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
                    let id = state.source.allocate_is(&cst);
                    signature = Some(id);
                }
                cst::InstanceMemberStatement::InstanceEquationStatement(cst) => {
                    let id = state.source.allocate_ie(&cst);
                    equations.push(id);
                }
            }
        }

        children.for_each(|statement| {
            if let cst::InstanceMemberStatement::InstanceEquationStatement(cst) = statement {
                let id = state.source.allocate_ie(&cst);
                equations.push(id);
            }
        });

        if let Some(name) = name {
            in_scope.insert(name, (signature, equations));
        }
    }

    let root = context.module.syntax();
    in_scope
        .into_iter()
        .map(|(_, (signature, equations))| {
            state.with_scope(|s| {
                s.push_forall_scope();
                let signature = signature.and_then(|id| {
                    let cst = s.source[id].to_node(root);
                    cst.type_().map(|t| recursive::lower_forall(s, context, &t))
                });
                let equations = equations
                    .iter()
                    .map(|&id| {
                        let cst = s.source[id].to_node(root);
                        recursive::lower_equation_like(
                            s,
                            context,
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

fn lower_roles(context: &Context, id: TypeRoleId) -> Arc<[Role]> {
    let root = context.module.syntax();
    let cst = &context.indexed.source[id].to_node(root);
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
