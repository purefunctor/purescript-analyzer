mod recursive;

use std::mem;
use std::sync::Arc;

use files::FileId;
use indexing::{
    IndexedModule, TermItem, TermItemId, TermItemKind, TypeItem, TypeItemId, TypeItemKind,
    TypeRoleId,
};
use itertools::Itertools;
use petgraph::prelude::DiGraphMap;
use resolving::ResolvedModule;
use rowan::ast::AstNode;
use rustc_hash::{FxBuildHasher, FxHashMap};
use smol_str::SmolStr;
use stabilizing::{ExpectId, StabilizedModule};
use syntax::cst;

use crate::error::*;
use crate::intermediate::*;
use crate::scope::*;
use crate::source::*;

#[derive(Default)]
pub(crate) struct State {
    pub(crate) info: LoweringInfo,
    pub(crate) graph: LoweringGraph,
    pub(crate) nodes: LoweringGraphNodes,
    pub(crate) graph_scope: Option<GraphNodeId>,

    pub(crate) current_term: Option<TermItemId>,
    pub(crate) current_type: Option<TypeItemId>,

    pub(crate) term_graph: ItemGraph<TermItemId>,
    pub(crate) type_graph: ItemGraph<TypeItemId>,

    pub(crate) errors: Vec<LoweringError>,
}

type ItemGraph<T> = DiGraphMap<T, (), FxBuildHasher>;

struct Context<'c> {
    file_id: FileId,
    root: &'c syntax::SyntaxNode,
    prim: &'c ResolvedModule,
    stabilized: &'c StabilizedModule,
    indexed: &'c IndexedModule,
    resolved: &'c ResolvedModule,
}

impl State {
    fn with_scope<T>(&mut self, mut f: impl FnMut(&mut State) -> T) -> T {
        let graph_scope = self.graph_scope;
        let result = f(self);
        self.graph_scope = graph_scope;
        result
    }

    fn begin_term(&mut self, id: TermItemId) {
        self.current_term = Some(id);
        self.current_type = None;
        self.term_graph.add_node(id);
    }

    fn begin_type(&mut self, id: TypeItemId) {
        self.current_term = None;
        self.current_type = Some(id);
        self.type_graph.add_node(id);
    }

    fn associate_binder_info(&mut self, id: BinderId, kind: BinderKind) {
        self.info.binder_kind.insert(id, kind);
        let Some(node) = self.graph_scope else { return };
        self.nodes.binder_node.insert(id, node);
    }

    fn associate_expression_info(&mut self, id: ExpressionId, kind: ExpressionKind) {
        self.info.expression_kind.insert(id, kind);
        let Some(node) = self.graph_scope else { return };
        self.nodes.expression_node.insert(id, node);
    }

    fn associate_type_info(&mut self, id: TypeId, kind: TypeKind) {
        self.info.type_kind.insert(id, kind);
        let Some(node) = self.graph_scope else { return };
        self.nodes.type_node.insert(id, node);
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

    fn resolve_term_full(
        &mut self,
        context: &Context,
        qualifier: Option<&str>,
        name: &str,
    ) -> Option<TermVariableResolution> {
        if qualifier.is_some() {
            self.resolve_term_reference(context, qualifier, name)
                .map(|(file_id, item_id)| TermVariableResolution::Reference(file_id, item_id))
        } else {
            self.resolve_term_local(name).or_else(|| {
                self.resolve_term_reference(context, qualifier, name)
                    .map(|(file_id, item_id)| TermVariableResolution::Reference(file_id, item_id))
            })
        }
    }

    fn resolve_term_reference(
        &mut self,
        context: &Context,
        qualifier: Option<&str>,
        name: &str,
    ) -> Option<(FileId, TermItemId)> {
        let (file_id, term_id) = context.lookup_term(qualifier, name)?;

        if context.file_id == file_id
            && let Some(current_id) = self.current_term
        {
            self.term_graph.add_edge(current_id, term_id, ());
        }

        Some((file_id, term_id))
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

    fn resolve_type_reference(
        &mut self,
        context: &Context,
        qualifier: Option<&str>,
        name: &str,
    ) -> Option<(FileId, TypeItemId)> {
        let (file_id, type_id) = context.lookup_type(qualifier, name)?;

        if context.file_id == file_id
            && let Some(current_id) = self.current_type
        {
            self.type_graph.add_edge(current_id, type_id, ());
        }

        Some((file_id, type_id))
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
    file_id: FileId,
    module: &cst::Module,
    prim: &ResolvedModule,
    stabilized: &StabilizedModule,
    indexed: &IndexedModule,
    resolved: &ResolvedModule,
) -> State {
    let mut state = State::default();

    let root = module.syntax();
    let context = Context { file_id, root, prim, stabilized, indexed, resolved };

    for (id, item) in context.indexed.items.iter_terms() {
        state.with_scope(|state| {
            state.begin_term(id);
            lower_term_item(state, &context, id, item);
        });
    }

    for (id, item) in context.indexed.items.iter_types() {
        state.with_scope(|state| {
            state.begin_type(id);
            lower_type_item(state, &context, id, item);
        })
    }

    state
}

fn lower_term_item(state: &mut State, context: &Context, item_id: TermItemId, item: &TermItem) {
    match &item.kind {
        TermItemKind::ClassMember { .. } => (), // See lower_type_item

        TermItemKind::Constructor { .. } => (), // See lower_type_item

        TermItemKind::Derive { id } => {
            let cst = context.stabilized.ast_ptr(*id).and_then(|cst| cst.try_to_node(context.root));

            let arguments = cst
                .as_ref()
                .and_then(|cst| {
                    let cst = cst.instance_head()?;
                    state.push_implicit_scope();
                    let arguments = cst
                        .children()
                        .map(|cst| recursive::lower_type(state, context, &cst))
                        .collect();
                    state.finish_implicit_scope();
                    Some(arguments)
                })
                .unwrap_or_default();

            let constraints = cst
                .as_ref()
                .and_then(|cst| {
                    let cst = cst.instance_constraints()?;
                    let constraints = cst
                        .children()
                        .map(|cst| recursive::lower_type(state, context, &cst))
                        .collect();
                    Some(constraints)
                })
                .unwrap_or_default();

            let kind = TermItemIr::Derive { constraints, arguments };
            state.info.term_item.insert(item_id, kind);
        }

        TermItemKind::Foreign { id } => {
            let cst = context.stabilized.ast_ptr(*id).and_then(|cst| cst.try_to_node(context.root));

            let signature = cst.and_then(|cst| {
                let cst = cst.type_()?;
                Some(recursive::lower_type(state, context, &cst))
            });

            let kind = TermItemIr::Foreign { signature };
            state.info.term_item.insert(item_id, kind);
        }

        TermItemKind::Instance { id } => {
            let cst = context.stabilized.ast_ptr(*id).and_then(|cst| cst.try_to_node(context.root));

            let arguments = cst
                .as_ref()
                .and_then(|cst| {
                    let cst = cst.instance_head()?;
                    state.push_implicit_scope();
                    let arguments = cst
                        .children()
                        .map(|cst| recursive::lower_type(state, context, &cst))
                        .collect();
                    state.finish_implicit_scope();
                    Some(arguments)
                })
                .unwrap_or_default();

            let constraints = cst
                .as_ref()
                .and_then(|cst| {
                    let cst = cst.instance_constraints()?;
                    let constraints = cst
                        .children()
                        .map(|cst| recursive::lower_type(state, context, &cst))
                        .collect();
                    Some(constraints)
                })
                .unwrap_or_default();

            let members = cst
                .as_ref()
                .and_then(|cst| {
                    let cst = cst.instance_statements()?;
                    Some(lower_instance_statements(state, context, &cst))
                })
                .unwrap_or_default();

            let kind = TermItemIr::Instance { constraints, arguments, members };
            state.info.term_item.insert(item_id, kind);
        }

        TermItemKind::Operator { id } => {
            let cst = context.stabilized.ast_ptr(*id).and_then(|cst| cst.try_to_node(context.root));

            let associativity = cst.as_ref().and_then(|cst| {
                cst.infix()
                    .map(|_| Associativity::None)
                    .or_else(|| cst.infixl().map(|_| Associativity::Left))
                    .or_else(|| cst.infixr().map(|_| Associativity::Right))
            });

            let precedence = cst.as_ref().and_then(|cst| {
                let cst = cst.precedence()?;
                cst.text().parse().ok()
            });

            let resolution = cst.as_ref().and_then(|cst| {
                let cst = cst.qualified()?;
                let (qualifier, name) =
                    recursive::lower_qualified_name(&cst, cst::QualifiedName::lower)?;
                state.resolve_term_reference(context, qualifier.as_deref(), &name)
            });

            let kind = TermItemIr::Operator { associativity, precedence, resolution };
            state.info.term_item.insert(item_id, kind);
        }

        TermItemKind::Value { signature, equations } => {
            state.term_graph.add_node(item_id);

            let signature = signature.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;
                let cst = cst.signature()?;
                Some(recursive::lower_forall(state, context, &cst))
            });

            let equations = equations
                .iter()
                .filter_map(|id| {
                    let cst = context
                        .stabilized
                        .ast_ptr(*id)
                        .and_then(|cst| cst.try_to_node(context.root))?;
                    Some(recursive::lower_equation_like(
                        state,
                        context,
                        cst,
                        cst::ValueEquation::function_binders,
                        cst::ValueEquation::guarded_expression,
                    ))
                })
                .collect();

            let kind = TermItemIr::ValueGroup { signature, equations };
            state.info.term_item.insert(item_id, kind);
        }
    }
}

fn lower_type_item(state: &mut State, context: &Context, item_id: TypeItemId, item: &TypeItem) {
    match &item.kind {
        TypeItemKind::Data { signature, equation, role } => {
            let signature = signature.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;
                state.push_forall_scope();
                cst.type_().map(|t| recursive::lower_forall(state, context, &t))
            });

            let data = equation.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;

                state.push_forall_scope();
                let variables = cst
                    .type_variables()
                    .map(|t| recursive::lower_type_variable_binding(state, context, &t))
                    .collect();

                Some(DataIr { variables })
            });

            let roles = role.map(|id| lower_roles(context, id)).unwrap_or_default();

            let kind = TypeItemIr::DataGroup { signature, data, roles };
            state.info.type_item.insert(item_id, kind);

            lower_constructors(state, context, item_id);
        }

        TypeItemKind::Newtype { signature, equation, role } => {
            let signature = signature.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;
                state.push_forall_scope();
                cst.type_().map(|t| recursive::lower_forall(state, context, &t))
            });

            let newtype = equation.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;

                state.push_forall_scope();
                let variables = cst
                    .type_variables()
                    .map(|t| recursive::lower_type_variable_binding(state, context, &t))
                    .collect();

                Some(NewtypeIr { variables })
            });

            let roles = role.map(|id| lower_roles(context, id)).unwrap_or_default();

            let kind = TypeItemIr::NewtypeGroup { signature, newtype, roles };
            state.info.type_item.insert(item_id, kind);

            lower_constructors(state, context, item_id);
        }

        TypeItemKind::Synonym { signature, equation } => {
            let signature = signature.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;
                state.push_forall_scope();
                cst.type_().map(|t| recursive::lower_forall(state, context, &t))
            });

            let synonym = equation.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;

                state.push_forall_scope();
                let variables = cst
                    .children()
                    .map(|cst| recursive::lower_type_variable_binding(state, context, &cst))
                    .collect();

                let synonym = cst.type_().map(|cst| recursive::lower_type(state, context, &cst));

                Some(SynonymIr { variables, synonym })
            });

            let kind = TypeItemIr::SynonymGroup { signature, synonym };
            state.info.type_item.insert(item_id, kind);
        }

        TypeItemKind::Class { signature, declaration } => {
            let signature = signature.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;
                state.push_forall_scope();
                cst.type_().map(|t| recursive::lower_forall(state, context, &t))
            });

            let class = declaration.and_then(|id| {
                let cst =
                    context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))?;

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

                Some(ClassIr { constraints, variables })
            });

            let kind = TypeItemIr::ClassGroup { signature, class };
            state.info.type_item.insert(item_id, kind);

            lower_class_members(state, context, item_id);
        }

        TypeItemKind::Foreign { id, role } => {
            let cst = context.stabilized.ast_ptr(*id).and_then(|cst| cst.try_to_node(context.root));

            let signature = cst.as_ref().and_then(|cst| {
                let cst = cst.type_()?;
                Some(recursive::lower_type(state, context, &cst))
            });

            let roles = role.map(|id| lower_roles(context, id)).unwrap_or_default();

            let kind = TypeItemIr::Foreign { signature, roles };
            state.info.type_item.insert(item_id, kind);
        }

        TypeItemKind::Operator { id } => {
            let cst = context.stabilized.ast_ptr(*id).and_then(|cst| cst.try_to_node(context.root));

            let associativity = cst.as_ref().and_then(|cst| {
                cst.infix()
                    .map(|_| Associativity::None)
                    .or_else(|| cst.infixl().map(|_| Associativity::Left))
                    .or_else(|| cst.infixr().map(|_| Associativity::Right))
            });

            let precedence = cst.as_ref().and_then(|cst| {
                let cst = cst.precedence()?;
                cst.text().parse().ok()
            });

            let resolution = cst.as_ref().and_then(|cst| {
                let cst = cst.qualified()?;
                let (qualifier, name) =
                    recursive::lower_qualified_name(&cst, cst::QualifiedName::upper)?;
                state.resolve_type_reference(context, qualifier.as_deref(), &name)
            });

            let kind = TypeItemIr::Operator { associativity, precedence, resolution };
            state.info.type_item.insert(item_id, kind);
        }
    }
}

fn lower_constructors(state: &mut State, context: &Context, id: TypeItemId) {
    for item_id in context.indexed.pairs.data_constructors(id) {
        let TermItemKind::Constructor { id } = context.indexed.items[item_id].kind else {
            unreachable!("invariant violated: expected TermItemKind::Constructor");
        };

        let Some(cst) =
            context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))
        else {
            continue;
        };

        let arguments = cst.children().map(|t| recursive::lower_type(state, context, &t)).collect();

        let kind = TermItemIr::Constructor { arguments };
        state.info.term_item.insert(item_id, kind);
    }
}

fn lower_class_members(state: &mut State, context: &Context, id: TypeItemId) {
    for item_id in context.indexed.pairs.class_members(id) {
        let TermItemKind::ClassMember { id } = context.indexed.items[item_id].kind else {
            unreachable!("invariant violated: expected TermItemKind::ClassMember");
        };

        let Some(cst) =
            context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root))
        else {
            continue;
        };

        let signature = cst.type_().map(|t| recursive::lower_type(state, context, &t));

        let kind = TermItemIr::ClassMember { signature };
        state.info.term_item.insert(item_id, kind);
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
                    let id = context.stabilized.lookup_cst(&cst).expect_id();
                    signature = Some(id);
                }
                cst::InstanceMemberStatement::InstanceEquationStatement(cst) => {
                    let id = context.stabilized.lookup_cst(&cst).expect_id();
                    equations.push(id);
                }
            }
        }

        children.for_each(|statement| {
            if let cst::InstanceMemberStatement::InstanceEquationStatement(cst) = statement {
                let id = context.stabilized.lookup_cst(&cst).expect_id();
                equations.push(id);
            }
        });

        if let Some(name) = name {
            in_scope.insert(name, (signature, equations));
        }
    }

    in_scope
        .into_iter()
        .map(|(_, (signature, equations))| {
            state.with_scope(|state| {
                state.push_forall_scope();
                let signature = signature.and_then(|id| {
                    let cst = context.stabilized.ast_ptr(id)?.try_to_node(context.root)?;
                    cst.type_().map(|t| recursive::lower_forall(state, context, &t))
                });
                let equations = equations
                    .iter()
                    .filter_map(|&id| {
                        let cst = context.stabilized.ast_ptr(id)?.try_to_node(context.root)?;
                        Some(recursive::lower_equation_like(
                            state,
                            context,
                            cst,
                            cst::InstanceEquationStatement::function_binders,
                            cst::InstanceEquationStatement::guarded_expression,
                        ))
                    })
                    .collect();
                InstanceMemberGroup { signature, equations }
            })
        })
        .collect()
}

fn lower_roles(context: &Context, id: TypeRoleId) -> Arc<[Role]> {
    let cst = context.stabilized.ast_ptr(id).and_then(|cst| cst.try_to_node(context.root));
    cst.map(|cst| {
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
    })
    .unwrap_or_default()
}
