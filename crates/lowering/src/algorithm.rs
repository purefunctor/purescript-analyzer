mod recursive;

use std::mem;

use indexing::{Index, Relational, TermItem, TermItemId, TypeItem, TypeItemId};
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
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

    fn push_binder_scope(&mut self) -> Option<GraphNodeId> {
        let parent = mem::take(&mut self.graph_scope);
        let bindings = FxHashMap::default();
        let id = self.graph.inner.alloc(GraphNode::Binder { parent, bindings });
        mem::replace(&mut self.graph_scope, Some(id))
    }

    fn insert_type(&mut self, name: &str, id: TypeVariableBindingId) {
        let Some(node) = self.graph_scope else { return };
        let GraphNode::Forall { bindings, .. } = &mut self.graph.inner[node] else { return };

        let name = SmolStr::from(name);
        bindings.insert(name, id);
    }

    fn push_forall_scope(&mut self) -> Option<GraphNodeId> {
        let parent = mem::take(&mut self.graph_scope);
        let bindings = FxHashMap::default();
        let id = self.graph.inner.alloc(GraphNode::Forall { parent, bindings });
        mem::replace(&mut self.graph_scope, Some(id))
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

    fn resolve_term_local(&mut self, name: &str) -> Option<TermResolution> {
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

    fn resolve_type_variable(&mut self, name: &str) -> Option<TypeVariableBindingId> {
        let id = self.graph_scope?;
        self.graph.traverse(id).find_map(|graph| {
            if let GraphNode::Forall { bindings, .. } = graph {
                bindings.get(name).copied()
            } else {
                None
            }
        })
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

fn lower_term_item(s: &mut State, e: &Environment, id: TermItemId, item: &TermItem) {
    let root = e.module.syntax();
    match item {
        TermItem::ClassMember { .. } => (), // See lower_type_item
        TermItem::Constructor { .. } => (), // See lower_type_item
        TermItem::Derive { .. } => (),
        TermItem::Foreign { id } => {
            let cst = &e.source[*id].to_node(root);
            dbg!(cst);
        }
        TermItem::Instance { .. } => (),
        TermItem::Operator { id } => {
            let cst = &e.source[*id].to_node(root);
            dbg!(cst);
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
            s.intermediate.insert_term_item(id, kind);
        }
    }
}

fn lower_type_item(s: &mut State, e: &Environment, id: TypeItemId, item: &TypeItem) {
    let root = e.module.syntax();
    match item {
        TypeItem::Data { signature, equation, .. } => {
            signature.map(|id| {
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            });
            equation.map(|id| {
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            });
            lower_constructors(s, e, id);
        }
        TypeItem::Newtype { signature, equation, .. } => {
            signature.map(|id| {
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            });
            equation.map(|id| {
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            });
            lower_constructors(s, e, id);
        }
        TypeItem::Synonym { signature, equation } => {
            signature.map(|id| {
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            });
            equation.map(|id| {
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            });
        }
        TypeItem::Class { signature, declaration } => {
            signature.map(|id| {
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            });
            declaration.map(|id| {
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            });
            for id in e.relational.class_members_of(id) {
                let TermItem::ClassMember { id } = e.index[id] else {
                    unreachable!("invariant violated: expected TermItem::ClassMember");
                };
                let cst = &e.source[id].to_node(root);
                dbg!(cst);
            }
        }
        TypeItem::Foreign { id } => {
            let cst = &e.source[*id].to_node(root);
            dbg!(cst);
        }
        TypeItem::Operator { id } => {
            let cst = &e.source[*id].to_node(root);
            dbg!(cst);
        }
    }
}

fn lower_constructors(_: &mut State, e: &Environment, id: TypeItemId) {
    let root = e.module.syntax();
    for id in e.relational.constructors_of(id) {
        let TermItem::Constructor { id } = e.index[id] else {
            unreachable!("invariant violated: expected TermItem::Constructor");
        };
        let cst = &e.source[id].to_node(root);
        dbg!(cst);
    }
}
