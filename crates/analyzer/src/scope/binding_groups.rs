use itertools::Itertools;
use la_arena::Arena;
use nonempty::NonEmpty;
use petgraph::{algo::kosaraju_scc, prelude::DiGraph};
use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;

use crate::lower::{self, ExprId};

#[derive(Default)]
struct LocalBindings {
    inner: NonEmpty<FxHashSet<SmolStr>>,
}

impl LocalBindings {
    fn push(&mut self) {
        self.inner.push(FxHashSet::default());
    }

    fn pop(&mut self) {
        self.inner.pop();
    }

    fn define(&mut self, name: impl AsRef<str>) {
        self.inner.last_mut().insert(name.as_ref().into());
    }

    fn contains(&self, name: impl AsRef<str>) -> bool {
        self.inner.iter().rev().any(|layer| layer.contains(name.as_ref()))
    }
}

struct CollectNameContext<'a> {
    expr_arena: &'a Arena<lower::Expr>,
    collected_names: FxHashSet<SmolStr>,
    local_bindings: LocalBindings,
}

impl<'a> CollectNameContext<'a> {
    fn new(expr_arena: &'a Arena<lower::Expr>) -> CollectNameContext<'a> {
        CollectNameContext {
            expr_arena,
            collected_names: FxHashSet::default(),
            local_bindings: LocalBindings::default(),
        }
    }

    fn finish(self) -> FxHashSet<SmolStr> {
        self.collected_names
    }

    fn collect_name(&mut self, name: impl AsRef<str>) {
        self.collected_names.insert(name.as_ref().into());
    }

    fn collect_binding(&mut self, binding: &lower::Binding) {
        match binding {
            lower::Binding::Unconditional { where_expr } => {
                self.collect_where_expr(where_expr);
            }
        }
    }

    fn collect_where_expr(&mut self, where_expr: &lower::WhereExpr) {
        self.collect_expr(where_expr.expr_id);
    }

    fn collect_expr(&mut self, expr_id: ExprId) {
        match &self.expr_arena[expr_id] {
            lower::Expr::LetIn { let_bindings, in_expr_id } => {
                self.local_bindings.push();
                for let_binding in let_bindings.iter() {
                    match let_binding {
                        lower::LetBinding::Name { name, .. } => {
                            self.local_bindings.define(name);
                        }
                    }
                }
                self.collect_expr(*in_expr_id);
                self.local_bindings.pop();
            }
            lower::Expr::Lit(literal) => match literal {
                lower::Lit::Array(expr_ids) => {
                    for expr_id in expr_ids.iter() {
                        self.collect_expr(*expr_id);
                    }
                }
                lower::Lit::Record(items) => {
                    for item in items.iter() {
                        match item {
                            lower::RecordItem::RecordPun(_) => (),
                            lower::RecordItem::RecordField(_, expr_id) => {
                                self.collect_expr(*expr_id);
                            }
                        }
                    }
                }
                _ => (),
            },
            lower::Expr::Var(variable) => {
                if !self.local_bindings.contains(&variable.value) {
                    self.collect_name(&variable.value);
                }
            }
        }
    }
}

pub(crate) fn collect_binding_groups<'a>(
    expr_arena: &'a Arena<lower::Expr>,
    let_bindings: impl Iterator<Item = &'a lower::LetBinding>,
) {
    let mut graph = DiGraph::<SmolStr, ()>::default();
    let mut interner = FxHashMap::default();
    for let_binding in let_bindings {
        let mut context = CollectNameContext::new(expr_arena);
        match let_binding {
            lower::LetBinding::Name { name, binding } => {
                context.collect_binding(binding);
                let source = SmolStr::new(name.as_ref());
                let targets = context.finish();

                let source_id =
                    *interner.entry(source.clone()).or_insert_with(|| graph.add_node(source));
                for target in targets {
                    let target_id =
                        *interner.entry(target.clone()).or_insert_with(|| graph.add_node(target));
                    graph.add_edge(source_id, target_id, ());
                }
            }
        }
    }
    dbg!(kosaraju_scc(&graph)
        .iter()
        .map(|group| group.iter().map(|index| graph.node_weight(*index)).collect_vec())
        .collect_vec());
}
