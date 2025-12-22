use std::mem;
use std::sync::Arc;

use itertools::Itertools;
use petgraph::algo::tarjan_scc;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use smol_str::{SmolStr, StrExt};
use stabilizing::ExpectId;
use syntax::{SyntaxToken, cst};

use crate::*;

use super::{Context, State};

pub(crate) fn lower_binder(state: &mut State, context: &Context, cst: &cst::Binder) -> BinderId {
    let id = context.stabilized.lookup_cst(cst).expect_id();
    let kind = lower_binder_kind(state, context, cst, id);
    state.associate_binder_info(id, kind);
    id
}

fn lower_binder_kind(
    state: &mut State,
    context: &Context<'_>,
    cst: &cst::Binder,
    id: BinderId,
) -> BinderKind {
    match cst {
        cst::Binder::BinderTyped(cst) => {
            let binder = cst.binder().map(|cst| lower_binder(state, context, &cst));
            let type_ = cst.type_().map(|cst| lower_type(state, context, &cst));
            BinderKind::Typed { binder, type_ }
        }
        cst::Binder::BinderOperatorChain(cst) => {
            let head = cst.binder().map(|cst| lower_binder(state, context, &cst));
            let tail = cst
                .children()
                .map(|cst| {
                    let operator = cst.operator();
                    let id = operator.and_then(|cst| lower_term_operator(state, context, &cst));
                    let element = cst.binder().map(|cst| lower_binder(state, context, &cst));
                    OperatorPair { id, element }
                })
                .collect();
            BinderKind::OperatorChain { head, tail }
        }
        cst::Binder::BinderInteger(_) => BinderKind::Integer,
        cst::Binder::BinderNumber(_) => BinderKind::Number,
        cst::Binder::BinderConstructor(cst) => {
            let resolution = cst.name().and_then(|cst| {
                let (qualifier, name) = lower_qualified_name(&cst, cst::QualifiedName::upper)?;
                state.resolve_term_reference(context, qualifier.as_deref(), &name)
            });
            let arguments = cst.children().map(|cst| lower_binder(state, context, &cst)).collect();
            BinderKind::Constructor { resolution, arguments }
        }
        cst::Binder::BinderVariable(cst) => {
            let variable = cst.name_token().map(|cst| {
                let text = cst.text();
                SmolStr::from(text)
            });
            if let Some(name) = &variable {
                state.insert_binder(name, id);
            }
            BinderKind::Variable { variable }
        }
        cst::Binder::BinderNamed(cst) => {
            let named = cst.name_token().map(|cst| {
                let text = cst.text();
                SmolStr::from(text)
            });
            if let Some(name) = &named {
                state.insert_binder(name, id);
            }
            let binder = cst.binder().map(|cst| lower_binder(state, context, &cst));
            BinderKind::Named { named, binder }
        }
        cst::Binder::BinderWildcard(_) => BinderKind::Wildcard,
        cst::Binder::BinderString(_) => BinderKind::String,
        cst::Binder::BinderChar(_) => BinderKind::Char,
        cst::Binder::BinderTrue(_) => BinderKind::Boolean { boolean: true },
        cst::Binder::BinderFalse(_) => BinderKind::Boolean { boolean: false },
        cst::Binder::BinderArray(cst) => {
            let array = cst.children().map(|cst| lower_binder(state, context, &cst)).collect();
            BinderKind::Array { array }
        }
        cst::Binder::BinderRecord(cst) => {
            let lower_item = |i| match i {
                cst::RecordItem::RecordField(cst) => {
                    let name = cst.name().and_then(|cst| {
                        let token = cst.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let value = cst.binder().map(|cst| lower_binder(state, context, &cst));
                    BinderRecordItem::RecordField { name, value }
                }
                cst::RecordItem::RecordPun(cst) => {
                    // Get the RecordPun node's ID from stabilizing
                    let id = context.stabilized.lookup_cst(&cst).expect_id();

                    let name = cst.name().and_then(|cst| {
                        let token = cst.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });

                    if let Some(name) = &name {
                        state.insert_record_pun(name, id);
                    }

                    BinderRecordItem::RecordPun { id, name }
                }
            };
            let record = cst.children().map(lower_item).collect();
            BinderKind::Record { record }
        }
        cst::Binder::BinderParenthesized(cst) => {
            let parenthesized = cst.binder().map(|cst| lower_binder(state, context, &cst));
            BinderKind::Parenthesized { parenthesized }
        }
    }
}

pub(crate) fn lower_expression(
    state: &mut State,
    context: &Context,
    cst: &cst::Expression,
) -> ExpressionId {
    let id = context.stabilized.lookup_cst(cst).expect_id();
    let kind = lower_expression_kind(state, context, cst);
    state.associate_expression_info(id, kind);
    id
}

fn lower_expression_kind(
    state: &mut State,
    context: &Context<'_>,
    cst: &cst::Expression,
) -> ExpressionKind {
    match cst {
        cst::Expression::ExpressionTyped(cst) => {
            let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));
            let type_ = cst.signature().map(|cst| lower_type(state, context, &cst));
            ExpressionKind::Typed { expression, type_ }
        }
        cst::Expression::ExpressionOperatorChain(cst) => {
            let head = cst.expression().map(|cst| lower_expression(state, context, &cst));
            let tail = cst
                .children()
                .map(|cst| {
                    let operator = cst.operator();
                    let id = operator.and_then(|cst| lower_term_operator(state, context, &cst));
                    let expression = cst.expression();
                    let element = expression.map(|cst| lower_expression(state, context, &cst));
                    OperatorPair { id, element }
                })
                .collect();
            ExpressionKind::OperatorChain { head, tail }
        }
        cst::Expression::ExpressionInfixChain(cst) => {
            let head = cst.expression().map(|cst| lower_expression(state, context, &cst));
            let tail = cst
                .children()
                .map(|cst| {
                    let tick = cst.tick().and_then(|cst| {
                        let cst = cst.expression()?;
                        Some(lower_expression(state, context, &cst))
                    });
                    let element =
                        cst.expression().map(|cst| lower_expression(state, context, &cst));
                    InfixPair { tick, element }
                })
                .collect();
            ExpressionKind::InfixChain { head, tail }
        }
        cst::Expression::ExpressionNegate(cst) => {
            let negate = state.resolve_term_full(context, None, "negate");

            if negate.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state.errors.push(LoweringError::NotInScope(NotInScope::NegateFn { id }));
            }

            let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));
            ExpressionKind::Negate { negate, expression }
        }
        cst::Expression::ExpressionApplicationChain(cst) => {
            let lower_argument =
                |state: &mut State, context: &Context, cst: &cst::ExpressionArgument| match cst {
                    cst::ExpressionArgument::ExpressionTypeArgument(cst) => {
                        let id = cst.type_().map(|cst| lower_type(state, context, &cst));
                        ExpressionArgument::Type(id)
                    }
                    cst::ExpressionArgument::ExpressionTermArgument(cst) => {
                        let id = cst.expression().map(|cst| lower_expression(state, context, &cst));
                        ExpressionArgument::Term(id)
                    }
                };

            let function = cst.expression().map(|cst| lower_expression(state, context, &cst));
            let arguments =
                cst.children().map(|cst| lower_argument(state, context, &cst)).collect();

            ExpressionKind::Application { function, arguments }
        }
        cst::Expression::ExpressionIfThenElse(cst) => {
            let if_ = cst.if_().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(state, context, &cst))
            });
            let then = cst.then().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(state, context, &cst))
            });
            let else_ = cst.else_().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(state, context, &cst))
            });
            ExpressionKind::IfThenElse { if_, then, else_ }
        }
        cst::Expression::ExpressionLetIn(cst) => state.with_scope(|s| {
            let bindings =
                cst.bindings().map(|cst| lower_bindings(s, context, &cst)).unwrap_or_default();
            let expression = cst.expression().map(|cst| lower_expression(s, context, &cst));
            ExpressionKind::LetIn { bindings, expression }
        }),
        cst::Expression::ExpressionLambda(cst) => state.with_scope(|state| {
            state.push_binder_scope();
            let binders = cst
                .function_binders()
                .map(|cst| {
                    let children = cst.children();
                    children.map(|cst| lower_binder(state, context, &cst)).collect()
                })
                .unwrap_or_default();
            let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));
            ExpressionKind::Lambda { binders, expression }
        }),
        cst::Expression::ExpressionCaseOf(cst) => {
            fn lower_case_branch(
                state: &mut State,
                context: &Context,
                cst: &cst::CaseBranch,
            ) -> CaseBranch {
                state.with_scope(|state| {
                    state.push_binder_scope();
                    let binders = cst
                        .binders()
                        .map(|cst| {
                            cst.children().map(|cst| lower_binder(state, context, &cst)).collect()
                        })
                        .unwrap_or_default();
                    let guarded_expression =
                        cst.guarded_expression().map(|cst| lower_guarded(state, context, &cst));
                    CaseBranch { binders, guarded_expression }
                })
            }

            let trunk = cst
                .trunk()
                .map(|cst| {
                    cst.children().map(|cst| lower_expression(state, context, &cst)).collect()
                })
                .unwrap_or_default();
            let branches = cst
                .branches()
                .map(|cst| {
                    cst.children().map(|cst| lower_case_branch(state, context, &cst)).collect()
                })
                .unwrap_or_default();

            ExpressionKind::CaseOf { trunk, branches }
        }
        cst::Expression::ExpressionDo(cst) => state.with_scope(|state| {
            let qualifier = cst.qualifier().and_then(|cst| {
                let token = cst.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });

            let bind = state.resolve_term_full(context, qualifier.as_deref(), "bind");
            let discard = state.resolve_term_full(context, qualifier.as_deref(), "discard");

            if bind.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state
                    .errors
                    .push(LoweringError::NotInScope(NotInScope::DoFn { kind: DoFn::Bind, id }));
            }

            if discard.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state
                    .errors
                    .push(LoweringError::NotInScope(NotInScope::DoFn { kind: DoFn::Discard, id }));
            }

            let statements = cst
                .statements()
                .map(|cst| {
                    cst.children().map(|cst| lower_do_statement(state, context, &cst)).collect()
                })
                .unwrap_or_default();

            ExpressionKind::Do { bind, discard, statements }
        }),
        cst::Expression::ExpressionAdo(cst) => state.with_scope(|state| {
            let qualifier = cst.qualifier().and_then(|cst| {
                let token = cst.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });

            let map = state.resolve_term_full(context, qualifier.as_deref(), "map");
            let apply = state.resolve_term_full(context, qualifier.as_deref(), "apply");

            if map.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state
                    .errors
                    .push(LoweringError::NotInScope(NotInScope::AdoFn { kind: AdoFn::Map, id }));
            }

            if apply.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state
                    .errors
                    .push(LoweringError::NotInScope(NotInScope::AdoFn { kind: AdoFn::Apply, id }));
            }

            let statements = cst
                .statements()
                .map(|cst| {
                    cst.children().map(|cst| lower_do_statement(state, context, &cst)).collect()
                })
                .unwrap_or_default();
            let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));

            ExpressionKind::Ado { map, apply, statements, expression }
        }),
        cst::Expression::ExpressionConstructor(cst) => {
            let resolution = cst.name().and_then(|cst| {
                let (qualifier, name) = lower_qualified_name(&cst, cst::QualifiedName::upper)?;
                state.resolve_term_reference(context, qualifier.as_deref(), &name)
            });
            if resolution.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state.errors.push(LoweringError::NotInScope(NotInScope::ExprConstructor { id }));
            }
            ExpressionKind::Constructor { resolution }
        }
        cst::Expression::ExpressionVariable(cst) => {
            let resolution = cst.name().and_then(|cst| {
                let (qualifier, name) = lower_qualified_name(&cst, cst::QualifiedName::lower)?;
                state.resolve_term_full(context, qualifier.as_deref(), name.as_str())
            });
            if resolution.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state.errors.push(LoweringError::NotInScope(NotInScope::ExprVariable { id }));
            }
            ExpressionKind::Variable { resolution }
        }
        cst::Expression::ExpressionOperatorName(cst) => {
            let resolution = cst.name().and_then(|cst| {
                let (qualifier, name) =
                    lower_qualified_name(&cst, cst::QualifiedName::operator_name)?;
                state.resolve_term_reference(context, qualifier.as_deref(), &name)
            });
            if resolution.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state.errors.push(LoweringError::NotInScope(NotInScope::ExprOperatorName { id }));
            }
            ExpressionKind::OperatorName { resolution }
        }
        cst::Expression::ExpressionSection(_) => ExpressionKind::Section,
        cst::Expression::ExpressionHole(_) => ExpressionKind::Hole,
        cst::Expression::ExpressionString(_) => ExpressionKind::String,
        cst::Expression::ExpressionChar(_) => ExpressionKind::Char,
        cst::Expression::ExpressionTrue(_) => ExpressionKind::Boolean { boolean: true },
        cst::Expression::ExpressionFalse(_) => ExpressionKind::Boolean { boolean: false },
        cst::Expression::ExpressionInteger(_) => ExpressionKind::Integer,
        cst::Expression::ExpressionNumber(_) => ExpressionKind::Number,
        cst::Expression::ExpressionArray(cst) => {
            let array = cst.children().map(|cst| lower_expression(state, context, &cst)).collect();
            ExpressionKind::Array { array }
        }
        cst::Expression::ExpressionRecord(cst) => {
            let lower_item = |state: &mut State, cst| match cst {
                cst::RecordItem::RecordField(cst) => {
                    let name = cst.name().and_then(|cst| {
                        let token = cst.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let value = cst.expression().map(|cst| lower_expression(state, context, &cst));
                    ExpressionRecordItem::RecordField { name, value }
                }
                cst::RecordItem::RecordPun(cst) => {
                    let name = cst.name().and_then(|cst| {
                        let token = cst.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let resolution = name.as_ref().and_then(|name| {
                        let qualifier: Option<&str> = None;
                        state.resolve_term_full(context, qualifier, name)
                    });
                    ExpressionRecordItem::RecordPun { name, resolution }
                }
            };
            let record = cst.children().map(|cst| lower_item(state, cst)).collect();
            ExpressionKind::Record { record }
        }
        cst::Expression::ExpressionParenthesized(cst) => {
            let parenthesized = cst.expression().map(|cst| lower_expression(state, context, &cst));
            ExpressionKind::Parenthesized { parenthesized }
        }
        cst::Expression::ExpressionRecordAccess(cst) => {
            let record = cst.expression().map(|cst| lower_expression(state, context, &cst));
            let labels = cst
                .children()
                .map(|cst| {
                    let token = cst.text()?;
                    let text = token.text();
                    Some(SmolStr::from(text))
                })
                .collect();
            ExpressionKind::RecordAccess { record, labels }
        }
        cst::Expression::ExpressionRecordUpdate(cst) => {
            let record = cst.expression().map(|cst| lower_expression(state, context, &cst));
            let updates = cst
                .record_updates()
                .map(|cst| lower_record_updates(state, context, &cst))
                .unwrap_or_default();
            ExpressionKind::RecordUpdate { record, updates }
        }
    }
}

fn lower_guarded(
    state: &mut State,
    context: &Context,
    cst: &cst::GuardedExpression,
) -> GuardedExpression {
    match cst {
        cst::GuardedExpression::Unconditional(cst) => {
            let where_expression =
                cst.where_expression().map(|cst| lower_where_expression(state, context, &cst));
            GuardedExpression::Unconditional { where_expression }
        }
        cst::GuardedExpression::Conditionals(cst) => {
            let pattern_guarded =
                cst.children().map(|cst| lower_pattern_guarded(state, context, &cst)).collect();
            GuardedExpression::Conditionals { pattern_guarded }
        }
    }
}

fn lower_where_expression(
    state: &mut State,
    context: &Context,
    cst: &cst::WhereExpression,
) -> WhereExpression {
    state.with_scope(|s| {
        let bindings =
            cst.bindings().map(|cst| lower_bindings(s, context, &cst)).unwrap_or_default();
        let expression = cst.expression().map(|cst| lower_expression(s, context, &cst));
        WhereExpression { expression, bindings }
    })
}

fn lower_pattern_guarded(
    state: &mut State,
    context: &Context,
    cst: &cst::PatternGuarded,
) -> PatternGuarded {
    // ```
    // guarded a b
    //   | Just c <- a = c
    //   | Just d <- b = d
    // ```
    state.with_scope(|s| {
        let pattern_guards =
            cst.children().map(|cst| lower_pattern_guard(s, context, &cst)).collect();
        let where_expression =
            cst.where_expression().map(|cst| lower_where_expression(s, context, &cst));
        PatternGuarded { pattern_guards, where_expression }
    })
}

fn lower_pattern_guard(
    state: &mut State,
    context: &Context,
    cst: &cst::PatternGuard,
) -> PatternGuard {
    match cst {
        cst::PatternGuard::PatternGuardBinder(cst) => {
            state.push_binder_scope();
            let binder = cst.binder().map(|cst| lower_binder(state, context, &cst));
            let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));
            PatternGuard { binder, expression }
        }
        cst::PatternGuard::PatternGuardExpression(cst) => {
            let binder = None;
            let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));
            PatternGuard { binder, expression }
        }
    }
}

fn lower_bindings(
    state: &mut State,
    context: &Context,
    cst: &cst::LetBindingStatements,
) -> Arc<[LetBindingChunk]> {
    #[derive(Debug, PartialEq, Eq)]
    enum Chunk {
        Pattern,
        Equation,
    }

    let chunks = cst.children().chunk_by(|cst| match cst {
        cst::LetBinding::LetBindingPattern(_) => Chunk::Pattern,
        cst::LetBinding::LetBindingSignature(_) => Chunk::Equation,
        cst::LetBinding::LetBindingEquation(_) => Chunk::Equation,
    });

    let mut result = vec![];
    for (kind, children) in chunks.into_iter() {
        match kind {
            Chunk::Pattern => {
                // Each pattern becomes its own chunk
                for pattern in children {
                    result.push(lower_pattern_chunk(state, context, pattern));
                }
            }
            Chunk::Equation => {
                // All consecutive equations become one Names chunk
                result.push(lower_equation_chunk(state, context, children));
            }
        }
    }

    result.into()
}

fn lower_pattern_chunk(
    state: &mut State,
    context: &Context,
    pattern: cst::LetBinding,
) -> LetBindingChunk {
    let cst::LetBinding::LetBindingPattern(pattern) = &pattern else {
        unreachable!("invariant violated: expected LetBindingPattern");
    };
    let where_expression =
        pattern.where_expression().map(|cst| lower_where_expression(state, context, &cst));
    state.push_binder_scope();
    let binder = pattern.binder().map(|cst| lower_binder(state, context, &cst));
    LetBindingChunk::Pattern { binder, where_expression }
}

struct PendingLetBinding {
    name: Option<SmolStr>,
    signature: Option<LetBindingSignatureId>,
    equations: Arc<[LetBindingEquationId]>,
}

fn lower_equation_chunk(
    state: &mut State,
    context: &Context,
    children: impl Iterator<Item = cst::LetBinding>,
) -> LetBindingChunk {
    let outer_graph = mem::take(&mut state.let_binding_graph);

    let children = children.chunk_by(|cst| match cst {
        cst::LetBinding::LetBindingPattern(_) => {
            unreachable!("invariant violated: expected LetBindingSignature / LetBindingEquation");
        }
        cst::LetBinding::LetBindingSignature(cst) => cst.name_token().map(|cst| {
            let text = cst.text();
            SmolStr::from(text)
        }),
        cst::LetBinding::LetBindingEquation(cst) => cst.name_token().map(|cst| {
            let text = cst.text();
            SmolStr::from(text)
        }),
    });

    let mut pending = vec![];

    for (name, mut children) in children.into_iter() {
        let mut signature = None;
        let mut equations = vec![];

        if let Some(cst) = children.next() {
            match cst {
                cst::LetBinding::LetBindingPattern(_) => {
                    unreachable!(
                        "invariant violated: expected LetBindingSignature / LetBindingEquation"
                    );
                }
                cst::LetBinding::LetBindingSignature(cst) => {
                    let ast_id = context.stabilized.lookup_cst(&cst).expect_id();
                    signature = Some(ast_id);
                }
                cst::LetBinding::LetBindingEquation(cst) => {
                    let ast_id = context.stabilized.lookup_cst(&cst).expect_id();
                    equations.push(ast_id);
                }
            }
        }

        children.for_each(|cst| {
            if let cst::LetBinding::LetBindingEquation(cst) = cst {
                let ast_id = context.stabilized.lookup_cst(&cst).expect_id();
                equations.push(ast_id);
            }
        });

        let equations = equations.into();
        pending.push(PendingLetBinding { name, signature, equations });
    }

    let mut let_bound = FxHashMap::default();
    let mut groups = vec![];

    for PendingLetBinding { name, signature, equations } in pending {
        let group = LetBindingNameGroup { signature, equations };

        let id = state.alloc_let_binding(group);

        if let Some(ref name) = name {
            let_bound.insert(name.clone(), id);
        }

        groups.push(id);
    }

    let graph_node =
        state.graph.inner.alloc(GraphNode::Let { parent: state.graph_scope, bindings: let_bound });

    state.graph_scope = Some(graph_node);
    state.current_let_scope = Some(graph_node);

    // Lower each binding, tracking current_let_binding for dependency tracking
    for &id in &groups {
        state.current_let_binding = Some(id);

        let let_binding = &state.info.let_binding[id];
        let signature = let_binding.signature;
        let equations = Arc::clone(&let_binding.equations);

        state.with_scope(|state| {
            let signature = signature.and_then(|id| {
                let cst = context.stabilized.ast_ptr(id)?.try_to_node(context.root)?;
                let cst = cst.type_()?;
                Some(lower_forall(state, context, &cst))
            });

            let equations = equations.iter().filter_map(|&id| {
                let cst = context.stabilized.ast_ptr(id)?.try_to_node(context.root)?;
                Some(lower_equation_like(
                    state,
                    context,
                    cst,
                    cst::LetBindingEquation::function_binders,
                    cst::LetBindingEquation::guarded_expression,
                ))
            });

            let equations = equations.collect();

            let info = LetBindingName { signature, equations };
            state.associate_let_binding_name(id, info);
        });

        state.current_let_binding = None;
    }

    state.current_let_scope = None;

    // Compute SCCs from dependency graph
    let mut graph = mem::take(&mut state.let_binding_graph);
    for &id in &groups {
        graph.add_node(id);
    }

    let sccs = tarjan_scc(&graph);
    let scc = sccs
        .into_iter()
        .map(|scc| match scc[..] {
            [single] if !graph.contains_edge(single, single) => Scc::Base(single),
            [single] => Scc::Recursive(single),
            _ => Scc::Mutual(scc),
        })
        .collect();

    state.let_binding_graph = outer_graph;

    LetBindingChunk::Names { bindings: groups.into(), scc }
}

pub(crate) fn lower_equation_like<T: AstNode>(
    state: &mut State,
    context: &Context,
    equation: T,
    binders: impl Fn(&T) -> Option<cst::FunctionBinders>,
    guarded: impl Fn(&T) -> Option<cst::GuardedExpression>,
) -> Equation {
    state.with_scope(|state| {
        state.push_binder_scope();
        let binders = binders(&equation)
            .map(|cst| cst.children().map(|cst| lower_binder(state, context, &cst)).collect())
            .unwrap_or_default();
        let guarded = guarded(&equation).map(|cst| lower_guarded(state, context, &cst));
        Equation { binders, guarded }
    })
}

fn lower_do_statement(state: &mut State, context: &Context, cst: &cst::DoStatement) -> DoStatement {
    match cst {
        cst::DoStatement::DoStatementBind(cst) => {
            let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));
            state.push_binder_scope();
            let binder = cst.binder().map(|cst| lower_binder(state, context, &cst));
            DoStatement::Bind { binder, expression }
        }
        cst::DoStatement::DoStatementLet(cst) => {
            let statements = cst
                .statements()
                .map(|cst| lower_bindings(state, context, &cst))
                .unwrap_or_default();
            DoStatement::Let { statements }
        }
        cst::DoStatement::DoStatementDiscard(cst) => {
            let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));
            DoStatement::Discard { expression }
        }
    }
}

fn lower_record_updates(
    state: &mut State,
    context: &Context,
    cst: &cst::RecordUpdates,
) -> Arc<[RecordUpdate]> {
    cst.children()
        .map(|cst| match cst {
            cst::RecordUpdate::RecordUpdateLeaf(cst) => {
                let name = cst.name().and_then(|cst| {
                    let token = cst.text()?;
                    let text = token.text();
                    Some(SmolStr::from(text))
                });
                let expression = cst.expression().map(|cst| lower_expression(state, context, &cst));
                RecordUpdate::Leaf { name, expression }
            }
            cst::RecordUpdate::RecordUpdateBranch(cst) => {
                let name = cst.name().and_then(|cst| {
                    let token = cst.text()?;
                    let text = token.text();
                    Some(SmolStr::from(text))
                });
                let updates = cst
                    .record_updates()
                    .map(|cst| lower_record_updates(state, context, &cst))
                    .unwrap_or_default();
                RecordUpdate::Branch { name, updates }
            }
        })
        .collect()
}

pub(crate) fn lower_type(state: &mut State, context: &Context, cst: &cst::Type) -> TypeId {
    let id = context.stabilized.lookup_cst(cst).expect_id();
    let kind = lower_type_kind(state, context, cst, id);
    state.associate_type_info(id, kind);
    id
}

fn lower_type_kind(
    state: &mut State,
    context: &Context<'_>,
    cst: &cst::Type,
    id: TypeId,
) -> TypeKind {
    match cst {
        cst::Type::TypeApplicationChain(cst) => {
            let mut children = cst.children().map(|cst| lower_type(state, context, &cst));
            let function = children.next();
            let arguments = children.collect();
            TypeKind::ApplicationChain { function, arguments }
        }
        cst::Type::TypeArrow(cst) => {
            let mut children = cst.children().map(|cst| lower_type(state, context, &cst));
            let argument = children.next();
            let result = children.next();
            TypeKind::Arrow { argument, result }
        }
        cst::Type::TypeConstrained(cst) => {
            let mut children = cst.children().map(|cst| lower_type(state, context, &cst));
            let constraint = children.next();
            let constrained = children.next();
            TypeKind::Constrained { constraint, constrained }
        }
        cst::Type::TypeConstructor(cst) => {
            let resolution = cst.name().and_then(|cst| {
                let (qualifier, name) = lower_qualified_name(&cst, cst::QualifiedName::upper)?;
                state.resolve_type_reference(context, qualifier.as_deref(), &name)
            });
            if resolution.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state.errors.push(LoweringError::NotInScope(NotInScope::TypeConstructor { id }));
            }
            TypeKind::Constructor { resolution }
        }
        // Rank-N Types must be scoped. See `lower_forall`.
        cst::Type::TypeForall(cst) => state.with_scope(|s| {
            s.push_forall_scope();
            let bindings =
                cst.children().map(|cst| lower_type_variable_binding(s, context, &cst)).collect();
            let inner = cst.type_().map(|cst| lower_type(s, context, &cst));
            TypeKind::Forall { bindings, inner }
        }),
        cst::Type::TypeHole(_) => TypeKind::Hole,
        cst::Type::TypeInteger(cst) => {
            let value = cst.integer_token().and_then(|token| {
                let text = token.text();
                let integer = if let Some(hex) = text.strip_prefix("0x") {
                    let clean = hex.replace_smolstr("_", "");
                    i32::from_str_radix(&clean, 16).ok()?
                } else {
                    let clean = text.replace_smolstr("_", "");
                    clean.parse().ok()?
                };
                if cst.minus_token().is_some() { Some(-integer) } else { Some(integer) }
            });
            TypeKind::Integer { value }
        }
        cst::Type::TypeKinded(cst) => {
            let mut children = cst.children().map(|cst| lower_type(state, context, &cst));
            let type_ = children.next();
            let kind = children.next();
            TypeKind::Kinded { type_, kind }
        }
        cst::Type::TypeOperatorName(cst) => {
            let resolution = cst.name().and_then(|cst| {
                let (qualifier, name) =
                    lower_qualified_name(&cst, cst::QualifiedName::operator_name)?;
                state.resolve_type_reference(context, qualifier.as_deref(), &name)
            });
            if resolution.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state.errors.push(LoweringError::NotInScope(NotInScope::TypeOperatorName { id }));
            }
            TypeKind::Operator { resolution }
        }
        cst::Type::TypeOperatorChain(cst) => {
            let head = cst.type_().map(|cst| lower_type(state, context, &cst));
            let tail = cst
                .children()
                .map(|cst| {
                    let operator = cst.operator();
                    let id = operator.and_then(|cst| lower_type_operator(state, context, &cst));
                    let element = cst.type_().as_ref().map(|cst| lower_type(state, context, cst));
                    OperatorPair { id, element }
                })
                .collect();
            TypeKind::OperatorChain { head, tail }
        }
        cst::Type::TypeString(cst) => {
            let (kind, value) = if let Some(token) = cst.string() {
                let text = token.text();
                let value = text
                    .strip_prefix('"')
                    .and_then(|text| text.strip_suffix('"'))
                    .map(SmolStr::from);
                (StringKind::String, value)
            } else if let Some(token) = cst.raw_string() {
                let text = token.text();
                let value = text
                    .strip_prefix("\"\"\"")
                    .and_then(|text| text.strip_suffix("\"\"\""))
                    .map(SmolStr::from);
                (StringKind::RawString, value)
            } else {
                (StringKind::String, None)
            };
            TypeKind::String { kind, value }
        }
        cst::Type::TypeVariable(cst) => {
            let name = cst.name_token().map(|cst| {
                let text = cst.text();
                SmolStr::from(text)
            });
            let resolution = cst.name_token().and_then(|cst| {
                let text = cst.text();
                state.resolve_type_variable(id, text)
            });
            if resolution.is_none() {
                let id = context.stabilized.lookup_cst(cst).expect_id();
                state.errors.push(LoweringError::NotInScope(NotInScope::TypeVariable { id }));
            }
            TypeKind::Variable { name, resolution }
        }
        cst::Type::TypeWildcard(_) => TypeKind::Wildcard,
        cst::Type::TypeRecord(cst) => {
            let items = cst.children().map(|cst| lower_row_item(state, context, &cst)).collect();
            let tail = cst.tail().and_then(|cst| {
                let cst = cst.type_()?;
                Some(lower_type(state, context, &cst))
            });
            TypeKind::Record { items, tail }
        }
        cst::Type::TypeRow(cst) => {
            let items = cst.children().map(|cst| lower_row_item(state, context, &cst)).collect();
            let tail = cst.tail().and_then(|cst| {
                let cst = cst.type_()?;
                Some(lower_type(state, context, &cst))
            });
            TypeKind::Row { items, tail }
        }
        cst::Type::TypeParenthesized(cst) => {
            let parenthesized = cst.type_().map(|cst| lower_type(state, context, &cst));
            TypeKind::Parenthesized { parenthesized }
        }
    }
}

pub(crate) fn lower_forall(state: &mut State, context: &Context, cst: &cst::Type) -> TypeId {
    // To enable lexically-scoped type variables, we avoid calling `with_scope`
    // as to not reset to the parent scope once all the top-level `forall` have
    // been lowered. In `lower_type`, the `TypeForall` branch explicitly calls
    // `with_scope` in order to scope Rank-N type variables.
    if let cst::Type::TypeForall(f) = cst {
        let id = context.stabilized.lookup_cst(cst).expect_id();
        state.push_forall_scope();
        let bindings =
            f.children().map(|cst| lower_type_variable_binding(state, context, &cst)).collect();
        let inner = f.type_().map(|cst| lower_forall(state, context, &cst));
        let kind = TypeKind::Forall { bindings, inner };
        state.associate_type_info(id, kind);
        id
    } else {
        lower_type(state, context, cst)
    }
}

fn lower_term_operator(
    state: &mut State,
    context: &Context,
    cst: &cst::TermOperator,
) -> Option<TermOperatorId> {
    let id = context.stabilized.lookup_cst(cst).expect_id();

    let (qualifier, name) = cst.qualified().and_then(|cst| {
        let qualifier = cst.qualifier().and_then(|cst| {
            let token = cst.text()?;
            let text = token.text().trim_end_matches('.');
            Some(SmolStr::from(text))
        });

        let token = cst.operator()?;
        let text = token.text().trim_start_matches('(').trim_end_matches(')');
        let name = SmolStr::from(text);

        Some((qualifier, name))
    })?;

    let Some((file_id, term_id)) =
        state.resolve_term_reference(context, qualifier.as_deref(), &name)
    else {
        let id = context.stabilized.lookup_cst(cst).expect_id();
        state.errors.push(LoweringError::NotInScope(NotInScope::TermOperator { id }));
        return None;
    };

    state.info.term_operator.insert(id, (file_id, term_id));

    Some(id)
}

fn lower_type_operator(
    state: &mut State,
    context: &Context,
    cst: &cst::TypeOperator,
) -> Option<TypeOperatorId> {
    let id = context.stabilized.lookup_cst(cst).expect_id();
    let (qualifier, name) =
        cst.qualified().and_then(|cst| lower_qualified_name(&cst, cst::QualifiedName::operator))?;

    let Some((file_id, type_id)) =
        state.resolve_type_reference(context, qualifier.as_deref(), &name)
    else {
        let id = context.stabilized.lookup_cst(cst).expect_id();
        state.errors.push(LoweringError::NotInScope(NotInScope::TypeOperator { id }));
        return None;
    };

    state.info.type_operator.insert(id, (file_id, type_id));

    Some(id)
}

pub(crate) fn lower_qualified_name(
    cst: &cst::QualifiedName,
    token: impl Fn(&cst::QualifiedName) -> Option<SyntaxToken>,
) -> Option<(Option<SmolStr>, SmolStr)> {
    let qualifier = cst.qualifier().and_then(|cst| {
        let token = cst.text()?;
        let text = token.text().trim_end_matches('.');
        Some(SmolStr::from(text))
    });

    let token = token(cst)?;
    let text = token.text().trim_start_matches('(').trim_end_matches(')');
    let name = SmolStr::from(text);

    Some((qualifier, name))
}

pub(crate) fn lower_type_variable_binding(
    state: &mut State,
    context: &Context,
    cst: &cst::TypeVariableBinding,
) -> TypeVariableBinding {
    let id = context.stabilized.lookup_cst(cst).expect_id();
    let visible = cst.at().is_some();
    let name = cst.name().map(|cst| {
        let text = cst.text();
        SmolStr::from(text)
    });
    let kind = cst.kind().map(|cst| lower_type(state, context, &cst));
    if let Some(name) = &name {
        state.insert_bound_variable(name, id);
    }
    TypeVariableBinding { visible, id, name, kind }
}

fn lower_row_item(state: &mut State, context: &Context, cst: &cst::TypeRowItem) -> TypeRowItem {
    let name = cst.name().and_then(|cst| {
        let token = cst.text()?;
        let text = token.text();
        Some(SmolStr::from(text))
    });
    let type_ = cst.type_().map(|t| lower_type(state, context, &t));
    TypeRowItem { name, type_ }
}
