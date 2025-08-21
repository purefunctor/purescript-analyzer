use std::sync::Arc;

use itertools::Itertools;
use rowan::ast::AstNode;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::{cst, SyntaxToken};

use crate::*;

use super::{Environment, State};

pub(super) fn lower_binder(s: &mut State, e: &Environment, cst: &cst::Binder) -> BinderId {
    let id = s.source.allocate_bd(cst);
    let kind = lower_binder_kind(s, e, cst, id).unwrap_or(BinderKind::Unknown);
    s.associate_binder_info(id, kind);
    id
}

fn lower_binder_kind(
    s: &mut State,
    e: &Environment<'_>,
    cst: &cst::Binder,
    id: la_arena::Idx<rowan::ast::AstPtr<cst::Binder>>,
) -> Option<BinderKind> {
    Some(match cst {
        cst::Binder::BinderTyped(cst) => {
            let binder = cst.binder().map(|cst| lower_binder(s, e, &cst));
            let r#type = cst.r#type().map(|cst| lower_type(s, e, &cst));
            BinderKind::Typed { binder, r#type }
        }
        cst::Binder::BinderOperatorChain(cst) => {
            let head = cst.binder().map(|cst| lower_binder(s, e, &cst));
            let tail = cst
                .children()
                .map(|cst| {
                    let qualified = cst.qualified()?;
                    let binder = cst.binder().map(|cst| lower_binder(s, e, &cst));
                    Some(lower_pair(s, ResolutionDomain::Term, qualified, binder))
                })
                .collect::<Option<Arc<_>>>()?;
            BinderKind::OperatorChain { head, tail }
        }
        cst::Binder::BinderInteger(_) => BinderKind::Integer,
        cst::Binder::BinderNumber(_) => BinderKind::Number,
        cst::Binder::BinderConstructor(cst) => {
            let (_, qualifier, name) =
                cst.name().map(|cst| lower_qualified_name(s, &cst, cst::QualifiedName::upper))?;
            let resolution = s.resolve_deferred(ResolutionDomain::Term, qualifier, name);
            let arguments = cst.children().map(|cst| lower_binder(s, e, &cst)).collect();
            BinderKind::Constructor { resolution, arguments }
        }
        cst::Binder::BinderVariable(cst) => {
            let variable = cst.name_token().map(|cst| {
                let text = cst.text();
                SmolStr::from(text)
            });
            if let Some(name) = &variable {
                s.insert_binder(name, id);
            }
            BinderKind::Variable { variable }
        }
        cst::Binder::BinderNamed(cst) => {
            let named = cst.name_token().map(|cst| {
                let text = cst.text();
                SmolStr::from(text)
            });
            if let Some(name) = &named {
                s.insert_binder(name, id);
            }
            let binder = cst.binder().map(|cst| lower_binder(s, e, &cst));
            BinderKind::Named { named, binder }
        }
        cst::Binder::BinderWildcard(_) => BinderKind::Wildcard,
        cst::Binder::BinderString(_) => BinderKind::String,
        cst::Binder::BinderChar(_) => BinderKind::Char,
        cst::Binder::BinderTrue(_) => BinderKind::Boolean { boolean: true },
        cst::Binder::BinderFalse(_) => BinderKind::Boolean { boolean: false },
        cst::Binder::BinderArray(cst) => {
            let array = cst.children().map(|cst| lower_binder(s, e, &cst)).collect();
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
                    let value = cst.binder().map(|cst| lower_binder(s, e, &cst));
                    BinderRecordItem::RecordField { name, value }
                }
                cst::RecordItem::RecordPun(cst) => {
                    let name = cst.name().and_then(|cst| {
                        let token = cst.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    if let Some(name) = &name {
                        s.insert_binder(name, id);
                    }
                    BinderRecordItem::RecordPun { name }
                }
            };
            let record = cst.children().map(lower_item).collect();
            BinderKind::Record { record }
        }
        cst::Binder::BinderParenthesized(cst) => {
            let parenthesized = cst.binder().map(|cst| lower_binder(s, e, &cst));
            BinderKind::Parenthesized { parenthesized }
        }
    })
}

pub(super) fn lower_expression(
    s: &mut State,
    e: &Environment,
    cst: &cst::Expression,
) -> ExpressionId {
    let id = s.source.allocate_ex(cst);
    let kind = lower_expression_kind(s, e, cst).unwrap_or(ExpressionKind::Unknown);
    s.intermediate.insert_expression_kind(id, kind);
    id
}

fn lower_expression_kind(
    s: &mut State,
    e: &Environment<'_>,
    cst: &cst::Expression,
) -> Option<ExpressionKind> {
    Some(match cst {
        cst::Expression::ExpressionTyped(cst) => {
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
            let r#type = cst.signature().map(|cst| lower_type(s, e, &cst));
            ExpressionKind::Typed { expression, r#type }
        }
        cst::Expression::ExpressionOperatorChain(cst) => {
            let head = cst.expression().map(|cst| lower_expression(s, e, &cst));
            let tail = cst
                .children()
                .map(|cst| {
                    let qualified = cst.qualified()?;
                    let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
                    Some(lower_pair(s, ResolutionDomain::Term, qualified, expression))
                })
                .collect::<Option<Arc<_>>>()?;
            ExpressionKind::OperatorChain { head, tail }
        }
        cst::Expression::ExpressionInfixChain(cst) => {
            let head = cst.expression().map(|cst| lower_expression(s, e, &cst));
            let tail = cst
                .children()
                .map(|cst| {
                    let tick = cst.tick().and_then(|cst| {
                        let cst = cst.expression()?;
                        Some(lower_expression(s, e, &cst))
                    });
                    let element = cst.expression().map(|cst| lower_expression(s, e, &cst));
                    InfixPair { tick, element }
                })
                .collect();
            ExpressionKind::InfixChain { head, tail }
        }
        cst::Expression::ExpressionNegate(cst) => {
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
            ExpressionKind::Negate { expression }
        }
        cst::Expression::ExpressionApplicationChain(cst) => {
            let lower_argument =
                |s: &mut State, e: &Environment, cst: &cst::ExpressionArgument| match cst {
                    cst::ExpressionArgument::ExpressionTypeArgument(cst) => {
                        let id = cst.r#type().map(|cst| lower_type(s, e, &cst));
                        ExpressionArgument::Type(id)
                    }
                    cst::ExpressionArgument::ExpressionTermArgument(cst) => {
                        let id = cst.expression().map(|cst| lower_expression(s, e, &cst));
                        ExpressionArgument::Term(id)
                    }
                };

            let function = cst.expression().map(|cst| lower_expression(s, e, &cst));
            let arguments = cst.children().map(|cst| lower_argument(s, e, &cst)).collect();

            ExpressionKind::Application { function, arguments }
        }
        cst::Expression::ExpressionIfThenElse(cst) => {
            let r#if = cst.r#if().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(s, e, &cst))
            });
            let then = cst.then().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(s, e, &cst))
            });
            let r#else = cst.r#else().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(s, e, &cst))
            });
            ExpressionKind::IfThenElse { r#if, then, r#else }
        }
        cst::Expression::ExpressionLetIn(cst) => s.with_scope(|s| {
            let bindings = cst.bindings().map(|cst| lower_bindings(s, e, &cst)).unwrap_or_default();
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
            ExpressionKind::LetIn { bindings, expression }
        }),
        cst::Expression::ExpressionLambda(cst) => s.with_scope(|s| {
            s.push_binder_scope();
            let binders = cst
                .function_binders()
                .map(|cst| {
                    let children = cst.children();
                    children.map(|cst| lower_binder(s, e, &cst)).collect()
                })
                .unwrap_or_default();
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
            ExpressionKind::Lambda { binders, expression }
        }),
        cst::Expression::ExpressionCaseOf(cst) => {
            let lower_case_branch = |s: &mut State, e: &Environment, cst: &cst::CaseBranch| {
                let binders = cst
                    .binders()
                    .map(|cst| cst.children().map(|cst| lower_binder(s, e, &cst)).collect())
                    .unwrap_or_default();
                let guarded_expression =
                    cst.guarded_expression().map(|cst| lower_guarded(s, e, &cst));
                CaseBranch { binders, guarded_expression }
            };

            let trunk = cst
                .trunk()
                .map(|cst| cst.children().map(|cst| lower_expression(s, e, &cst)).collect())
                .unwrap_or_default();
            let branches = cst
                .branches()
                .map(|cst| cst.children().map(|cst| lower_case_branch(s, e, &cst)).collect())
                .unwrap_or_default();

            ExpressionKind::CaseOf { trunk, branches }
        }
        cst::Expression::ExpressionDo(cst) => s.with_scope(|s| {
            let qualifier = cst.qualifier().and_then(|cst| {
                let token = cst.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });

            const BIND: Option<SmolStr> = Some(SmolStr::new_inline("bind"));
            const DISCARD: Option<SmolStr> = Some(SmolStr::new_inline("discard"));

            let bind = s.resolve_term(qualifier.clone(), BIND);
            let discard = s.resolve_term(qualifier.clone(), DISCARD);

            let statements = cst
                .statements()
                .map(|cst| cst.children().map(|cst| lower_do_statement(s, e, &cst)).collect())
                .unwrap_or_default();

            ExpressionKind::Do { bind, discard, statements }
        }),
        cst::Expression::ExpressionAdo(cst) => s.with_scope(|s| {
            let qualifier = cst.qualifier().and_then(|cst| {
                let token = cst.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });

            const MAP: Option<SmolStr> = Some(SmolStr::new_inline("map"));
            const APPLY: Option<SmolStr> = Some(SmolStr::new_inline("apply"));

            let map = s.resolve_term(qualifier.clone(), MAP);
            let apply = s.resolve_term(qualifier.clone(), APPLY);

            let statements = cst
                .statements()
                .map(|cst| cst.children().map(|cst| lower_do_statement(s, e, &cst)).collect())
                .unwrap_or_default();
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));

            ExpressionKind::Ado { map, apply, statements, expression }
        }),
        cst::Expression::ExpressionConstructor(cst) => {
            let (_, qualifier, name) =
                cst.name().map(|cst| lower_qualified_name(s, &cst, cst::QualifiedName::upper))?;
            let resolution = s.resolve_deferred(ResolutionDomain::Term, qualifier, name);
            ExpressionKind::Constructor { resolution }
        }
        cst::Expression::ExpressionVariable(cst) => {
            let (_, qualifier, name) =
                cst.name().map(|cst| lower_qualified_name(s, &cst, cst::QualifiedName::lower))?;
            let resolution = s.resolve_term(qualifier, name);
            ExpressionKind::Variable { resolution }
        }
        cst::Expression::ExpressionOperatorName(cst) => {
            let (_, qualifier, name) = cst
                .name()
                .map(|cst| lower_qualified_name(s, &cst, cst::QualifiedName::operator_name))?;
            let resolution = s.resolve_deferred(ResolutionDomain::Term, qualifier, name);
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
            let array = cst.children().map(|cst| lower_expression(s, e, &cst)).collect();
            ExpressionKind::Array { array }
        }
        cst::Expression::ExpressionRecord(cst) => {
            let lower_item = |s: &mut State, cst| match cst {
                cst::RecordItem::RecordField(cst) => {
                    let name = cst.name().and_then(|cst| {
                        let token = cst.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let value = cst.expression().map(|cst| lower_expression(s, e, &cst));
                    ExpressionRecordItem::RecordField { name, value }
                }
                cst::RecordItem::RecordPun(cst) => {
                    let name = cst.name().and_then(|cst| {
                        let token = cst.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let resolution = s.resolve_term(None, name.clone());
                    ExpressionRecordItem::RecordPun { name, resolution }
                }
            };
            let record = cst.children().map(|cst| lower_item(s, cst)).collect();
            ExpressionKind::Record { record }
        }
        cst::Expression::ExpressionParenthesized(cst) => {
            let parenthesized = cst.expression().map(|cst| lower_expression(s, e, &cst));
            ExpressionKind::Parenthesized { parenthesized }
        }
        cst::Expression::ExpressionRecordAccess(cst) => {
            let record = cst.expression().map(|cst| lower_expression(s, e, &cst));
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
            let updates = cst
                .record_updates()
                .map(|cst| lower_record_updates(s, e, &cst))
                .unwrap_or_default();
            ExpressionKind::RecordUpdate { updates }
        }
    })
}

fn lower_guarded(
    s: &mut State,
    e: &Environment,
    cst: &cst::GuardedExpression,
) -> GuardedExpression {
    match cst {
        cst::GuardedExpression::Unconditional(cst) => {
            let where_expression =
                cst.where_expression().map(|cst| lower_where_expression(s, e, &cst));
            GuardedExpression::Unconditional { where_expression }
        }
        cst::GuardedExpression::Conditionals(cst) => {
            let pattern_guarded =
                cst.children().map(|cst| lower_pattern_guarded(s, e, &cst)).collect();
            GuardedExpression::Conditionals { pattern_guarded }
        }
    }
}

fn lower_where_expression(
    s: &mut State,
    e: &Environment,
    cst: &cst::WhereExpression,
) -> WhereExpression {
    s.with_scope(|s| {
        let bindings = cst.bindings().map(|cst| lower_bindings(s, e, &cst)).unwrap_or_default();
        let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
        WhereExpression { expression, bindings }
    })
}

fn lower_pattern_guarded(
    s: &mut State,
    e: &Environment,
    cst: &cst::PatternGuarded,
) -> PatternGuarded {
    // ```
    // guarded a b
    //   | Just c <- a = c
    //   | Just d <- b = d
    // ```
    s.with_scope(|s| {
        let pattern_guards = cst.children().map(|cst| lower_pattern_guard(s, e, &cst)).collect();
        let where_expression = cst.where_expression().map(|cst| lower_where_expression(s, e, &cst));
        PatternGuarded { pattern_guards, where_expression }
    })
}

fn lower_pattern_guard(s: &mut State, e: &Environment, cst: &cst::PatternGuard) -> PatternGuard {
    match cst {
        cst::PatternGuard::PatternGuardBinder(cst) => {
            s.push_binder_scope();
            let binder = cst.binder().map(|cst| lower_binder(s, e, &cst));
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
            PatternGuard { binder, expression }
        }
        cst::PatternGuard::PatternGuardExpression(cst) => {
            let binder = None;
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
            PatternGuard { binder, expression }
        }
    }
}

fn lower_bindings(
    s: &mut State,
    e: &Environment,
    cst: &cst::LetBindingStatements,
) -> Arc<[LetBinding]> {
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

    let mut bindings = vec![];
    for (kind, children) in chunks.into_iter() {
        match kind {
            Chunk::Pattern => {
                lower_pattern_bindings(s, e, &mut bindings, children);
            }
            Chunk::Equation => {
                lower_equation_bindings(s, e, &mut bindings, children);
            }
        }
    }

    bindings.into()
}

fn lower_pattern_bindings(
    s: &mut State,
    e: &Environment,
    bindings: &mut Vec<LetBinding>,
    children: impl Iterator<Item = cst::LetBinding>,
) {
    bindings.extend(children.map(|pattern| {
        let cst::LetBinding::LetBindingPattern(pattern) = &pattern else {
            unreachable!("invariant violated: expected LetBindingPattern");
        };
        let where_expression =
            pattern.where_expression().map(|cst| lower_where_expression(s, e, &cst));
        s.push_binder_scope();
        let binder = pattern.binder().map(|cst| lower_binder(s, e, &cst));
        LetBinding::Pattern { binder, where_expression }
    }))
}

fn lower_equation_bindings(
    s: &mut State,
    e: &Environment,
    bindings: &mut Vec<LetBinding>,
    children: impl Iterator<Item = cst::LetBinding>,
) {
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

    let mut in_scope = FxHashMap::default();
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
                    let id = s.source.allocate_ls(&cst);
                    signature = Some(id);
                }
                cst::LetBinding::LetBindingEquation(cst) => {
                    let id = s.source.allocate_le(&cst);
                    equations.push(id);
                }
            }
        }

        children.for_each(|cst| {
            if let cst::LetBinding::LetBindingEquation(cst) = cst {
                let id = s.source.allocate_le(&cst);
                equations.push(id);
            }
        });

        let equations = equations.into();
        let resolution = LetBindingResolution { signature, equations };

        if let Some(name) = name {
            in_scope.insert(name, resolution);
        }
    }

    let parent = s.graph_scope;
    let to_traverse = in_scope.values().cloned().collect_vec();

    let id = s.graph.inner.alloc(GraphNode::Let { parent, bindings: in_scope });
    s.graph_scope = Some(id);

    let root = e.module.syntax();
    bindings.extend(to_traverse.into_iter().map(|resolution| {
        s.with_scope(|s| {
            let signature = resolution.signature.and_then(|id| {
                let cst = s.source[id].to_node(root);
                cst.r#type().map(|t| lower_forall(s, e, &t))
            });
            let equations = resolution
                .equations
                .iter()
                .map(|&id| {
                    let cst = s.source[id].to_node(root);
                    lower_equation_like(
                        s,
                        e,
                        cst,
                        cst::LetBindingEquation::function_binders,
                        cst::LetBindingEquation::guarded_expression,
                    )
                })
                .collect();
            LetBinding::Name { signature, equations }
        })
    }));
}

pub(super) fn lower_equation_like<T: AstNode>(
    s: &mut State,
    e: &Environment,
    equation: T,
    binders: impl Fn(&T) -> Option<cst::FunctionBinders>,
    guarded: impl Fn(&T) -> Option<cst::GuardedExpression>,
) -> Equation {
    s.with_scope(|s| {
        s.push_binder_scope();
        let binders = binders(&equation)
            .map(|cst| cst.children().map(|cst| lower_binder(s, e, &cst)).collect())
            .unwrap_or_default();
        let guarded = guarded(&equation).map(|cst| lower_guarded(s, e, &cst));
        Equation { binders, guarded }
    })
}

fn lower_do_statement(s: &mut State, e: &Environment, cst: &cst::DoStatement) -> DoStatement {
    match cst {
        cst::DoStatement::DoStatementBind(cst) => {
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
            s.push_binder_scope();
            let binder = cst.binder().map(|cst| lower_binder(s, e, &cst));
            DoStatement::Bind { binder, expression }
        }
        cst::DoStatement::DoStatementLet(cst) => {
            let statements =
                cst.statements().map(|cst| lower_bindings(s, e, &cst)).unwrap_or_default();
            DoStatement::Let { statements }
        }
        cst::DoStatement::DoStatementDiscard(cst) => {
            let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
            DoStatement::Discard { expression }
        }
    }
}

fn lower_record_updates(
    s: &mut State,
    e: &Environment,
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
                let expression = cst.expression().map(|cst| lower_expression(s, e, &cst));
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
                    .map(|cst| lower_record_updates(s, e, &cst))
                    .unwrap_or_default();
                RecordUpdate::Branch { name, updates }
            }
        })
        .collect()
}

pub(super) fn lower_type(s: &mut State, e: &Environment, cst: &cst::Type) -> TypeId {
    let id = s.source.allocate_ty(cst);
    let kind = lower_type_kind(s, e, cst, id).unwrap_or(TypeKind::Unknown);
    s.associate_type_info(id, kind);
    id
}

fn lower_type_kind(
    s: &mut State,
    e: &Environment<'_>,
    cst: &cst::Type,
    id: la_arena::Idx<rowan::ast::AstPtr<cst::Type>>,
) -> Option<TypeKind> {
    Some(match cst {
        cst::Type::TypeApplicationChain(cst) => {
            let mut children = cst.children().map(|cst| lower_type(s, e, &cst));
            let function = children.next();
            let arguments = children.collect();
            TypeKind::ApplicationChain { function, arguments }
        }
        cst::Type::TypeArrow(cst) => {
            let mut children = cst.children().map(|cst| lower_type(s, e, &cst));
            let argument = children.next();
            let result = children.next();
            TypeKind::Arrow { argument, result }
        }
        cst::Type::TypeConstrained(cst) => {
            let mut children = cst.children().map(|cst| lower_type(s, e, &cst));
            let constraint = children.next();
            let constrained = children.next();
            TypeKind::Constrained { constraint, constrained }
        }
        cst::Type::TypeConstructor(cst) => {
            let (_, qualifier, name) =
                cst.name().map(|cst| lower_qualified_name(s, &cst, cst::QualifiedName::upper))?;
            let resolution = s.resolve_deferred(ResolutionDomain::Type, qualifier, name);
            TypeKind::Constructor { resolution }
        }
        // Rank-N Types must be scoped. See `lower_forall`.
        cst::Type::TypeForall(cst) => s.with_scope(|s| {
            s.push_forall_scope();
            let bindings =
                cst.children().map(|cst| lower_type_variable_binding(s, e, &cst)).collect();
            let r#type = cst.r#type().map(|cst| lower_type(s, e, &cst));
            TypeKind::Forall { bindings, r#type }
        }),
        cst::Type::TypeHole(_) => TypeKind::Hole,
        cst::Type::TypeInteger(_) => TypeKind::Integer,
        cst::Type::TypeKinded(cst) => {
            let mut children = cst.children().map(|cst| lower_type(s, e, &cst));
            let r#type = children.next();
            let kind = children.next();
            TypeKind::Kinded { r#type, kind }
        }
        cst::Type::TypeOperator(cst) => {
            let (_, qualifier, name) = cst
                .name()
                .map(|cst| lower_qualified_name(s, &cst, cst::QualifiedName::operator_name))?;
            let resolution = s.resolve_deferred(ResolutionDomain::Type, qualifier, name);
            TypeKind::Operator { resolution }
        }
        cst::Type::TypeOperatorChain(cst) => {
            let head = cst.r#type().map(|cst| lower_type(s, e, &cst));
            let tail = cst
                .children()
                .map(|cst| {
                    let qualified = cst.qualified()?;
                    let r#type = cst.r#type().map(|cst| lower_type(s, e, &cst));
                    Some(lower_pair(s, ResolutionDomain::Type, qualified, r#type))
                })
                .collect::<Option<Arc<_>>>()?;
            TypeKind::OperatorChain { head, tail }
        }
        cst::Type::TypeString(_) => TypeKind::String,
        cst::Type::TypeVariable(cst) => {
            let name = cst.name_token().map(|cst| {
                let text = cst.text();
                SmolStr::from(text)
            });
            let resolution = cst.name_token().and_then(|cst| {
                let text = cst.text();
                s.resolve_type_variable(id, text)
            });
            TypeKind::Variable { name, resolution }
        }
        cst::Type::TypeWildcard(_) => TypeKind::Wildcard,
        cst::Type::TypeRecord(cst) => {
            let items = cst.children().map(|cst| lower_row_item(s, e, &cst)).collect();
            let tail = cst.tail().and_then(|cst| {
                let cst = cst.r#type()?;
                Some(lower_type(s, e, &cst))
            });
            TypeKind::Record { items, tail }
        }
        cst::Type::TypeRow(cst) => {
            let items = cst.children().map(|cst| lower_row_item(s, e, &cst)).collect();
            let tail = cst.tail().and_then(|cst| {
                let cst = cst.r#type()?;
                Some(lower_type(s, e, &cst))
            });
            TypeKind::Row { items, tail }
        }
        cst::Type::TypeParenthesized(cst) => {
            let parenthesized = cst.r#type().map(|cst| lower_type(s, e, &cst));
            TypeKind::Parenthesized { parenthesized }
        }
    })
}

pub(super) fn lower_forall(s: &mut State, e: &Environment, cst: &cst::Type) -> TypeId {
    // To enable lexically-scoped type variables, we avoid calling `with_scope`
    // as to not reset to the parent scope once all the top-level `forall` have
    // been lowered. In `lower_type`, the `TypeForall` branch explicitly calls
    // `with_scope` in order to scope Rank-N type variables.
    if let cst::Type::TypeForall(f) = cst {
        let id = s.source.allocate_ty(cst);
        s.push_forall_scope();
        let bindings = f.children().map(|cst| lower_type_variable_binding(s, e, &cst)).collect();
        let r#type = f.r#type().map(|cst| lower_forall(s, e, &cst));
        let kind = TypeKind::Forall { bindings, r#type };
        s.associate_type_info(id, kind);
        id
    } else {
        lower_type(s, e, cst)
    }
}

fn lower_pair<T>(
    s: &mut State,
    domain: ResolutionDomain,
    qualified: cst::QualifiedName,
    element: Option<T>,
) -> OperatorPair<T> {
    let (_, qualifier, operator) =
        lower_qualified_name(s, &qualified, cst::QualifiedName::operator);
    let resolution = s.resolve_deferred(domain, qualifier, operator);
    OperatorPair { resolution, element }
}

pub(super) fn lower_qualified_name(
    s: &mut State,
    cst: &cst::QualifiedName,
    token: impl Fn(&cst::QualifiedName) -> Option<SyntaxToken>,
) -> (QualifiedNameId, Option<SmolStr>, Option<SmolStr>) {
    let id = s.source.allocate_qualified_name(cst);
    let qualifier = cst.qualifier().and_then(|cst| {
        let token = cst.text()?;
        let text = token.text().trim_end_matches(".");
        Some(SmolStr::from(text))
    });
    let name = token(cst).map(|cst| {
        let text = cst.text().trim_start_matches("(").trim_end_matches(")");
        SmolStr::from(text)
    });
    (id, qualifier, name)
}

pub(super) fn lower_type_variable_binding(
    s: &mut State,
    e: &Environment,
    cst: &cst::TypeVariableBinding,
) -> TypeVariableBinding {
    let id = s.source.allocate_tv(cst);
    let visible = cst.at().is_some();
    let name = cst.name().map(|cst| {
        let text = cst.text();
        SmolStr::from(text)
    });
    let kind = cst.kind().map(|cst| lower_type(s, e, &cst));
    if let Some(name) = &name {
        s.insert_bound_variable(name, id);
    }
    TypeVariableBinding { visible, id, name, kind }
}

fn lower_row_item(s: &mut State, e: &Environment, cst: &cst::TypeRowItem) -> TypeRowItem {
    let name = cst.name().and_then(|cst| {
        let token = cst.text()?;
        let text = token.text();
        Some(SmolStr::from(text))
    });
    let r#type = cst.r#type().map(|t| lower_type(s, e, &t));
    TypeRowItem { name, r#type }
}
