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
    let kind = match cst {
        cst::Binder::BinderTyped(t) => {
            let binder = t.binder().map(|b| lower_binder(s, e, &b));
            let r#type = t.r#type().map(|t| lower_type(s, e, &t));
            BinderKind::Typed { binder, r#type }
        }
        cst::Binder::BinderOperatorChain(o) => {
            let head = o.binder().map(|b| lower_binder(s, e, &b));
            let tail = o
                .children()
                .map(|p| {
                    let qualified = p.qualified();
                    let binder = p.binder().map(|b| lower_binder(s, e, &b));
                    lower_pair(s, ResolutionDomain::Term, qualified, binder)
                })
                .collect();
            BinderKind::OperatorChain { head, tail }
        }
        cst::Binder::BinderInteger(_) => BinderKind::Integer,
        cst::Binder::BinderNumber(_) => BinderKind::Number,
        cst::Binder::BinderConstructor(c) => {
            let (qualifier, name) = c
                .name()
                .map_or((None, None), |n| lower_qualified_name(&n, cst::QualifiedName::upper));
            let resolution = s.resolve_root(ResolutionDomain::Term, qualifier, name);
            let arguments = c.children().map(|b| lower_binder(s, e, &b)).collect();
            BinderKind::Constructor { resolution, arguments }
        }
        cst::Binder::BinderVariable(v) => {
            let variable = v.name_token().map(|t| {
                let text = t.text();
                SmolStr::from(text)
            });
            if let Some(name) = &variable {
                s.insert_binder(name, id);
            }
            BinderKind::Variable { variable }
        }
        cst::Binder::BinderNamed(n) => {
            let named = n.name_token().map(|t| {
                let text = t.text();
                SmolStr::from(text)
            });
            if let Some(name) = &named {
                s.insert_binder(name, id);
            }
            let binder = n.binder().map(|b| lower_binder(s, e, &b));
            BinderKind::Named { named, binder }
        }
        cst::Binder::BinderWildcard(_) => BinderKind::Wildcard,
        cst::Binder::BinderString(_) => BinderKind::String,
        cst::Binder::BinderChar(_) => BinderKind::Char,
        cst::Binder::BinderTrue(_) => BinderKind::Boolean { boolean: true },
        cst::Binder::BinderFalse(_) => BinderKind::Boolean { boolean: false },
        cst::Binder::BinderArray(a) => {
            let array = a.children().map(|b| lower_binder(s, e, &b)).collect();
            BinderKind::Array { array }
        }
        cst::Binder::BinderRecord(r) => {
            let lower_item = |i| match i {
                cst::RecordItem::RecordField(f) => {
                    let name = f.name().and_then(|t| {
                        let token = t.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let value = f.binder().map(|b| lower_binder(s, e, &b));
                    BinderRecordItem::RecordField { name, value }
                }
                cst::RecordItem::RecordPun(p) => {
                    let name = p.name().and_then(|t| {
                        let token = t.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    BinderRecordItem::RecordPun { name }
                }
            };
            let record = r.children().map(lower_item).collect();
            BinderKind::Record { record }
        }
        cst::Binder::BinderParenthesized(p) => {
            let parenthesized = p.binder().map(|b| lower_binder(s, e, &b));
            BinderKind::Parenthesized { parenthesized }
        }
    };
    s.associate_binder_info(id, kind);
    id
}

pub(super) fn lower_expression(
    s: &mut State,
    e: &Environment,
    cst: &cst::Expression,
) -> ExpressionId {
    let id = s.source.allocate_ex(cst);
    let kind = match cst {
        cst::Expression::ExpressionTyped(t) => {
            let expression = t.expression().map(|cst| lower_expression(s, e, &cst));
            let r#type = t.signature().map(|cst| lower_type(s, e, &cst));
            ExpressionKind::Typed { expression, r#type }
        }
        cst::Expression::ExpressionOperatorChain(o) => {
            let head = o.expression().map(|cst| lower_expression(s, e, &cst));
            let tail = o
                .children()
                .map(|p| {
                    let qualified = p.qualified();
                    let expression = p.expression().map(|cst| lower_expression(s, e, &cst));
                    lower_pair(s, ResolutionDomain::Term, qualified, expression)
                })
                .collect();
            ExpressionKind::OperatorChain { head, tail }
        }
        cst::Expression::ExpressionInfixChain(i) => {
            let head = i.expression().map(|cst| lower_expression(s, e, &cst));
            let tail = i
                .children()
                .map(|p| {
                    let tick = p.tick().and_then(|cst| {
                        let cst = cst.expression()?;
                        Some(lower_expression(s, e, &cst))
                    });
                    let element = p.expression().map(|cst| lower_expression(s, e, &cst));
                    InfixPair { tick, element }
                })
                .collect();
            ExpressionKind::InfixChain { head, tail }
        }
        cst::Expression::ExpressionNegate(n) => {
            let expression = n.expression().map(|cst| lower_expression(s, e, &cst));
            ExpressionKind::Negate { expression }
        }
        cst::Expression::ExpressionApplicationChain(a) => {
            let lower_argument =
                |s: &mut State, e: &Environment, cst: &cst::ExpressionArgument| match cst {
                    cst::ExpressionArgument::ExpressionTypeArgument(t) => {
                        let id = t.r#type().map(|cst| lower_type(s, e, &cst));
                        ExpressionArgument::Type(id)
                    }
                    cst::ExpressionArgument::ExpressionTermArgument(t) => {
                        let id = t.expression().map(|cst| lower_expression(s, e, &cst));
                        ExpressionArgument::Term(id)
                    }
                };

            let function = a.expression().map(|cst| lower_expression(s, e, &cst));
            let arguments = a.children().map(|cst| lower_argument(s, e, &cst)).collect();

            ExpressionKind::Application { function, arguments }
        }
        cst::Expression::ExpressionIfThenElse(i) => {
            let r#if = i.r#if().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(s, e, &cst))
            });
            let then = i.then().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(s, e, &cst))
            });
            let r#else = i.r#else().and_then(|cst| {
                let cst = cst.expression()?;
                Some(lower_expression(s, e, &cst))
            });
            ExpressionKind::IfThenElse { r#if, then, r#else }
        }
        cst::Expression::ExpressionLetIn(l) => s.with_scope(|s| {
            let bindings = l.bindings().map(|cst| lower_bindings(s, e, &cst)).unwrap_or_default();
            let expression = l.expression().map(|cst| lower_expression(s, e, &cst));
            ExpressionKind::LetIn { bindings, expression }
        }),
        cst::Expression::ExpressionLambda(l) => s.with_scope(|s| {
            s.push_binder_scope();
            let binders = l
                .function_binders()
                .map(|b| {
                    let children = b.children();
                    children.map(|cst| lower_binder(s, e, &cst)).collect()
                })
                .unwrap_or_default();
            let expression = l.expression().map(|cst| lower_expression(s, e, &cst));
            ExpressionKind::Lambda { binders, expression }
        }),
        cst::Expression::ExpressionCaseOf(c) => {
            let lower_case_branch = |s: &mut State, e: &Environment, cst: &cst::CaseBranch| {
                let binders = cst
                    .binders()
                    .map(|b| b.children().map(|cst| lower_binder(s, e, &cst)).collect())
                    .unwrap_or_default();
                let guarded_expression =
                    cst.guarded_expression().map(|cst| lower_guarded(s, e, &cst));
                CaseBranch { binders, guarded_expression }
            };

            let trunk = c
                .trunk()
                .map(|t| t.children().map(|cst| lower_expression(s, e, &cst)).collect())
                .unwrap_or_default();
            let branches = c
                .branches()
                .map(|b| b.children().map(|cst| lower_case_branch(s, e, &cst)).collect())
                .unwrap_or_default();

            ExpressionKind::CaseOf { trunk, branches }
        }
        cst::Expression::ExpressionDo(d) => s.with_scope(|s| {
            let qualifier = d.qualifier().and_then(|q| {
                let token = q.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });

            const BIND: Option<SmolStr> = Some(SmolStr::new_inline("bind"));
            const DISCARD: Option<SmolStr> = Some(SmolStr::new_inline("discard"));

            let bind = s.resolve_term(qualifier.clone(), BIND);
            let discard = s.resolve_term(qualifier.clone(), DISCARD);

            let statements = d
                .statements()
                .map(|cst| cst.children().map(|cst| lower_do_statement(s, e, &cst)).collect())
                .unwrap_or_default();

            ExpressionKind::Do { bind, discard, statements }
        }),
        cst::Expression::ExpressionAdo(a) => s.with_scope(|s| {
            let qualifier = a.qualifier().and_then(|q| {
                let token = q.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });

            const MAP: Option<SmolStr> = Some(SmolStr::new_inline("map"));
            const APPLY: Option<SmolStr> = Some(SmolStr::new_inline("apply"));

            let map = s.resolve_term(qualifier.clone(), MAP);
            let apply = s.resolve_term(qualifier.clone(), APPLY);

            let statements = a
                .statements()
                .map(|cst| cst.children().map(|cst| lower_do_statement(s, e, &cst)).collect())
                .unwrap_or_default();
            let expression = a.expression().map(|cst| lower_expression(s, e, &cst));

            ExpressionKind::Ado { map, apply, statements, expression }
        }),
        cst::Expression::ExpressionConstructor(c) => {
            let (qualifier, name) = c
                .name()
                .map(|n| lower_qualified_name(&n, cst::QualifiedName::upper))
                .unwrap_or_default();
            let resolution = s.resolve_root(ResolutionDomain::Term, qualifier, name);
            ExpressionKind::Constructor { resolution }
        }
        cst::Expression::ExpressionVariable(v) => {
            let (qualifier, name) = v
                .name()
                .map(|n| lower_qualified_name(&n, cst::QualifiedName::lower))
                .unwrap_or_default();
            let resolution = s.resolve_term(qualifier, name);
            ExpressionKind::Variable { resolution }
        }
        cst::Expression::ExpressionOperatorName(o) => {
            let (qualifier, name) = o
                .name()
                .map(|n| lower_qualified_name(&n, cst::QualifiedName::operator_name))
                .unwrap_or_default();
            let resolution = s.resolve_root(ResolutionDomain::Term, qualifier, name);
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
        cst::Expression::ExpressionArray(a) => {
            let array = a.children().map(|cst| lower_expression(s, e, &cst)).collect();
            ExpressionKind::Array { array }
        }
        cst::Expression::ExpressionRecord(r) => {
            let lower_item = |s: &mut State, i| match i {
                cst::RecordItem::RecordField(f) => {
                    let name = f.name().and_then(|t| {
                        let token = t.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let value = f.expression().map(|cst| lower_expression(s, e, &cst));
                    ExpressionRecordItem::RecordField { name, value }
                }
                cst::RecordItem::RecordPun(p) => {
                    let name = p.name().and_then(|t| {
                        let token = t.text()?;
                        let text = token.text();
                        Some(SmolStr::from(text))
                    });
                    let resolution = s.resolve_term(None, name.clone());
                    ExpressionRecordItem::RecordPun { name, resolution }
                }
            };
            let record = r.children().map(|cst| lower_item(s, cst)).collect();
            ExpressionKind::Record { record }
        }
        cst::Expression::ExpressionParenthesized(p) => {
            let parenthesized = p.expression().map(|p| lower_expression(s, e, &p));
            ExpressionKind::Parenthesized { parenthesized }
        }
        cst::Expression::ExpressionRecordAccess(r) => {
            let record = r.expression().map(|cst| lower_expression(s, e, &cst));
            let labels = r
                .children()
                .map(|t| {
                    let token = t.text()?;
                    let text = token.text();
                    Some(SmolStr::from(text))
                })
                .collect();
            ExpressionKind::RecordAccess { record, labels }
        }
        cst::Expression::ExpressionRecordUpdate(r) => {
            let updates =
                r.record_updates().map(|cst| lower_record_updates(s, e, &cst)).unwrap_or_default();
            ExpressionKind::RecordUpdate { updates }
        }
    };
    s.intermediate.insert_expression_kind(id, kind);
    id
}

fn lower_guarded(
    s: &mut State,
    e: &Environment,
    cst: &cst::GuardedExpression,
) -> GuardedExpression {
    match cst {
        cst::GuardedExpression::Unconditional(u) => {
            let where_expression = u.where_expression().map(|w| lower_where_expression(s, e, &w));
            GuardedExpression::Unconditional { where_expression }
        }
        cst::GuardedExpression::Conditionals(c) => {
            let pattern_guarded = c.children().map(|p| lower_pattern_guarded(s, e, &p)).collect();
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
        let pattern_guards = cst.children().map(|p| lower_pattern_guard(s, e, &p)).collect();
        let where_expression = cst.where_expression().map(|w| lower_where_expression(s, e, &w));
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

    let chunks = cst.children().chunk_by(|b| match b {
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
        let where_expression = pattern.where_expression().map(|w| lower_where_expression(s, e, &w));
        s.push_binder_scope();
        let binder = pattern.binder().map(|b| lower_binder(s, e, &b));
        LetBinding::Pattern { binder, where_expression }
    }))
}

fn lower_equation_bindings(
    s: &mut State,
    e: &Environment,
    bindings: &mut Vec<LetBinding>,
    children: impl Iterator<Item = cst::LetBinding>,
) {
    let children = children.chunk_by(|b| match b {
        cst::LetBinding::LetBindingPattern(_) => {
            unreachable!("invariant violated: expected LetBindingSignature / LetBindingEquation");
        }
        cst::LetBinding::LetBindingSignature(s) => s.name_token().map(|t| {
            let text = t.text();
            SmolStr::from(text)
        }),
        cst::LetBinding::LetBindingEquation(e) => e.name_token().map(|t| {
            let text = t.text();
            SmolStr::from(text)
        }),
    });

    let mut in_scope = FxHashMap::default();
    for (name, mut children) in children.into_iter() {
        let mut signature = None;
        let mut equations = vec![];

        if let Some(binding) = children.next() {
            match binding {
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

        children.for_each(|binding| {
            if let cst::LetBinding::LetBindingEquation(cst) = binding {
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
            .map(|b| b.children().map(|b| lower_binder(s, e, &b)).collect())
            .unwrap_or_default();
        let guarded = guarded(&equation).map(|g| lower_guarded(s, e, &g));
        Equation { binders, guarded }
    })
}

fn lower_do_statement(s: &mut State, e: &Environment, cst: &cst::DoStatement) -> DoStatement {
    match cst {
        cst::DoStatement::DoStatementBind(b) => {
            s.push_binder_scope();
            let binder = b.binder().map(|cst| lower_binder(s, e, &cst));
            let expression = b.expression().map(|cst| lower_expression(s, e, &cst));
            DoStatement::Bind { binder, expression }
        }
        cst::DoStatement::DoStatementLet(l) => {
            let statements =
                l.statements().map(|cst| lower_bindings(s, e, &cst)).unwrap_or_default();
            DoStatement::Let { statements }
        }
        cst::DoStatement::DoStatementDiscard(d) => {
            let expression = d.expression().map(|cst| lower_expression(s, e, &cst));
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
        .map(|u| match u {
            cst::RecordUpdate::RecordUpdateLeaf(l) => {
                let name = l.name().and_then(|l| {
                    let token = l.text()?;
                    let text = token.text();
                    Some(SmolStr::from(text))
                });
                let expression = l.expression().map(|cst| lower_expression(s, e, &cst));
                RecordUpdate::Leaf { name, expression }
            }
            cst::RecordUpdate::RecordUpdateBranch(b) => {
                let name = b.name().and_then(|l| {
                    let token = l.text()?;
                    let text = token.text();
                    Some(SmolStr::from(text))
                });
                let updates = b
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
    let kind = match cst {
        cst::Type::TypeApplicationChain(a) => {
            let mut children = a.children().map(|t| lower_type(s, e, &t));
            let function = children.next();
            let arguments = children.collect();
            TypeKind::ApplicationChain { function, arguments }
        }
        cst::Type::TypeArrow(a) => {
            let mut children = a.children().map(|t| lower_type(s, e, &t));
            let argument = children.next();
            let result = children.next();
            TypeKind::Arrow { argument, result }
        }
        cst::Type::TypeConstrained(c) => {
            let mut children = c.children().map(|t| lower_type(s, e, &t));
            let constraint = children.next();
            let constrained = children.next();
            TypeKind::Constrained { constraint, constrained }
        }
        cst::Type::TypeConstructor(c) => {
            let (qualifier, name) = c
                .name()
                .map_or((None, None), |n| lower_qualified_name(&n, cst::QualifiedName::upper));
            let resolution = s.resolve_root(ResolutionDomain::Type, qualifier, name);
            TypeKind::Constructor { resolution }
        }
        // Rank-N Types must be scoped. See `lower_forall`.
        cst::Type::TypeForall(f) => s.with_scope(|s| {
            s.push_forall_scope();
            let bindings = f.children().map(|b| lower_type_variable_binding(s, e, &b)).collect();
            let r#type = f.r#type().map(|t| lower_type(s, e, &t));
            TypeKind::Forall { bindings, r#type }
        }),
        cst::Type::TypeHole(_) => TypeKind::Hole,
        cst::Type::TypeInteger(_) => TypeKind::Integer,
        cst::Type::TypeKinded(k) => {
            let mut children = k.children().map(|t| lower_type(s, e, &t));
            let r#type = children.next();
            let kind = children.next();
            TypeKind::Kinded { r#type, kind }
        }
        cst::Type::TypeOperator(o) => {
            let (qualifier, name) = o
                .name()
                .map(|n| lower_qualified_name(&n, cst::QualifiedName::operator_name))
                .unwrap_or_default();
            let resolution = s.resolve_root(ResolutionDomain::Type, qualifier, name);
            TypeKind::Operator { resolution }
        }
        cst::Type::TypeOperatorChain(o) => {
            let head = o.r#type().map(|t| lower_type(s, e, &t));
            let tail = o
                .children()
                .map(|p| {
                    let qualified = p.qualified();
                    let r#type = p.r#type().map(|t| lower_type(s, e, &t));
                    lower_pair(s, ResolutionDomain::Type, qualified, r#type)
                })
                .collect();
            TypeKind::OperatorChain { head, tail }
        }
        cst::Type::TypeString(_) => TypeKind::String,
        cst::Type::TypeVariable(v) => {
            let name = v.name_token().map(|t| {
                let text = t.text();
                SmolStr::from(text)
            });
            let resolution = v.name_token().and_then(|t| {
                let text = t.text();
                s.resolve_type_variable(id, text)
            });
            TypeKind::Variable { name, resolution }
        }
        cst::Type::TypeWildcard(_) => TypeKind::Wildcard,
        cst::Type::TypeRecord(r) => {
            let items = r.children().map(|i| lower_row_item(s, e, &i)).collect();
            let tail = r.tail().and_then(|t| t.r#type()).map(|t| lower_type(s, e, &t));
            TypeKind::Record { items, tail }
        }
        cst::Type::TypeRow(r) => {
            let items = r.children().map(|i| lower_row_item(s, e, &i)).collect();
            let tail = r.tail().and_then(|t| t.r#type()).map(|t| lower_type(s, e, &t));
            TypeKind::Row { items, tail }
        }
        cst::Type::TypeParenthesized(p) => {
            let parenthesized = p.r#type().map(|p| lower_type(s, e, &p));
            TypeKind::Parenthesized { parenthesized }
        }
    };
    s.associate_type_info(id, kind);
    id
}

pub(super) fn lower_forall(s: &mut State, e: &Environment, cst: &cst::Type) -> TypeId {
    // To enable lexically-scoped type variables, we avoid calling `with_scope`
    // as to not reset to the parent scope once all the top-level `forall` have
    // been lowered. In `lower_type`, the `TypeForall` branch explicitly calls
    // `with_scope` in order to scope Rank-N type variables.
    if let cst::Type::TypeForall(f) = cst {
        let id = s.source.allocate_ty(cst);
        s.push_forall_scope();
        let bindings = f.children().map(|b| lower_type_variable_binding(s, e, &b)).collect();
        let r#type = f.r#type().map(|t| lower_forall(s, e, &t));
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
    qualified: Option<cst::QualifiedName>,
    element: Option<T>,
) -> OperatorPair<T> {
    let (qualifier, operator) =
        qualified.map_or((None, None), |q| lower_qualified_name(&q, cst::QualifiedName::operator));
    let resolution = s.resolve_root(domain, qualifier, operator);
    OperatorPair { resolution, element }
}

pub(super) fn lower_qualified_name(
    cst: &cst::QualifiedName,
    token: impl Fn(&cst::QualifiedName) -> Option<SyntaxToken>,
) -> (Option<SmolStr>, Option<SmolStr>) {
    let qualifier = cst.qualifier().and_then(|q| {
        let q = q.text()?;
        let text = q.text();
        Some(SmolStr::from(text))
    });
    let name = token(cst).map(|t| {
        let text = t.text().trim_start_matches("(").trim_end_matches(")");
        SmolStr::from(text)
    });
    (qualifier, name)
}

pub(super) fn lower_type_variable_binding(
    s: &mut State,
    e: &Environment,
    cst: &cst::TypeVariableBinding,
) -> TypeVariableBinding {
    let id = s.source.allocate_tv(cst);
    let visible = cst.at().is_some();
    let name = cst.name().map(|t| {
        let text = t.text();
        SmolStr::from(text)
    });
    let kind = cst.kind().map(|k| lower_type(s, e, &k));
    if let Some(name) = &name {
        s.insert_bound_variable(name, id);
    }
    TypeVariableBinding { visible, id, name, kind }
}

fn lower_row_item(s: &mut State, e: &Environment, cst: &cst::TypeRowItem) -> TypeRowItem {
    let name = cst.name().and_then(|l| {
        let token = l.text()?;
        let text = token.text();
        Some(SmolStr::from(text))
    });
    let r#type = cst.r#type().map(|t| lower_type(s, e, &t));
    TypeRowItem { name, r#type }
}
