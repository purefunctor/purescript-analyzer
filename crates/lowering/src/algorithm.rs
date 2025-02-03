use std::iter;

use indexing::{ExprItem, ExprItemId, IndexingResult, ValueGroupId};
use rowan::ast::{AstNode, AstPtr};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::{cst, SyntaxToken};

use crate::{
    BinderId, BinderKind, CaseBranch, DoStatement, ExpressionArgument, ExpressionId,
    ExpressionKind, GuardedExpression, LetBinding, LetBindingId, LetBindingKindId, LoweredEquation,
    LoweredExprItem, LoweringMap, LoweringResult, OperatorPair, PatternGuard, PatternGuarded,
    SourceMap, TickPair, TypeId, TypeKind, WhereExpression,
};

#[derive(Default)]
struct State {
    source_map: SourceMap,
    lowering_map: LoweringMap,
}

pub(super) fn lower_module(module: &cst::Module, index: &IndexingResult) -> LoweringResult {
    let mut state = State::default();

    for (item_id, _, item) in index.nominal.iter_expr() {
        lower_expr_item(&mut state, module, index, item_id, item);
    }

    let State { source_map, lowering_map } = state;
    LoweringResult { source_map, lowering_map }
}

fn lower_expr_item(
    state: &mut State,
    module: &cst::Module,
    index: &IndexingResult,
    item_id: ExprItemId,
    item: &ExprItem,
) {
    let item = match item {
        ExprItem::Constructor(_) => todo!(),
        ExprItem::Instance(_) => todo!(),
        ExprItem::Derive(_) => todo!(),
        ExprItem::ClassMember(_) => todo!(),
        ExprItem::Value(v) => lower_value_group(state, module, index, v),
        ExprItem::Foreign(_) => todo!(),
        ExprItem::Operator(_) => todo!(),
    };
    state.lowering_map.expr_item.insert(item_id, item);
}

fn lower_value_group(
    state: &mut State,
    module: &cst::Module,
    index: &IndexingResult,
    group: &ValueGroupId,
) -> LoweredExprItem {
    let root = module.syntax();

    // let-else = anti-hadouken
    let mut signature = None;
    'signature: {
        let Some(id) = group.signature else { break 'signature };
        let Some(ptr) = index.source_map.declaration_ptr(id) else { break 'signature };
        let Some(node) = ptr.try_to_node(root) else { break 'signature };
        let cst::Declaration::ValueSignature(s) = node else { break 'signature };
        let Some(s) = s.signature() else { break 'signature };
        signature = Some(lower_type(state, &s));
    };

    // The resilient nature of the parser makes it possible to extract
    // information from source files that produce partially-valid CSTs.
    // As such, traversals over the CST must be written with this in
    // mind to make available as much information as possible.
    //
    // For instance, in the following snippet, the compiler should
    // be able to provide information about the `Just` binder despite
    // the equation itself being invalid.
    //
    // ```
    // fromMaybe :: forall a. a -> Maybe a -> a
    // fromMaybe _ (Just a) =
    // fromMaybe a Nothing = a
    // ```
    let count = group.equations.len();
    let mut equations: Vec<_> = iter::repeat_with(LoweredEquation::default).take(count).collect();

    let id_iter = group.equations.iter().copied();
    let equations_iter = equations.iter_mut();

    for (id, equation) in id_iter.zip(equations_iter) {
        let Some(ptr) = index.source_map.declaration_ptr(id) else { continue };
        let Some(node) = ptr.try_to_node(root) else { continue };
        let cst::Declaration::ValueEquation(e) = node else { continue };
        lower_equation_like(state, equation, e.function_binders(), e.guarded_expression());
    }

    LoweredExprItem::Value { signature, equations }
}

fn lower_type(state: &mut State, cst: &cst::Type) -> TypeId {
    let kind = match &cst {
        cst::Type::TypeApplicationChain(_a) => TypeKind::ApplicationChain,
        cst::Type::TypeArrow(_a) => TypeKind::Arrow,
        cst::Type::TypeConstrained(_c) => TypeKind::Constrained,
        cst::Type::TypeConstructor(_c) => TypeKind::Constructor,
        cst::Type::TypeForall(_f) => TypeKind::Forall,
        cst::Type::TypeHole(_h) => TypeKind::Hole,
        cst::Type::TypeInteger(_i) => TypeKind::Integer,
        cst::Type::TypeKinded(_k) => TypeKind::Kinded,
        cst::Type::TypeOperator(_o) => TypeKind::Operator,
        cst::Type::TypeOperatorChain(_o) => TypeKind::OperatorChain,
        cst::Type::TypeString(_s) => TypeKind::String,
        cst::Type::TypeVariable(_v) => TypeKind::Variable,
        cst::Type::TypeVariableBinding(_v) => TypeKind::VariableBinding,
        cst::Type::TypeWildcard(_w) => TypeKind::Wildcard,
        cst::Type::TypeRecord(_r) => TypeKind::Record,
        cst::Type::TypeRow(_r) => TypeKind::Row,
        cst::Type::TypeParenthesized(_p) => TypeKind::Parenthesized,
    };
    state.source_map.insert_type(cst, kind)
}

fn lower_binder(state: &mut State, cst: &cst::Binder) -> BinderId {
    let kind = match cst {
        cst::Binder::BinderTyped(_t) => BinderKind::Typed,
        cst::Binder::BinderOperatorChain(_o) => BinderKind::OperatorChain,
        cst::Binder::BinderInteger(_i) => BinderKind::Integer,
        cst::Binder::BinderNumber(_n) => BinderKind::Number,
        cst::Binder::BinderConstructor(_c) => BinderKind::Constructor,
        cst::Binder::BinderVariable(_v) => BinderKind::Variable,
        cst::Binder::BinderNamed(_n) => BinderKind::Named,
        cst::Binder::BinderWildcard(_w) => BinderKind::Wildcard,
        cst::Binder::BinderString(_s) => BinderKind::String,
        cst::Binder::BinderChar(_c) => BinderKind::Char,
        cst::Binder::BinderTrue(_t) => BinderKind::True,
        cst::Binder::BinderFalse(_f) => BinderKind::False,
        cst::Binder::BinderArray(_a) => BinderKind::Array,
        cst::Binder::BinderRecord(_r) => BinderKind::Record,
        cst::Binder::BinderParenthesized(_p) => BinderKind::Parenthesized,
    };
    state.source_map.insert_binder(cst, kind)
}

fn lower_expression(state: &mut State, cst: &cst::Expression) -> ExpressionId {
    let kind = match cst {
        cst::Expression::ExpressionTyped(t) => {
            let expression = t.expression().map(|e| lower_expression(state, &e));
            let signature = t.signature().map(|s| lower_type(state, &s));
            ExpressionKind::Typed { expression, signature }
        }
        cst::Expression::ExpressionOperatorChain(o) => {
            let lower_pair = |state: &mut State, p: &cst::ExpressionOperatorPair| {
                let qualified = p.qualified();
                let qualifier = qualified.as_ref().and_then(|q| {
                    let q = q.qualifier()?;
                    let t = q.text()?;
                    Some(SmolStr::from(t.text()))
                });
                let operator = qualified.as_ref().and_then(|q| {
                    let o = q.operator()?;
                    Some(SmolStr::from(o.text()))
                });
                let element = p.expression().map(|e| lower_expression(state, &e));
                OperatorPair { qualifier, operator, element }
            };

            let head = o.expression().map(|e| lower_expression(state, &e));
            let tail: Vec<_> = o.children().map(|p| lower_pair(state, &p)).collect();

            ExpressionKind::OperatorChain { head, tail }
        }
        cst::Expression::ExpressionInfixChain(i) => {
            let lower_pair = |state: &mut State, p: &cst::ExpressionInfixPair| {
                let tick = p.tick().and_then(|t| {
                    let e = t.expression()?;
                    Some(lower_expression(state, &e))
                });
                let element = p.expression().map(|e| lower_expression(state, &e));
                TickPair { tick, element }
            };

            let head = i.expression().map(|e| lower_expression(state, &e));
            let tail: Vec<_> = i.children().map(|p| lower_pair(state, &p)).collect();

            ExpressionKind::InfixChain { head, tail }
        }
        cst::Expression::ExpressionNegate(n) => {
            let expression = n.expression().map(|e| lower_expression(state, &e));
            ExpressionKind::Negate { expression }
        }
        cst::Expression::ExpressionApplicationChain(a) => {
            let lower_argument = |state: &mut State, a: &cst::ExpressionArgument| match a {
                cst::ExpressionArgument::ExpressionTypeArgument(t) => {
                    let t = t.type_argument().map(|t| lower_type(state, &t));
                    ExpressionArgument::Type(t)
                }
                cst::ExpressionArgument::ExpressionTermArgument(t) => {
                    let e = t.expression().map(|e| lower_expression(state, &e));
                    ExpressionArgument::Expression(e)
                }
            };

            let head = a.expression().map(|e| lower_expression(state, &e));
            let tail: Vec<_> = a.children().map(|a| lower_argument(state, &a)).collect();

            ExpressionKind::ApplicationChain { head, tail }
        }
        cst::Expression::ExpressionIfThenElse(i) => {
            let r#if = i.r#if().and_then(|c| {
                let e = c.expression()?;
                Some(lower_expression(state, &e))
            });
            let then = i.then().and_then(|t| {
                let e = t.expression()?;
                Some(lower_expression(state, &e))
            });
            let r#else = i.r#else().and_then(|e| {
                let e = e.expression()?;
                Some(lower_expression(state, &e))
            });
            ExpressionKind::IfThenElse { r#if, then, r#else }
        }
        cst::Expression::ExpressionLetIn(l) => {
            let bindings = l.bindings().map_or(vec![], |b| lower_let_binding_statements(state, &b));
            let expression = l.expression().map(|e| lower_expression(state, &e));
            ExpressionKind::LetIn { bindings, expression }
        }
        cst::Expression::ExpressionLambda(l) => {
            let binders = l.function_binders().map_or(vec![], |b| {
                let children = b.children();
                children.map(|b| lower_binder(state, &b)).collect()
            });
            let expression = l.expression().map(|e| lower_expression(state, &e));
            ExpressionKind::Lambda { binders, expression }
        }
        cst::Expression::ExpressionCaseOf(c) => {
            let lower_case_branch = |state: &mut State, cst: &cst::CaseBranch| {
                let binders = cst
                    .binders()
                    .map_or(vec![], |b| b.children().map(|b| lower_binder(state, &b)).collect());
                let guarded_expression =
                    cst.guarded_expression().map(|g| lower_guarded_expression(state, &g));
                CaseBranch { binders, guarded_expression }
            };

            let trunk = c
                .trunk()
                .map_or(vec![], |t| t.children().map(|e| lower_expression(state, &e)).collect());
            let branches = c
                .branches()
                .map_or(vec![], |b| b.children().map(|b| lower_case_branch(state, &b)).collect());

            ExpressionKind::CaseOf { trunk, branches }
        }
        cst::Expression::ExpressionDo(d) => {
            let qualifier = d.qualifier().and_then(|t| {
                let token = t.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });
            let statements = d
                .statements()
                .map_or(vec![], |s| s.children().map(|s| lower_do_statement(state, &s)).collect());
            ExpressionKind::Do { qualifier, statements }
        }
        cst::Expression::ExpressionAdo(a) => {
            let qualifier = a.qualifier().and_then(|t| {
                let token = t.text()?;
                let text = token.text();
                Some(SmolStr::from(text))
            });
            let statements = a
                .statements()
                .map_or(vec![], |s| s.children().map(|s| lower_do_statement(state, &s)).collect());
            let expression = a.expression().map(|e| lower_expression(state, &e));
            ExpressionKind::Ado { qualifier, statements, expression }
        }
        cst::Expression::ExpressionConstructor(c) => {
            let (qualifier, name) = c
                .name()
                .map(|n| lower_qualified_name(&n, cst::QualifiedName::upper))
                .unwrap_or_default();
            ExpressionKind::Constructor { qualifier, name }
        }
        cst::Expression::ExpressionVariable(v) => {
            let (qualifier, name) = v
                .name()
                .map(|n| lower_qualified_name(&n, cst::QualifiedName::lower))
                .unwrap_or_default();
            ExpressionKind::Variable { qualifier, name }
        }
        cst::Expression::ExpressionOperatorName(o) => {
            let (qualifier, name) = o
                .name()
                .map(|n| lower_qualified_name(&n, cst::QualifiedName::operator_name))
                .unwrap_or_default();
            ExpressionKind::OperatorName { qualifier, name }
        }
        cst::Expression::ExpressionSection(_) => ExpressionKind::Section,
        cst::Expression::ExpressionHole(_) => ExpressionKind::Hole,
        // TODO: These need to be lowered for exhaustiveness checking
        // but we'll cross that bridge when we get there.
        cst::Expression::ExpressionString(_s) => ExpressionKind::String,
        cst::Expression::ExpressionChar(_c) => ExpressionKind::Char,
        cst::Expression::ExpressionTrue(_t) => ExpressionKind::True,
        cst::Expression::ExpressionFalse(_f) => ExpressionKind::False,
        cst::Expression::ExpressionInteger(_i) => ExpressionKind::Integer,
        cst::Expression::ExpressionNumber(_n) => ExpressionKind::Number,
        // TODO: Array/Record
        //
        // Think about how to lower record puns that makes sense for resolution?
        cst::Expression::ExpressionArray(_a) => ExpressionKind::Array,
        cst::Expression::ExpressionRecord(_r) => ExpressionKind::Record,
        // TODO: Parenthesis/RecordAccess are quite easy to do as well
        cst::Expression::ExpressionParenthesized(_p) => ExpressionKind::Parenthesized,
        cst::Expression::ExpressionRecordAccess(_r) => ExpressionKind::RecordAccess,
        // TODO: RecordUpdate
        //
        // This one is tricky as well, depends on Record lowering
        cst::Expression::ExpressionRecordUpdate(_r) => ExpressionKind::RecordUpdate,
    };
    state.source_map.insert_expression(cst, kind)
}

fn lower_let_binding_statements(
    state: &mut State,
    cst: &cst::LetBindingStatements,
) -> Vec<LetBindingId> {
    let mut nominal = FxHashMap::default();
    cst.children().map(|b| lower_let_binding(state, &mut nominal, &b)).collect()
}

fn lower_let_binding(
    state: &mut State,
    nominal: &mut FxHashMap<SmolStr, LetBindingKindId>,
    cst: &cst::LetBinding,
) -> LetBindingId {
    fn insert_kind_id(
        state: &mut State,
        ptr: &cst::LetBinding,
        kind_id: LetBindingKindId,
    ) -> LetBindingId {
        let ptr = AstPtr::new(ptr);
        let (index, _) = state.source_map.let_bindings.insert_full(ptr, kind_id);
        LetBindingId::from_raw(index)
    }

    fn insert_kind(
        state: &mut State,
        ptr: &cst::LetBinding,
        kind: LetBinding,
    ) -> (LetBindingKindId, LetBindingId) {
        let index = state.source_map.let_bindings_grouped.len();
        let kind_id = LetBindingKindId::from_raw(index);
        state.source_map.let_bindings_grouped.push(kind);
        let ptr_id = insert_kind_id(state, ptr, kind_id);
        (kind_id, ptr_id)
    }

    match cst {
        cst::LetBinding::LetBindingPattern(p) => {
            let kind = lower_let_binding_pattern(state, p);
            let (_, ptr_id) = insert_kind(state, cst, kind);
            ptr_id
        }
        cst::LetBinding::LetBindingSignature(s) => {
            let token = s.name_token();
            let name = token.as_ref().map(|t| t.text());
            let signature = s.signature().map(|s| lower_type(state, &s));

            if let Some(name) = name {
                if let Some(&kind_id) = nominal.get(name) {
                    let index: usize = kind_id.into();
                    let group = &mut state.source_map.let_bindings_grouped[index];
                    if let LetBinding::Value { signature: s, .. } = group {
                        if let Some(existing) = s {
                            unimplemented!(
                                "{:?} <-> {:?} : duplicate let bindings",
                                existing,
                                signature
                            );
                        } else {
                            *s = signature;
                        }
                    }
                    return insert_kind_id(state, cst, kind_id);
                }
            }

            let name = name.map(SmolStr::from);
            let for_nominal = name.clone();

            let equations = vec![];
            let kind = LetBinding::Value { name, signature, equations };

            let (kind_id, ptr_id) = insert_kind(state, cst, kind);

            if let Some(name) = for_nominal {
                nominal.insert(name, kind_id);
            }

            ptr_id
        }
        cst::LetBinding::LetBindingEquation(e) => {
            let token = e.name_token();
            let name = token.as_ref().map(|t| t.text());

            let mut equation = LoweredEquation::default();
            lower_equation_like(state, &mut equation, e.function_binders(), e.guarded_expression());

            if let Some(name) = name {
                if let Some(&kind_id) = nominal.get(name) {
                    let index: usize = kind_id.into();
                    let group = &mut state.source_map.let_bindings_grouped[index];
                    if let LetBinding::Value { equations, .. } = group {
                        equations.push(equation);
                    }
                    return insert_kind_id(state, cst, kind_id);
                }
            }

            let name = name.map(SmolStr::from);
            let for_nominal = name.clone();

            let signature = None;
            let equations = vec![equation];
            let kind = LetBinding::Value { name, signature, equations };

            let (kind_id, ptr_id) = insert_kind(state, cst, kind);

            if let Some(name) = for_nominal {
                nominal.insert(name, kind_id);
            }

            ptr_id
        }
    }
}

fn lower_let_binding_pattern(state: &mut State, cst: &cst::LetBindingPattern) -> LetBinding {
    let pattern = cst.binder().map(|b| lower_binder(state, &b));

    let where_expression = cst.where_expression();

    let expression = where_expression.as_ref().and_then(|w| {
        let e = w.expression()?;
        Some(lower_expression(state, &e))
    });

    let bindings = where_expression
        .as_ref()
        .and_then(|w| {
            let b = w.bindings()?;
            Some(lower_let_binding_statements(state, &b))
        })
        .unwrap_or_default();

    LetBinding::Pattern { pattern, expression, bindings }
}

fn lower_equation_like(
    state: &mut State,
    equation: &mut LoweredEquation,
    function_binders: Option<cst::FunctionBinders>,
    guarded_expression: Option<cst::GuardedExpression>,
) {
    equation.binders = function_binders.map_or(vec![], |b| {
        let children = b.children();
        children.map(|b| lower_binder(state, &b)).collect()
    });
    equation.guarded = guarded_expression.as_ref().map(|g| lower_guarded_expression(state, &g));
}

fn lower_guarded_expression(state: &mut State, cst: &cst::GuardedExpression) -> GuardedExpression {
    match cst {
        cst::GuardedExpression::Unconditional(u) => {
            let where_expression = u.where_expression().map(|w| lower_where_expression(state, &w));
            GuardedExpression::Unconditional { where_expression }
        }
        cst::GuardedExpression::Conditionals(c) => {
            let pattern_guarded = c.children().map(|p| lower_pattern_guarded(state, &p)).collect();
            GuardedExpression::Conditionals { pattern_guarded }
        }
    }
}

fn lower_where_expression(state: &mut State, cst: &cst::WhereExpression) -> WhereExpression {
    let expression = cst.expression().map(|e| lower_expression(state, &e));
    let bindings = cst.bindings().map_or(vec![], |b| lower_let_binding_statements(state, &b));
    WhereExpression { expression, bindings }
}

fn lower_pattern_guarded(state: &mut State, cst: &cst::PatternGuarded) -> PatternGuarded {
    let pattern_guards = cst.children().map(|p| lower_pattern_guard(state, &p)).collect();
    let where_expression = cst.where_expression().map(|w| lower_where_expression(state, &w));
    PatternGuarded { pattern_guards, where_expression }
}

fn lower_pattern_guard(state: &mut State, cst: &cst::PatternGuard) -> PatternGuard {
    match cst {
        cst::PatternGuard::PatternGuardBinder(b) => {
            let binder = b.binder().map(|b| lower_binder(state, &b));
            let expression = b.expression().map(|e| lower_expression(state, &e));
            PatternGuard { binder, expression }
        }
        cst::PatternGuard::PatternGuardExpression(e) => {
            let binder = None;
            let expression = e.expression().map(|e| lower_expression(state, &e));
            PatternGuard { binder, expression }
        }
    }
}

fn lower_do_statement(state: &mut State, cst: &cst::DoStatement) -> DoStatement {
    match cst {
        cst::DoStatement::DoStatementBind(b) => {
            let binder = b.binder().map(|b| lower_binder(state, &b));
            let expression = b.expression().map(|e| lower_expression(state, &e));
            DoStatement::Bind { binder, expression }
        }
        cst::DoStatement::DoStatementLet(l) => {
            let statements =
                l.statements().map_or(vec![], |l| lower_let_binding_statements(state, &l));
            DoStatement::Let { statements }
        }
        cst::DoStatement::DoStatementDiscard(d) => {
            let expression = d.expression().map(|e| lower_expression(state, &e));
            DoStatement::Discard { expression }
        }
    }
}

fn lower_qualified_name(
    cst: &cst::QualifiedName,
    token: impl Fn(&cst::QualifiedName) -> Option<SyntaxToken>,
) -> (Option<SmolStr>, Option<SmolStr>) {
    let qualifier = cst.qualifier().and_then(|q| {
        let q = q.text()?;
        let text = q.text();
        Some(SmolStr::from(text))
    });
    let name = token(cst).and_then(|t| {
        let text = t.text();
        Some(SmolStr::from(text))
    });
    (qualifier, name)
}
