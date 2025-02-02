use std::iter;

use indexing::{ExprItem, ExprItemId, IndexingResult, ValueGroupId};
use rowan::ast::AstNode;
use smol_str::SmolStr;
use syntax::cst;

use crate::{
    BinderId, BinderKind, ExpressionId, ExpressionKind, LoweredEquation, LoweredExprItem,
    LoweringMap, LoweringResult, OperatorPair, SourceMap, TickPair, TypeId, TypeKind,
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

        if let Some(f) = e.function_binders() {
            for b in f.children() {
                equation.binders.push(lower_binder(state, &b));
            }
        }

        let Some(g) = e.guarded_expression() else { continue };
        let Some(w) = g.where_expression() else { continue };
        let Some(e) = w.expression() else { continue };
        equation.expression = Some(lower_expression(state, &e));
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
        cst::Expression::ExpressionApplicationChain(_a) => ExpressionKind::ApplicationChain,
        cst::Expression::ExpressionTypeArgument(_t) => ExpressionKind::TypeArgument,
        cst::Expression::ExpressionTermArgument(_t) => ExpressionKind::TermArgument,
        cst::Expression::ExpressionIfThenElse(_i) => ExpressionKind::IfThenElse,
        cst::Expression::ExpressionLetIn(_l) => ExpressionKind::LetIn,
        cst::Expression::ExpressionLambda(_l) => ExpressionKind::Lambda,
        cst::Expression::ExpressionCaseOf(_c) => ExpressionKind::CaseOf,
        cst::Expression::ExpressionDo(_d) => ExpressionKind::Do,
        cst::Expression::ExpressionAdo(_a) => ExpressionKind::Ado,
        cst::Expression::ExpressionConstructor(_c) => ExpressionKind::Constructor,
        cst::Expression::ExpressionVariable(_v) => ExpressionKind::Variable,
        cst::Expression::ExpressionOperatorName(_o) => ExpressionKind::OperatorName,
        cst::Expression::ExpressionSection(_s) => ExpressionKind::Section,
        cst::Expression::ExpressionHole(_h) => ExpressionKind::Hole,
        cst::Expression::ExpressionString(_s) => ExpressionKind::String,
        cst::Expression::ExpressionChar(_c) => ExpressionKind::Char,
        cst::Expression::ExpressionTrue(_t) => ExpressionKind::True,
        cst::Expression::ExpressionFalse(_f) => ExpressionKind::False,
        cst::Expression::ExpressionInteger(_i) => ExpressionKind::Integer,
        cst::Expression::ExpressionNumber(_n) => ExpressionKind::Number,
        cst::Expression::ExpressionArray(_a) => ExpressionKind::Array,
        cst::Expression::ExpressionRecord(_r) => ExpressionKind::Record,
        cst::Expression::ExpressionParenthesized(_p) => ExpressionKind::Parenthesized,
        cst::Expression::ExpressionRecordAccess(_r) => ExpressionKind::RecordAccess,
        cst::Expression::ExpressionRecordUpdate(_r) => ExpressionKind::RecordUpdate,
    };
    state.source_map.insert_expression(cst, kind)
}
