use indexing::{ExprItem, IndexingResult, ValueGroupId};
use rowan::ast::AstNode;
use syntax::cst;

use super::{BinderId, BinderKind, ExpressionId, ExpressionKind, SourceMap, TypeId, TypeKind};

#[derive(Default)]
struct State {
    source_map: SourceMap,
}

pub(super) fn lower_module(module: &cst::Module, index: &IndexingResult) {
    let mut state = State::default();

    for (_, _, item) in index.nominal.iter_expr() {
        lower_expr_item(&mut state, module, index, item);
    }
}

fn lower_expr_item(
    state: &mut State,
    module: &cst::Module,
    index: &IndexingResult,
    item: &ExprItem,
) {
    match item {
        ExprItem::Constructor(_) => (),
        ExprItem::Instance(_) => (),
        ExprItem::Derive(_) => (),
        ExprItem::ClassMember(_) => (),
        ExprItem::Value(v) => {
            lower_value_group(state, module, index, v);
        }
        ExprItem::Foreign(_) => (),
        ExprItem::Operator(_) => (),
    }
}

fn lower_value_group(
    state: &mut State,
    module: &cst::Module,
    index: &IndexingResult,
    group: &ValueGroupId,
) {
    let root = module.syntax();

    let mut signature_id = None;
    'signature: {
        let Some(signature) = group.signature else { break 'signature };
        let Some(signature) = index.source_map.declaration_ptr(signature) else { break 'signature };
        let Some(signature) = signature.try_to_node(root) else { break 'signature };
        let cst::Declaration::ValueSignature(signature) = signature else { break 'signature };
        let Some(signature) = signature.signature() else { break 'signature };
        signature_id = Some(lower_type(state, &signature));
    };

    let capacity = group.equations.len();
    let mut equation_id = Vec::with_capacity(capacity);
    for equation in &group.equations {
        let Some(equation) = index.source_map.declaration_ptr(*equation) else { continue };
        let Some(equation) = equation.try_to_node(root) else { continue };
        let cst::Declaration::ValueEquation(equation) = equation else { continue };
        
        dbg!(&equation);

        let Some(guarded_expression) = equation.guarded_expression() else { continue };
        let Some(where_expression) = guarded_expression.where_expression() else { continue };
        let Some(expression) = where_expression.expression() else { continue };
        equation_id.push(lower_expression(state, &expression));
    }

    dbg!((signature_id, equation_id));
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
        cst::Expression::ExpressionTyped(_t) => ExpressionKind::Typed,
        cst::Expression::ExpressionOperatorChain(_o) => ExpressionKind::OperatorChain,
        cst::Expression::ExpressionInfixChain(_i) => ExpressionKind::InfixChain,
        cst::Expression::ExpressionTick(_t) => ExpressionKind::Tick,
        cst::Expression::ExpressionNegate(_n) => ExpressionKind::Negate,
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
