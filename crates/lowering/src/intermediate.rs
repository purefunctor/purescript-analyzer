use std::sync::Arc;

use crate::{source::*, RootResolutionId};
use indexing::source::*;
use smol_str::SmolStr;

#[derive(Debug, PartialEq, Eq)]
pub enum BinderKind {
    Typed { binder: Option<BinderId>, r#type: Option<TypeId> },
    OperatorChain { head: Option<BinderId>, tail: Arc<[OperatorPair<BinderId>]> },
    Integer,
    Number,
    Constructor { resolution: RootResolutionId, arguments: Vec<BinderId> },
    Variable { variable: Option<SmolStr> },
    Named { named: Option<SmolStr>, binder: Option<BinderId> },
    Wildcard,
    String,
    Char,
    Boolean { boolean: bool },
    Array { array: Arc<[BinderId]> },
    Record { record: Arc<[RecordItem<BinderId>]> },
    Parenthesized { parenthesized: Option<BinderId> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    Parenthesized { parenthesized: Option<ExpressionId> },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeVariableBinding {
    pub visible: bool,
    pub name: Option<SmolStr>,
    pub kind: Option<TypeId>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeRowItem {
    pub name: Option<SmolStr>,
    pub r#type: Option<TypeId>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeKind {
    ApplicationChain { function: Option<TypeId>, arguments: Arc<[TypeId]> },
    Arrow { argument: Option<TypeId>, result: Option<TypeId> },
    Constrained { constraint: Option<TypeId>, constrained: Option<TypeId> },
    Constructor { resolution: RootResolutionId },
    Forall { bindings: Arc<[TypeVariableBinding]>, r#type: Option<TypeId> },
    Hole,
    Integer,
    Kinded { r#type: Option<TypeId>, kind: Option<TypeId> },
    Operator { resolution: RootResolutionId },
    OperatorChain { head: Option<TypeId>, tail: Arc<[OperatorPair<TypeId>]> },
    String,
    Variable { resolution: Option<TypeVariableBindingId> },
    Wildcard,
    Record { items: Arc<[TypeRowItem]>, tail: Option<TypeId> },
    Row { items: Arc<[TypeRowItem]>, tail: Option<TypeId> },
    Parenthesized { parenthesized: Option<TypeId> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Equation {
    pub binders: Arc<[BinderId]>,
    pub guarded: Option<GuardedExpression>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum GuardedExpression {
    Unconditional { where_expression: Option<WhereExpression> },
    Conditionals { pattern_guarded: Vec<PatternGuarded> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhereExpression {
    pub expression: Option<ExpressionId>,
    pub bindings: Vec<LetBindingId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternGuarded {
    pub pattern_guards: Vec<PatternGuard>,
    pub where_expression: Option<WhereExpression>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternGuard {
    pub binder: Option<BinderId>,
    pub expression: Option<ExpressionId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OperatorPair<T> {
    pub resolution: RootResolutionId,
    pub element: Option<T>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum RecordItem<T> {
    RecordField { name: Option<SmolStr>, value: Option<T> },
    RecordPun { name: Option<SmolStr> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueEquation {
    pub signature: Option<TypeId>,
    pub equations: Arc<[Equation]>,
}

syntax::create_association! {
    pub struct Intermediate {
        binder_kind: BinderId => BinderKind,
        expression_kind: ExpressionId => ExpressionKind,
        type_kind: TypeId => TypeKind,
        value_signature: ValueSignatureId => TypeId,
        value_equation: ValueEquationId => Equation,
    }
}
