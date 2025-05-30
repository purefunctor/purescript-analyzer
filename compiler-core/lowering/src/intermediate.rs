use std::sync::Arc;

use crate::{source::*, DeferredResolutionId, TermResolution, TypeVariableResolution};
use indexing::{TermItemId, TypeItemId};
use smol_str::SmolStr;

#[derive(Debug, PartialEq, Eq)]
pub enum BinderRecordItem {
    RecordField { name: Option<SmolStr>, value: Option<BinderId> },
    RecordPun { name: Option<SmolStr> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinderKind {
    Typed { binder: Option<BinderId>, r#type: Option<TypeId> },
    OperatorChain { head: Option<BinderId>, tail: Arc<[OperatorPair<BinderId>]> },
    Integer,
    Number,
    Constructor { resolution: DeferredResolutionId, arguments: Arc<[BinderId]> },
    Variable { variable: Option<SmolStr> },
    Named { named: Option<SmolStr>, binder: Option<BinderId> },
    Wildcard,
    String,
    Char,
    Boolean { boolean: bool },
    Array { array: Arc<[BinderId]> },
    Record { record: Arc<[BinderRecordItem]> },
    Parenthesized { parenthesized: Option<BinderId> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionArgument {
    Type(Option<TypeId>),
    Term(Option<ExpressionId>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum RecordUpdate {
    Leaf { name: Option<SmolStr>, expression: Option<ExpressionId> },
    Branch { name: Option<SmolStr>, updates: Arc<[RecordUpdate]> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CaseBranch {
    pub binders: Arc<[BinderId]>,
    pub guarded_expression: Option<GuardedExpression>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum DoStatement {
    Bind { binder: Option<BinderId>, expression: Option<ExpressionId> },
    Let { statements: Arc<[LetBinding]> },
    Discard { expression: Option<ExpressionId> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionRecordItem {
    RecordField { name: Option<SmolStr>, value: Option<ExpressionId> },
    RecordPun { name: Option<SmolStr>, resolution: Option<TermResolution> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    Typed {
        expression: Option<ExpressionId>,
        r#type: Option<TypeId>,
    },
    OperatorChain {
        head: Option<ExpressionId>,
        tail: Arc<[OperatorPair<ExpressionId>]>,
    },
    InfixChain {
        head: Option<ExpressionId>,
        tail: Arc<[InfixPair<ExpressionId>]>,
    },
    Negate {
        expression: Option<ExpressionId>,
    },
    Application {
        function: Option<ExpressionId>,
        arguments: Arc<[ExpressionArgument]>,
    },
    IfThenElse {
        r#if: Option<ExpressionId>,
        then: Option<ExpressionId>,
        r#else: Option<ExpressionId>,
    },
    LetIn {
        bindings: Arc<[LetBinding]>,
        expression: Option<ExpressionId>,
    },
    Lambda {
        binders: Arc<[BinderId]>,
        expression: Option<ExpressionId>,
    },
    CaseOf {
        trunk: Arc<[ExpressionId]>,
        branches: Arc<[CaseBranch]>,
    },
    Do {
        bind: Option<TermResolution>,
        discard: Option<TermResolution>,
        statements: Arc<[DoStatement]>,
    },
    Ado {
        map: Option<TermResolution>,
        apply: Option<TermResolution>,
        statements: Arc<[DoStatement]>,
        expression: Option<ExpressionId>,
    },
    Constructor {
        resolution: DeferredResolutionId,
    },
    Variable {
        resolution: Option<TermResolution>,
    },
    OperatorName {
        resolution: DeferredResolutionId,
    },
    Section,
    Hole,
    String,
    Char,
    Boolean {
        boolean: bool,
    },
    Integer,
    Number,
    Array {
        array: Arc<[ExpressionId]>,
    },
    Record {
        record: Arc<[ExpressionRecordItem]>,
    },
    Parenthesized {
        parenthesized: Option<ExpressionId>,
    },
    RecordAccess {
        record: Option<ExpressionId>,
        labels: Option<Arc<[SmolStr]>>,
    },
    RecordUpdate {
        updates: Arc<[RecordUpdate]>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeVariableBinding {
    pub visible: bool,
    pub id: TypeVariableBindingId,
    pub name: Option<SmolStr>,
    pub kind: Option<TypeId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeRowItem {
    pub name: Option<SmolStr>,
    pub r#type: Option<TypeId>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeKind {
    ApplicationChain { function: Option<TypeId>, arguments: Arc<[TypeId]> },
    Arrow { argument: Option<TypeId>, result: Option<TypeId> },
    Constrained { constraint: Option<TypeId>, constrained: Option<TypeId> },
    Constructor { resolution: DeferredResolutionId },
    Forall { bindings: Arc<[TypeVariableBinding]>, r#type: Option<TypeId> },
    Hole,
    Integer,
    Kinded { r#type: Option<TypeId>, kind: Option<TypeId> },
    Operator { resolution: DeferredResolutionId },
    OperatorChain { head: Option<TypeId>, tail: Arc<[OperatorPair<TypeId>]> },
    String,
    Variable { name: Option<SmolStr>, resolution: Option<TypeVariableResolution> },
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
    Conditionals { pattern_guarded: Arc<[PatternGuarded]> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhereExpression {
    pub expression: Option<ExpressionId>,
    pub bindings: Arc<[LetBinding]>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LetBinding {
    Name { signature: Option<TypeId>, equations: Arc<[Equation]> },
    Pattern { binder: Option<BinderId>, where_expression: Option<WhereExpression> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternGuarded {
    pub pattern_guards: Arc<[PatternGuard]>,
    pub where_expression: Option<WhereExpression>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternGuard {
    pub binder: Option<BinderId>,
    pub expression: Option<ExpressionId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OperatorPair<T> {
    pub resolution: DeferredResolutionId,
    pub element: Option<T>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InfixPair<T> {
    pub tick: Option<T>,
    pub element: Option<T>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceMemberGroup {
    pub signature: Option<TypeId>,
    pub equations: Arc<[Equation]>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TermItemIr {
    ClassMember {
        signature: Option<TypeId>,
    },
    Constructor {
        arguments: Arc<[TypeId]>,
    },
    Derive {
        resolution: DeferredResolutionId,
        constraints: Arc<[TypeId]>,
        arguments: Arc<[TypeId]>,
    },
    Foreign {
        signature: Option<TypeId>,
    },
    Instance {
        resolution: DeferredResolutionId,
        constraints: Arc<[TypeId]>,
        arguments: Arc<[TypeId]>,
        members: Arc<[InstanceMemberGroup]>,
    },
    Operator {
        resolution: DeferredResolutionId,
        precedence: Option<u16>,
    },
    ValueGroup {
        signature: Option<TypeId>,
        equations: Arc<[Equation]>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Role {
    Nominal,
    Representational,
    Phantom,
    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataIr {
    pub variables: Arc<[TypeVariableBinding]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct NewtypeIr {
    pub variables: Arc<[TypeVariableBinding]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SynonymIr {
    pub variables: Arc<[TypeVariableBinding]>,
    pub r#type: Option<TypeId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassIr {
    pub constraints: Arc<[TypeId]>,
    pub variables: Arc<[TypeVariableBinding]>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeItemIr {
    DataGroup { signature: Option<TypeId>, data: Option<DataIr>, roles: Arc<[Role]> },
    NewtypeGroup { signature: Option<TypeId>, newtype: Option<NewtypeIr>, roles: Arc<[Role]> },
    SynonymGroup { signature: Option<TypeId>, synonym: Option<SynonymIr> },
    ClassGroup { signature: Option<TypeId>, class: Option<ClassIr> },
    Foreign { signature: Option<TypeId> },
    Operator { resolution: DeferredResolutionId, precedence: Option<u16> },
}

syntax::create_association! {
    pub struct Intermediate {
        binder_kind: BinderId => BinderKind,
        expression_kind: ExpressionId => ExpressionKind,
        type_kind: TypeId => TypeKind,
        term_item: TermItemId => TermItemIr,
        type_item: TypeItemId => TypeItemIr,
    }
}

impl Intermediate {
    pub fn iter_binder(&self) -> impl Iterator<Item = (BinderId, &BinderKind)> {
        self.binder_kind.iter()
    }

    pub fn iter_expression(&self) -> impl Iterator<Item = (ExpressionId, &ExpressionKind)> {
        self.expression_kind.iter()
    }

    pub fn iter_type(&self) -> impl Iterator<Item = (TypeId, &TypeKind)> {
        self.type_kind.iter()
    }
}
