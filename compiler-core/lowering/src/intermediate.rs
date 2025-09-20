//! Types of intermediate representations.
use std::sync::Arc;

use files::FileId;
use indexing::{TermItemId, TypeItemId};
use smol_str::SmolStr;

use crate::{TermVariableResolution, TypeVariableResolution, source::*};

#[derive(Debug, PartialEq, Eq)]
pub enum BinderRecordItem {
    RecordField { name: Option<SmolStr>, value: Option<BinderId> },
    RecordPun { name: Option<SmolStr> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinderKind {
    Typed { binder: Option<BinderId>, type_: Option<TypeId> },
    OperatorChain { head: Option<BinderId>, tail: Arc<[OperatorPair<BinderId>]> },
    Integer,
    Number,
    Constructor { resolution: Option<(FileId, TermItemId)>, arguments: Arc<[BinderId]> },
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
    RecordPun { name: Option<SmolStr>, resolution: Option<TermVariableResolution> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    Typed {
        expression: Option<ExpressionId>,
        type_: Option<TypeId>,
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
        if_: Option<ExpressionId>,
        then: Option<ExpressionId>,
        else_: Option<ExpressionId>,
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
        bind: Option<TermVariableResolution>,
        discard: Option<TermVariableResolution>,
        statements: Arc<[DoStatement]>,
    },
    Ado {
        map: Option<TermVariableResolution>,
        apply: Option<TermVariableResolution>,
        statements: Arc<[DoStatement]>,
        expression: Option<ExpressionId>,
    },
    Constructor {
        resolution: Option<(FileId, TermItemId)>,
    },
    Variable {
        resolution: Option<TermVariableResolution>,
    },
    OperatorName {
        resolution: Option<(FileId, TermItemId)>,
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
    pub type_: Option<TypeId>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeKind {
    ApplicationChain { function: Option<TypeId>, arguments: Arc<[TypeId]> },
    Arrow { argument: Option<TypeId>, result: Option<TypeId> },
    Constrained { constraint: Option<TypeId>, constrained: Option<TypeId> },
    Constructor { resolution: Option<(FileId, TypeItemId)> },
    Forall { bindings: Arc<[TypeVariableBinding]>, type_: Option<TypeId> },
    Hole,
    Integer,
    Kinded { type_: Option<TypeId>, kind: Option<TypeId> },
    Operator { resolution: Option<(FileId, TypeItemId)> },
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

pub trait IsElement: Copy {
    type OperatorId: Copy;
}

impl IsElement for BinderId {
    type OperatorId = TermOperatorId;
}

impl IsElement for ExpressionId {
    type OperatorId = TermOperatorId;
}

impl IsElement for TypeId {
    type OperatorId = TypeOperatorId;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OperatorPair<I: IsElement> {
    pub id: Option<I::OperatorId>,
    pub element: Option<I>,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Associativity {
    /// infix
    None,
    /// infixl
    Left,
    /// infixr
    Right,
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
        constraints: Arc<[TypeId]>,
        arguments: Arc<[TypeId]>,
    },
    Foreign {
        signature: Option<TypeId>,
    },
    Instance {
        constraints: Arc<[TypeId]>,
        arguments: Arc<[TypeId]>,
        members: Arc<[InstanceMemberGroup]>,
    },
    Operator {
        associativity: Option<Associativity>,
        precedence: Option<u8>,
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
    pub type_: Option<TypeId>,
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
    Foreign { signature: Option<TypeId>, roles: Arc<[Role]> },
    Operator { associativity: Option<Associativity>, precedence: Option<u8> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Domain {
    Term,
    Type,
}

syntax::create_association! {
    /// Core structure for intermediate representations.
    ///
    /// This associates stable IDs allocated during [`indexing`] and
    /// [`lowering`] with their intermediate representations. Unlike
    /// traditional lowering designs, we do not need to represent the
    /// full structure of the module as an abstract syntax tree.
    ///
    /// [`lowering`]: crate
    pub struct Intermediate {
        binder_kind: BinderId => BinderKind,
        expression_kind: ExpressionId => ExpressionKind,
        type_kind: TypeId => TypeKind,
        term_item: TermItemId => TermItemIr,
        type_item: TypeItemId => TypeItemIr,
        term_operator: TermOperatorId => (FileId, TermItemId),
        type_operator: TypeOperatorId => (FileId, TypeItemId),
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

    pub fn iter_term_operator(&self) -> impl Iterator<Item = (TermOperatorId, FileId, TermItemId)> {
        self.term_operator.iter().map(|(o_id, (f_id, t_id))| (o_id, *f_id, *t_id))
    }

    pub fn iter_type_operator(&self) -> impl Iterator<Item = (TypeOperatorId, FileId, TypeItemId)> {
        self.type_operator.iter().map(|(o_id, (f_id, t_id))| (o_id, *f_id, *t_id))
    }
}
