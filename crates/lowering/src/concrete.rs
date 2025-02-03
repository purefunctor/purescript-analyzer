use id::Id;
use indexing::ExprItemId;
use rowan::ast::AstPtr;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::cst;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct OperatorPair<T> {
    pub qualifier: Option<SmolStr>,
    pub operator: Option<SmolStr>,
    pub element: Option<T>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TickPair<T> {
    pub tick: Option<T>,
    pub element: Option<T>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExpressionArgument {
    Expression(Option<ExpressionId>),
    Type(Option<TypeId>),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum LetBindingKind {
    Value {
        name: Option<SmolStr>,
        signature: Option<TypeId>,
        equations: Vec<LoweredEquation>,
    },
    Pattern {
        pattern: Option<BinderId>,
        expression: Option<ExpressionId>,
        bindings: Vec<LetBindingId>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct WhereExpression {
    pub expression: Option<ExpressionId>,
    pub bindings: Vec<LetBindingId>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum GuardedExpressionKind {
    Unconditional { where_expression: Option<WhereExpression> },
    Conditionals { pattern_guarded: Vec<PatternGuarded> },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatternGuarded {
    pub pattern_guards: Vec<PatternGuard>,
    pub where_expression: Option<WhereExpression>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatternGuard {
    pub binder: Option<BinderId>,
    pub expression: Option<ExpressionId>,
}

/// A stable ID for lowered let bindings.  
///
/// See comments in [`crate::sourcemap::SourceMap`]
pub type LetBindingKindId = Id<LetBindingKind>;

/// A stable ID for an individual let binding.
pub type LetBindingId = Id<cst::LetBinding>;

pub type LetBindingPtr = AstPtr<cst::LetBinding>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExpressionKind {
    Typed {
        expression: Option<ExpressionId>,
        signature: Option<TypeId>,
    },
    OperatorChain {
        head: Option<ExpressionId>,
        tail: Vec<OperatorPair<ExpressionId>>,
    },
    InfixChain {
        head: Option<ExpressionId>,
        tail: Vec<TickPair<ExpressionId>>,
    },
    Negate {
        expression: Option<ExpressionId>,
    },
    ApplicationChain {
        head: Option<ExpressionId>,
        tail: Vec<ExpressionArgument>,
    },
    IfThenElse {
        r#if: Option<ExpressionId>,
        then: Option<ExpressionId>,
        r#else: Option<ExpressionId>,
    },
    LetIn {
        bindings: Vec<LetBindingId>,
        expression: Option<ExpressionId>,
    },
    Lambda {
        binders: Vec<BinderId>,
        expression: Option<ExpressionId>,
    },
    CaseOf,
    Do,
    Ado,
    Constructor,
    Variable,
    OperatorName,
    Section,
    Hole,
    String,
    Char,
    True,
    False,
    Integer,
    Number,
    Array,
    Record,
    Parenthesized,
    RecordAccess,
    RecordUpdate,
}

pub type ExpressionId = Id<cst::Expression>;
pub type ExpressionPtr = AstPtr<cst::Expression>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub id: ExpressionId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeKind {
    ApplicationChain,
    Arrow,
    Constrained,
    Constructor,
    Forall,
    Hole,
    Integer,
    Kinded,
    Operator,
    OperatorChain,
    String,
    Variable,
    VariableBinding,
    Wildcard,
    Record,
    Row,
    Parenthesized,
}

pub type TypeId = Id<cst::Type>;
pub type TypePtr = AstPtr<cst::Type>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    pub id: TypeId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BinderKind {
    Typed,
    OperatorChain,
    Integer,
    Number,
    Constructor,
    Variable,
    Named,
    Wildcard,
    String,
    Char,
    True,
    False,
    Array,
    Record,
    Parenthesized,
}

pub type BinderId = Id<cst::Binder>;
pub type BinderPtr = AstPtr<cst::Binder>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Binder {
    pub kind: BinderKind,
    pub id: BinderId,
}

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub struct LoweredEquation {
    pub binders: Vec<BinderId>,
    pub guarded: Option<GuardedExpressionKind>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum LoweredExprItem {
    Value { signature: Option<TypeId>, equations: Vec<LoweredEquation> },
}

#[derive(Debug, Default)]
pub struct LoweringMap {
    pub expr_item: FxHashMap<ExprItemId, LoweredExprItem>,
}
