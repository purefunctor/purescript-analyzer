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
pub enum LetBinding {
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

/// A stable ID for lowered let bindings.  
///
/// See comments in [`crate::sourcemap::SourceMap`]
pub type LetBindingKindId = Id<LetBinding>;

/// A stable ID for an individual let binding.
pub type LetBindingId = Id<cst::LetBinding>;

pub type LetBindingPtr = AstPtr<cst::LetBinding>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct WhereExpression {
    pub expression: Option<ExpressionId>,
    pub bindings: Vec<LetBindingId>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum GuardedExpression {
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct CaseBranch {
    pub binders: Vec<BinderId>,
    pub guarded_expression: Option<GuardedExpression>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum DoStatement {
    Bind { binder: Option<BinderId>, expression: Option<ExpressionId> },
    Let { statements: Vec<LetBindingId> },
    Discard { expression: Option<ExpressionId> },
}

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
    CaseOf {
        trunk: Vec<ExpressionId>,
        branches: Vec<CaseBranch>,
    },
    Do {
        qualifier: Option<SmolStr>,
        statements: Vec<DoStatement>,
    },
    Ado {
        qualifier: Option<SmolStr>,
        statements: Vec<DoStatement>,
        expression: Option<ExpressionId>,
    },
    Constructor {
        qualifier: Option<SmolStr>,
        name: Option<SmolStr>,
    },
    Variable {
        qualifier: Option<SmolStr>,
        name: Option<SmolStr>,
    },
    OperatorName {
        qualifier: Option<SmolStr>,
        name: Option<SmolStr>,
    },
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
    Parenthesized {
        expression: Option<ExpressionId>,
    },
    RecordAccess {
        expression: Option<ExpressionId>,
        labels: Option<SmolStr>,
    },
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
    pub guarded: Option<GuardedExpression>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum LoweredExprItem {
    Value { signature: Option<TypeId>, equations: Vec<LoweredEquation> },
}

#[derive(Debug, Default)]
pub struct LoweringMap {
    pub expr_item: FxHashMap<ExprItemId, LoweredExprItem>,
}
