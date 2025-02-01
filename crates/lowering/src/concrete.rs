use id::Id;
use indexing::ExprItemId;
use rowan::ast::AstPtr;
use rustc_hash::FxHashMap;
use syntax::cst;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExpressionKind {
    Typed,
    OperatorChain,
    InfixChain,
    Tick,
    Negate,
    ApplicationChain,
    TypeArgument,
    TermArgument,
    IfThenElse,
    LetIn,
    Lambda,
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
    pub expression: Option<ExpressionId>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum LoweredExprItem {
    Value { signature: Option<TypeId>, equations: Vec<LoweredEquation> },
}

#[derive(Debug, Default)]
pub struct LoweringMap {
    pub expr_item: FxHashMap<ExprItemId, LoweredExprItem>,
}
