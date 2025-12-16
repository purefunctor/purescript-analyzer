use std::sync::Arc;

use indexing::TypeItemId;
use stabilizing::AstId;
use syntax::cst;

#[derive(Debug, PartialEq, Eq)]
pub enum LoweringError {
    NotInScope(NotInScope),
    RecursiveSynonym(RecursiveSynonym),
}

#[derive(Debug, PartialEq, Eq)]
pub enum NotInScope {
    ExprConstructor { id: AstId<cst::ExpressionConstructor> },
    ExprVariable { id: AstId<cst::ExpressionVariable> },
    ExprOperatorName { id: AstId<cst::ExpressionOperatorName> },
    TypeConstructor { id: AstId<cst::TypeConstructor> },
    TypeVariable { id: AstId<cst::TypeVariable> },
    TypeOperatorName { id: AstId<cst::TypeOperatorName> },
    DoFn { kind: DoFn, id: AstId<cst::ExpressionDo> },
    AdoFn { kind: AdoFn, id: AstId<cst::ExpressionAdo> },
    NegateFn { id: AstId<cst::ExpressionNegate> },
    TermOperator { id: AstId<cst::TermOperator> },
    TypeOperator { id: AstId<cst::TypeOperator> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum DoFn {
    Bind,
    Discard,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AdoFn {
    Map,
    Apply,
}

#[derive(Debug, PartialEq, Eq)]
pub struct RecursiveSynonym {
    pub group: Arc<[TypeItemId]>,
}
