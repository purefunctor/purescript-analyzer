//! The core AST used for semantic analysis.

use std::sync::Arc;

use la_arena::{Arena, Idx};
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    names::{Name, NameRef, Qualified},
};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ValueDeclarationData {
    pub expr_arena: Arena<Expr>,
    pub binder_arena: Arena<Binder>,
    pub binders: Box<[BinderId]>,
    pub binding: Binding,
    /// The type annotation for this value declaration, if it exists.
    pub annotation: Option<AstId<ast::AnnotationDeclaration>>,
    /// PureScript supports "equational" style declarations.
    ///
    /// ```haskell
    /// isZero :: Int ->
    /// isZero 0 = true
    /// isZero _ = false
    /// ```
    pub siblings: Arc<[InFile<AstId<ast::ValueDeclaration>>]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    Unconditional { where_expr: WhereExpr },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereExpr {
    pub expr_id: Idx<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binder {
    Constructor { name: Qualified<NameRef>, fields: Box<[BinderId]> },
    Parenthesized(BinderId),
    Variable(Name),
    Wildcard,
}

pub type BinderId = Idx<Binder>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Literal(Literal),
    Variable(Qualified<NameRef>),
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Array(Box<[ExprId]>),
    Record(Box<[RecordItem<ExprId>]>),
    Int(usize),
    Number(SmolStr),
    String(SmolStr),
    Char(SmolStr),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordItem<I> {
    RecordPun(SmolStr),
    RecordField(SmolStr, I),
}
