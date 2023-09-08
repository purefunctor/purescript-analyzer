//! The core AST used for semantic analysis.

use std::sync::Arc;

use la_arena::{Arena, Idx};
use smol_str::SmolStr;
use syntax::ast;

use crate::{
    id::{AstId, InFile},
    names::{NameRef, Qualified},
};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ValueDeclarationData {
    pub expr_arena: Arena<Expr>,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Binding {
    Unconditional { where_expr: WhereExpr },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct WhereExpr {
    pub expr_id: Idx<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(Lit),
    Var(Qualified<NameRef>),
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
    Array(Box<[ExprId]>),
    Record(Box<[RecordItem]>),
    Int(usize),
    Number(SmolStr),
    String(SmolStr),
    Char(SmolStr),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordItem {
    RecordPun(SmolStr),
    RecordField(SmolStr, ExprId),
}
