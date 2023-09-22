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
    pub binders: Box<[Binder]>,
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
    Variable(Name),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    LetIn { let_bindings: Box<[LetBinding]>, in_expr_id: ExprId },
    Lit(Lit),
    Var(Qualified<NameRef>),
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LetBinding {
    Name { name: Name, binding: Binding },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
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
