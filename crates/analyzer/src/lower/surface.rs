//! The core AST used for semantic analysis.

use std::sync::Arc;

use la_arena::{Arena, Idx};
use syntax::ast;

use crate::id::{AstId, InFile};

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
    Unconditional { expr_id: Idx<Expr> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(Lit),
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
    Int(usize),
}
