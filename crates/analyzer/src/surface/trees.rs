//! The core AST used for semantic analysis.
mod printer;
pub mod visitor;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

pub use printer::PrettyPrinter;

use crate::{
    id::AstId,
    names::{Name, NameRef, Qualified},
};

#[derive(Debug, PartialEq, Eq)]
pub struct WithArena<T> {
    pub expr_arena: Arena<Expr>,
    pub let_name_group_arena: Arena<LetNameGroup>,
    pub binder_arena: Arena<Binder>,
    pub type_arena: Arena<Type>,
    pub value: T,
}

impl<T> WithArena<T> {
    pub fn new(
        expr_arena: Arena<Expr>,
        let_name_group_arena: Arena<LetNameGroup>,
        binder_arena: Arena<Binder>,
        type_arena: Arena<Type>,
        value: T,
    ) -> WithArena<T> {
        WithArena { expr_arena, let_name_group_arena, binder_arena, type_arena, value }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueAnnotation {
    pub ty: TypeId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueEquation {
    pub binders: Box<[BinderId]>,
    pub binding: Binding,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueGroup {
    pub name: SmolStr,
    pub annotation: Option<ValueAnnotation>,
    pub equations: FxHashMap<AstId<ast::ValueEquationDeclaration>, ValueEquation>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Binding {
    Unconditional { where_expr: WhereExpr },
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhereExpr {
    pub expr_id: Idx<Expr>,
    pub let_bindings: Box<[LetBinding]>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Binder {
    Constructor { name: Qualified<NameRef>, fields: Box<[BinderId]> },
    Literal(Literal<BinderId>),
    Negative(IntOrNumber),
    Parenthesized(BinderId),
    Variable(Name),
    Wildcard,
}

pub type BinderId = Idx<Binder>;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Application(ExprId, Box<[ExprId]>),
    Constructor(Qualified<NameRef>),
    Lambda(Box<[BinderId]>, ExprId),
    LetIn(Box<[LetBinding]>, ExprId),
    Literal(Literal<ExprId>),
    Variable(Qualified<NameRef>),
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, PartialEq, Eq)]
pub enum LetBinding {
    NameGroup { id: LetNameGroupId },
    Pattern { binder: BinderId, where_expr: WhereExpr },
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetNameAnnotation {
    pub ty: TypeId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetNameEquation {
    pub binders: Box<[BinderId]>,
    pub binding: Binding,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetNameGroup {
    pub name: Name,
    pub annotation: Option<LetNameAnnotation>,
    pub equations: Vec<LetNameEquation>,
}

pub type LetNameGroupId = Idx<LetNameGroup>;

#[derive(Debug, PartialEq, Eq)]
pub enum Literal<I> {
    Array(Box<[I]>),
    Record(Box<[RecordItem<I>]>),
    Int(usize),
    Number(SmolStr),
    String(SmolStr),
    Char(SmolStr),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Eq)]
pub enum IntOrNumber {
    Int(usize),
    Number(SmolStr),
}

#[derive(Debug, PartialEq, Eq)]
pub enum RecordItem<I> {
    RecordPun(SmolStr),
    RecordField(SmolStr, I),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Arrow(Box<[TypeId]>, TypeId),
    Application(TypeId, Box<[TypeId]>),
    Constructor(Qualified<NameRef>),
    Parenthesized(TypeId),
    Variable(NameRef),
}

pub type TypeId = Idx<Type>;
