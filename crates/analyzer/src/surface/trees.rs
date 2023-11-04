//! The core AST used for semantic analysis.
mod printer;
pub mod visitor;

use std::collections::BTreeMap;

use la_arena::{Arena, Idx};
use smol_str::SmolStr;
use syntax::ast;

pub use printer::PrettyPrinter;

use crate::{
    id::{AstId, InFile},
    names::{Name, NameRef, Qualified},
};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct WithArena<T> {
    pub expr_arena: Arena<Expr>,
    pub binder_arena: Arena<Binder>,
    pub type_arena: Arena<Type>,
    pub value: T,
}

impl<T> WithArena<T> {
    pub fn new(
        expr_arena: Arena<Expr>,
        binder_arena: Arena<Binder>,
        type_arena: Arena<Type>,
        value: T,
    ) -> WithArena<T> {
        WithArena { expr_arena, binder_arena, type_arena, value }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SurfaceValueAnnotation {
    pub ty: TypeId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SurfaceValueEquation {
    pub binders: Box<[BinderId]>,
    pub binding: Binding,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SurfaceValueGroup {
    pub name: SmolStr,
    pub annotation: Option<SurfaceValueAnnotation>,
    pub equations: BTreeMap<AstId<ast::ValueEquationDeclaration>, SurfaceValueEquation>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct DataDeclarationData {
    pub type_arena: Arena<Type>,
    pub name: Name,
    pub constructors: BTreeMap<InFile<AstId<ast::DataConstructor>>, DataConstructorData>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct DataConstructorData {
    pub name: Name,
    pub fields: Box<[TypeId]>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ForeignDataDeclarationData {
    pub type_arena: Arena<Type>,
    pub name: Name,
    pub type_id: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    Unconditional { where_expr: WhereExpr },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhereExpr {
    pub expr_id: Idx<Expr>,
    pub let_bindings: Box<[LetBinding]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binder {
    Constructor { name: Qualified<NameRef>, fields: Box<[BinderId]> },
    Literal(Literal<BinderId>),
    Negative(IntOrNumber),
    Parenthesized(BinderId),
    Variable(Name),
    Wildcard,
}

pub type BinderId = Idx<Binder>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Application(ExprId, Box<[ExprId]>),
    Constructor(Qualified<NameRef>),
    Lambda(Box<[BinderId]>, ExprId),
    LetIn(Box<[LetBinding]>, ExprId),
    Literal(Literal<ExprId>),
    Variable(Qualified<NameRef>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LetBinding {
    Name { name: Name, binding: Binding },
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal<I> {
    Array(Box<[I]>),
    Record(Box<[RecordItem<I>]>),
    Int(usize),
    Number(SmolStr),
    String(SmolStr),
    Char(SmolStr),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntOrNumber {
    Int(usize),
    Number(SmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordItem<I> {
    RecordPun(SmolStr),
    RecordField(SmolStr, I),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Arrow(Box<[TypeId]>, TypeId),
    Application(TypeId, Box<[TypeId]>),
    Constructor(Qualified<NameRef>),
    Parenthesized(TypeId),
    Variable(NameRef),
}

pub type TypeId = Idx<Type>;
