//! A high-level AST for semantic information.
//!
//! The types defined in this file roughly follows the high-level AST for the
//! PureScript programming language. Syntax sugar is computed much later once
//! more information becomes available.

use std::{borrow::Borrow, ops::Index, sync::Arc};

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use syntax::ast;

use crate::id::AstId;

// ===== SECTION: Names ====== //

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleName(Arc<str>);

impl ModuleName {
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.0.split('.')
    }

    pub fn from_raw(inner: Arc<str>) -> ModuleName {
        ModuleName(inner)
    }
}

impl AsRef<str> for ModuleName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for ModuleName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for &ModuleName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name(Arc<str>);

impl Name {
    pub fn from_raw(inner: Arc<str>) -> Name {
        Name(inner)
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for Name {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for &Name {
    fn borrow(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Qualified<T> {
    pub prefix: Option<ModuleName>,
    pub value: T,
}

// ===== SECTION: Module ====== //

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub header: ModuleHeader,
    pub imports: ModuleImports,
    pub body: ModuleBody,
}

// ===== SECTION: ModuleHeader ====== //

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleHeader {
    pub name: ModuleName,
    pub export_list: ExportList,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExportList {
    pub items: Vec<ExportItem>,
    pub explicit: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExportItem {
    ExportType(Name, Option<DataMembers>),
    ExportValue(Name),
}

// ===== SECTION: ModuleImports ====== //

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleImports {
    pub declarations: Vec<ImportDeclaration>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportDeclaration {
    pub name: ModuleName,
    pub qualified_as: Option<ModuleName>,
    pub import_list: Option<ImportList>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportList {
    pub items: Vec<ImportItem>,
    pub hiding: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ImportItem {
    ImportType(Name, Option<DataMembers>),
    ImportValue(Name),
}

#[derive(Debug, PartialEq, Eq)]
pub enum DataMembers {
    DataAll,
    DataEnumerated(Vec<Name>),
}

// ===== SECTION: ModuleBody ====== //

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleBody {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    DataDeclaration(DataDeclaration),
    ValueDeclaration(ValueDeclaration),
}

// ===== SECTION: DataDeclaration ====== //

#[derive(Debug, PartialEq, Eq)]
pub struct DataDeclaration {
    pub name: Name,
    pub annotation: Option<TypeId>,
    pub variables: Vec<TypeVariable>,
    pub constructors: FxHashMap<AstId<ast::DataConstructor>, DataConstructor>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataConstructor {
    pub name: Name,
    pub fields: Vec<TypeId>,
}

// ===== SECTION: ValueDeclaration ====== //

#[derive(Debug, PartialEq, Eq)]
pub struct ValueDeclaration {
    pub name: Name,
    pub annotation: Option<TypeId>,
    pub equations: Vec<ValueEquation>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ValueEquation {
    pub binders: Vec<BinderId>,
    pub binding: Binding,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Binding {
    Unconditional { where_expr: WhereExpr },
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhereExpr {
    pub expr_id: ExprId,
    pub let_bindings: Vec<LetBinding>,
}

// ===== SECTION: Common ====== //

#[derive(Debug, PartialEq, Eq)]
pub enum Literal<I> {
    Array(Vec<I>),
    Record(Vec<RecordItem<I>>),
    Int(usize),
    Number(Arc<str>),
    String(Arc<str>),
    Char(Arc<str>),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Eq)]
pub enum IntOrNumber {
    Int(usize),
    Number(Arc<str>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum RecordItem<I> {
    RecordPun(Arc<str>),
    RecordField(Arc<str>, I),
}

// ===== SECTION: Expr ====== //

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Application(ExprId, Vec<ExprId>),
    Constructor(Qualified<Name>),
    Lambda(Vec<BinderId>, ExprId),
    LetIn(Vec<LetBinding>, ExprId),
    Literal(Literal<ExprId>),
    Variable(Qualified<Name>),
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, PartialEq, Eq)]
pub enum LetBinding {
    Name { id: LetNameId },
    Pattern { binder: BinderId, where_expr: WhereExpr },
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetName {
    pub name: Name,
    pub annotation: Option<LetNameAnnotation>,
    pub equations: Vec<LetNameEquation>,
}

pub type LetNameId = Idx<LetName>;

#[derive(Debug, PartialEq, Eq)]
pub struct LetNameAnnotation {
    pub ty: TypeId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetNameEquation {
    pub binders: Vec<BinderId>,
    pub binding: Binding,
}

// ===== SECTION: Binder ====== //

#[derive(Debug, PartialEq, Eq)]
pub enum Binder {
    Constructor { name: Qualified<Name>, fields: Vec<BinderId> },
    Literal(Literal<BinderId>),
    Negative(IntOrNumber),
    Parenthesized(BinderId),
    Variable(Name),
    Wildcard,
}

pub type BinderId = Idx<Binder>;

// ===== SECTION: Type ====== //

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Arrow(Vec<TypeId>, TypeId),
    Application(TypeId, Vec<TypeId>),
    Constructor(Qualified<Name>),
    Parenthesized(TypeId),
    Variable(Name),
}

pub type TypeId = Idx<Type>;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeVariable {
    Kinded(Name, TypeId),
    Name(Name),
}

// ===== SECTION: SurfaceArena ====== //

#[derive(Debug, Default, PartialEq, Eq)]
pub struct SurfaceArena {
    expr: Arena<Expr>,
    let_name: Arena<LetName>,
    binder: Arena<Binder>,
    ty: Arena<Type>,
}

impl Index<ExprId> for SurfaceArena {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.expr[index]
    }
}

impl Index<LetNameId> for SurfaceArena {
    type Output = LetName;

    fn index(&self, index: LetNameId) -> &Self::Output {
        &self.let_name[index]
    }
}

impl Index<BinderId> for SurfaceArena {
    type Output = Binder;

    fn index(&self, index: BinderId) -> &Self::Output {
        &self.binder[index]
    }
}

impl Index<TypeId> for SurfaceArena {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.ty[index]
    }
}
