//! The core AST used for semantic analysis.
mod printer;
pub mod visitor;

use files::FileId;
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::ast;

pub use printer::PrettyPrinter;

use crate::{
    id::AstId,
    names::{ModuleName, Name, NameRef, Qualified},
};

/// Exports in a module.
#[derive(Debug, PartialEq, Eq)]
pub struct ModuleExports {
    pub items: Vec<ExportItem>,
    /// Is the export list defined in source?
    pub explicit: bool,
}

impl ModuleExports {
    #[inline]
    fn is_exported(&self, predicate: impl Fn(&ExportItem) -> bool) -> bool {
        self.items.iter().any(predicate)
    }

    pub fn is_constructor_exported(
        &self,
        type_name: impl AsRef<str>,
        constructor_name: impl AsRef<str> + Copy,
    ) -> bool {
        self.is_exported(|export_item| match export_item {
            ExportItem::ExportType(i, m) => {
                let is_type = type_name.as_ref() == i.as_ref();
                let is_member =
                    m.as_ref().is_some_and(|data_members| data_members.is_member(constructor_name));
                is_type && is_member
            }
            _ => false,
        })
    }

    pub fn is_value_exported(&self, name: impl AsRef<str>) -> bool {
        self.is_exported(|export_item| match export_item {
            ExportItem::ExportValue(i) => name.as_ref() == i.as_ref(),
            _ => false,
        })
    }
}

/// A list of data constructors.
#[derive(Debug, PartialEq, Eq)]
pub enum DataMembers {
    DataAll,
    DataEnumerated(Vec<NameRef>),
}

impl DataMembers {
    pub fn is_member(&self, constructor_name: impl AsRef<str>) -> bool {
        match self {
            DataMembers::DataAll => true,
            DataMembers::DataEnumerated(constructors) => constructors
                .iter()
                .any(|constructor| constructor_name.as_ref() == constructor.as_ref()),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExportItem {
    ExportType(NameRef, Option<DataMembers>),
    ExportValue(NameRef),
}

/// Imports in a module.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleImports {
    pub(super) inner: Vec<ImportDeclaration>,
}

impl ModuleImports {
    pub fn find_qualified(&self, prefix: &ModuleName) -> Option<&ImportDeclaration> {
        self.inner.iter().find(|import_declaration| {
            if let Some(qualified_as) = &import_declaration.qualified_as {
                qualified_as == prefix
            } else {
                false
            }
        })
    }
}

/// An import in a module.
#[derive(Debug, PartialEq, Eq)]
pub struct ImportDeclaration {
    /// The name of the imported module.
    pub module_name: ModuleName,
    /// The associated [`FileId`], obtained from the [`ModuleMap`].
    ///
    /// [`ModuleMap`]: crate::resolver::ModuleMap
    pub file_id: FileId,
    /// The qualified name of the import.
    ///
    /// If [`None`], then this import is unqualified.
    pub qualified_as: Option<ModuleName>,
    /// The list of imported items.
    ///
    /// If [`None], then this import is open.
    pub import_list: Option<ImportList>,
}

impl ImportDeclaration {
    #[inline]
    fn is_imported(&self, predicate: impl Fn(&ImportItem) -> bool) -> bool {
        if let Some(import_list) = &self.import_list {
            let is_member = import_list.items.iter().any(predicate);
            if import_list.hiding {
                !is_member
            } else {
                is_member
            }
        } else {
            false
        }
    }

    pub fn is_constructor_imported(
        &self,
        type_name: impl AsRef<str>,
        constructor_name: impl AsRef<str> + Copy,
    ) -> bool {
        self.is_imported(|import_item| match import_item {
            ImportItem::ImportType(i, m) => {
                let is_type = type_name.as_ref() == i.as_ref();
                let is_member =
                    m.as_ref().is_some_and(|data_members| data_members.is_member(constructor_name));
                is_type && is_member
            }
            _ => false,
        })
    }

    pub fn is_value_imported(&self, v: impl AsRef<str>) -> bool {
        self.is_imported(|import_item| match import_item {
            ImportItem::ImportValue(i) => v.as_ref() == i.as_ref(),
            _ => false,
        })
    }
}

/// A list of imported items.
#[derive(Debug, PartialEq, Eq)]
pub struct ImportList {
    pub items: Vec<ImportItem>,
    /// Are these items `hidden`?
    pub hiding: bool,
}

/// The kind of the imported item.
#[derive(Debug, PartialEq, Eq)]
pub enum ImportItem {
    ImportType(NameRef, Option<DataMembers>),
    ImportValue(NameRef),
}

#[derive(Debug, PartialEq, Eq)]
pub struct WithArena<T> {
    pub expr_arena: Arena<Expr>,
    pub let_name_arena: Arena<LetName>,
    pub binder_arena: Arena<Binder>,
    pub type_arena: Arena<Type>,
    pub value: T,
}

impl<T> WithArena<T> {
    pub fn new(
        expr_arena: Arena<Expr>,
        let_name_arena: Arena<LetName>,
        binder_arena: Arena<Binder>,
        type_arena: Arena<Type>,
        value: T,
    ) -> WithArena<T> {
        WithArena { expr_arena, let_name_arena, binder_arena, type_arena, value }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeVariable {
    Kinded(SmolStr, TypeId),
    Name(SmolStr),
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataAnnotation {
    pub ty: TypeId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataDeclaration {
    pub constructors: FxHashMap<AstId<ast::DataConstructor>, DataConstructor>,
    pub variables: Vec<TypeVariable>,
}

impl DataDeclaration {
    pub fn get_constructor(&self, id: AstId<ast::DataConstructor>) -> &DataConstructor {
        self.constructors.get(&id).unwrap()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataConstructor {
    pub name: Name,
    pub fields: Vec<TypeId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataGroup {
    pub name: Name,
    pub annotation: Option<DataAnnotation>,
    pub declaration: DataDeclaration,
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
    pub name: Name,
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
    Name { id: LetNameId },
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
pub struct LetName {
    pub name: Name,
    pub annotation: Option<LetNameAnnotation>,
    pub equations: Vec<LetNameEquation>,
}

pub type LetNameId = Idx<LetName>;

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
