//! A high-level AST for semantic information.
//!
//! The types defined in this file roughly follows the high-level AST for the
//! PureScript programming language. Syntax sugar is computed much later once
//! more information becomes available.

use std::{borrow::Borrow, ops::Index, sync::Arc};

use analyzer_index::{nominal::*, AstId};
use la_arena::{Arena, Idx};
use paste::paste;
use rustc_hash::FxHashMap;
use syntax::{ast, SyntaxNodePtr};

// region: Names

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

// endregion

// region: Module

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub header: ModuleHeader,
    pub imports: ModuleImports,
    pub body: ModuleBody,
}

// endregion

// region: ModuleHeader

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleHeader {
    pub name: ModuleName,
    pub export_list: Option<ExportList>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExportList {
    pub items: Vec<ExportItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExportItem {
    ExportClass(Name),
    ExportType(Name, Option<DataMembers>),
    ExportValue(Name),
}

// endregion

// region: ModuleImports

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
    ImportClass(Name),
    ImportType(Name, Option<DataMembers>),
    ImportValue(Name),
}

#[derive(Debug, PartialEq, Eq)]
pub enum DataMembers {
    DataAll,
    DataEnumerated(Vec<Name>),
}

// endregion

// region: ModuleBody

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ModuleBody {
    pub declarations: Vec<Declaration>,
    pub class_declarations: FxHashMap<ClassGroupId, usize>,
    pub data_declarations: FxHashMap<DataGroupId, usize>,
    pub value_declarations: FxHashMap<ValueGroupId, usize>,
}

impl ModuleBody {
    pub fn class_declaration(&self, class_id: ClassGroupId) -> Option<&ClassDeclaration> {
        self.class_declarations.get(&class_id).map(|index| {
            let declaration = &self.declarations[*index];
            if let Declaration::ClassDeclaration(class_declaration) = declaration {
                class_declaration
            } else {
                unreachable!("impossible: index does not point to a class declaration");
            }
        })
    }

    pub fn data_declaration(&self, data_id: DataGroupId) -> Option<&DataDeclaration> {
        self.data_declarations.get(&data_id).map(|index| {
            let declaration = &self.declarations[*index];
            if let Declaration::DataDeclaration(data_declaration) = declaration {
                data_declaration
            } else {
                unreachable!("impossible: index does not point to a data declaration");
            }
        })
    }

    pub fn value_declaration(&self, value_id: ValueGroupId) -> Option<&ValueDeclaration> {
        self.value_declarations.get(&value_id).map(|index| {
            let declaration = &self.declarations[*index];
            if let Declaration::ValueDeclaration(value_declaration) = declaration {
                value_declaration
            } else {
                unreachable!("impossible: index does not point to a value declaration");
            }
        })
    }

    pub fn iter_class_declarations(&self) -> impl Iterator<Item = &ClassDeclaration> {
        self.declarations.iter().filter_map(|declaration| {
            if let Declaration::ClassDeclaration(data_declaration) = declaration {
                Some(data_declaration)
            } else {
                None
            }
        })
    }

    pub fn iter_data_declarations(&self) -> impl Iterator<Item = &DataDeclaration> {
        self.declarations.iter().filter_map(|declaration| {
            if let Declaration::DataDeclaration(data_declaration) = declaration {
                Some(data_declaration)
            } else {
                None
            }
        })
    }

    pub fn iter_value_declarations(&self) -> impl Iterator<Item = &ValueDeclaration> {
        self.declarations.iter().filter_map(|declaration| {
            if let Declaration::ValueDeclaration(value_declaration) = declaration {
                Some(value_declaration)
            } else {
                None
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    ClassDeclaration(ClassDeclaration),
    DataDeclaration(DataDeclaration),
    ValueDeclaration(ValueDeclaration),
}

// endregion

// region: DataDeclaration

#[derive(Debug, PartialEq, Eq)]
pub struct DataDeclaration {
    pub id: DataGroupId,
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

// endregion

// region: ClassDeclaration

#[derive(Debug, PartialEq, Eq)]
pub struct ClassDeclaration {
    pub id: ClassGroupId,
    pub name: Name,
    pub signature: Option<TypeId>,
    pub constraints: Vec<TypeId>,
    pub variables: Vec<TypeVariable>,
    pub fundeps: Vec<FunctionalDependency>,
    pub members: FxHashMap<AstId<ast::ClassMember>, ClassMember>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum FunctionalDependency {
    Determined(Vec<Name>),
    Determines(Vec<Name>, Vec<Name>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassMember {
    pub name: Name,
    pub ty: TypeId,
}

// region: ValueDeclaration

#[derive(Debug, PartialEq, Eq)]
pub struct ValueDeclaration {
    pub id: ValueGroupId,
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
    Guarded { guarded_exprs: Vec<GuardedExpr> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhereExpr {
    pub expr_id: ExprId,
    pub let_bindings: Vec<LetBinding>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GuardedExpr {
    pub pattern_guards: Vec<PatternGuard>,
    pub where_expr: WhereExpr,
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatternGuard {
    pub binder_id: Option<BinderId>,
    pub expr_id: ExprId,
}

// endregion

// region: Literal

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
    RecordPun(Name),
    RecordField(Name, I),
}

// endregion

// region: Expr

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Application(ExprId, Vec<ExprId>),
    Constructor(Qualified<Name>),
    IfThenElse(ExprId, ExprId, ExprId),
    InfixChain(ExprId, Vec<(ExprId, ExprId)>),
    Lambda(Vec<BinderId>, ExprId),
    LetIn(Vec<LetBinding>, ExprId),
    Literal(Literal<ExprId>),
    OperatorChain(ExprId, Vec<(Qualified<Name>, ExprId)>),
    Parenthesized(ExprId),
    Variable(Qualified<Name>),
    NotImplemented,
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
    pub annotation: Option<TypeId>,
    pub equations: Vec<LetNameEquation>,
}

pub type LetNameId = Idx<LetName>;

#[derive(Debug, PartialEq, Eq)]
pub struct LetNameEquation {
    pub binders: Vec<BinderId>,
    pub binding: Binding,
}

// endregion

// region: Binder

#[derive(Debug, PartialEq, Eq)]
pub enum Binder {
    Constructor { name: Qualified<Name>, fields: Vec<BinderId> },
    Literal(Literal<BinderId>),
    Negative(IntOrNumber),
    Parenthesized(BinderId),
    Variable(Name),
    Wildcard,
    NotImplemented,
}

pub type BinderId = Idx<Binder>;

// endregion

// region: Type

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Arrow(Vec<TypeId>, TypeId),
    Application(TypeId, Vec<TypeId>),
    Constrained(TypeId, TypeId),
    Constructor(Qualified<Name>),
    Forall(Vec<TypeVariable>, TypeId),
    OperatorChain(TypeId, Vec<(Qualified<Name>, TypeId)>),
    Parenthesized(TypeId),
    Variable(Name),
    NotImplemented,
}

pub type TypeId = Idx<Type>;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeVariable {
    Kinded(Name, TypeId),
    Name(Name),
}

impl TypeVariable {
    pub fn to_name(&self) -> Name {
        match self {
            TypeVariable::Kinded(name, _) => Name::clone(name),
            TypeVariable::Name(name) => Name::clone(name),
        }
    }
}

// endregion

// region: SurfaceArena

macro_rules! _surface_arena {
    ($($name:ident: $tree:ident),*) => {
        #[derive(Debug, Default, PartialEq, Eq)]
        pub struct SurfaceArena {
            $(
                $name: Arena<$tree>,
            )*
        }

        paste! {
            impl SurfaceArena {
                $(
                    pub fn [<alloc_ $name>](&mut self, $name: $tree) -> [<$tree Id>] {
                        self.$name.alloc($name)
                    }

                    pub fn [<iter_ $name>](&self) -> impl Iterator<Item = &$tree> {
                        self.$name.iter().map(|(_, tree)| tree)
                    }
                )*
            }

            $(
                impl Index<[<$tree Id>]> for SurfaceArena {
                    type Output = $tree;

                    fn index(&self, index: [<$tree Id>]) -> &$tree {
                        &self.$name[index]
                    }
                }
            )*
        }
    };
}

_surface_arena!(expr: Expr, let_name: LetName, binder: Binder, ty: Type);

// endregion

// region: SourceMap

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LetNamePtr {
    pub annotation_ptr: Option<SyntaxNodePtr>,
    pub equations_ptr: Vec<SyntaxNodePtr>,
}

/// A mapping from surface IDs to CST pointers.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct SourceMap {
    pub(crate) expr_to_cst: FxHashMap<ExprId, SyntaxNodePtr>,
    pub(crate) cst_to_expr: FxHashMap<SyntaxNodePtr, ExprId>,
    pub(crate) let_name_to_cst: FxHashMap<LetNameId, LetNamePtr>,
    pub(crate) cst_to_let_name: FxHashMap<SyntaxNodePtr, LetNameId>,
    pub(crate) binder_to_cst: FxHashMap<BinderId, SyntaxNodePtr>,
    pub(crate) cst_to_binder: FxHashMap<SyntaxNodePtr, BinderId>,
    pub(crate) type_to_cst: FxHashMap<TypeId, SyntaxNodePtr>,
    pub(crate) cst_to_type: FxHashMap<SyntaxNodePtr, TypeId>,
}

// endregion
