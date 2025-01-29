use indexmap::IndexMap;
use smol_str::SmolStr;

use crate::{ClassMemberId, ConstructorId, DeclarationId, ImportId, InstanceId, InstanceMemberId};
use id::Id;

/// A group of type signature and declaration.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct TypeGroupId {
    pub signature: Option<DeclarationId>,
    pub declaration: Option<DeclarationId>,
    pub role: Option<DeclarationId>,
}

impl TypeGroupId {
    pub(crate) fn from_signature(id: DeclarationId) -> TypeGroupId {
        TypeGroupId { signature: Some(id), declaration: None, role: None }
    }

    pub(crate) fn from_declaration(id: DeclarationId) -> TypeGroupId {
        TypeGroupId { signature: None, declaration: Some(id), role: None }
    }
}

/// A group of value signature and equations.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ValueGroupId {
    pub signature: Option<DeclarationId>,
    pub equations: Vec<DeclarationId>,
}

impl ValueGroupId {
    pub(crate) fn from_signature(id: DeclarationId) -> ValueGroupId {
        ValueGroupId { signature: Some(id), equations: Vec::new() }
    }

    pub(crate) fn from_equation(id: DeclarationId) -> ValueGroupId {
        ValueGroupId { signature: None, equations: vec![id] }
    }
}

pub type ExprItemId = Id<ExprItem>;

/// An item in the expression namespace.
#[derive(Debug, PartialEq, Eq)]
pub enum ExprItem {
    Constructor(ConstructorId),
    Instance(InstanceId),
    Derive(DeclarationId),
    ClassMember(ClassMemberId),
    Value(ValueGroupId),
    Foreign(DeclarationId),
    Operator(DeclarationId),
}

pub type TypeItemId = Id<TypeItem>;

/// An item in the type namespace.
#[derive(Debug, PartialEq, Eq)]
pub enum TypeItem {
    Class(TypeGroupId),
    Data(TypeGroupId),
    Newtype(TypeGroupId),
    Synonym(TypeGroupId),
    Foreign(TypeGroupId),
    Operator(DeclarationId),
}

/// Mapping from names to module items.
///
/// In PureScript, names can be shared between different kinds of declarations.
/// Lowercase names can be used to refer to class methods, value declarations,
/// and instance names.
///
/// ```purescript
/// class Eq a where
///   eq :: a -> a -> Boolean
///
/// isJust :: Maybe a -> Boolean
/// iJust (Just _) = true
/// isJust _ = false
///
/// instance eqInt :: Eq Int where
///   eq = eqIntImpl
/// ```
///
/// Meanwhile, uppercase names can be used to refer to different kinds of types
/// in the language, namely `class`, `data`, `newtype`, and `type` declarations.
///
/// ```purescript
/// class Eq a where
///   eq :: a -> a -> Boolean
///
/// data Reader r a = Reader (r -> a)
///
/// newtype Identity a = Identity a
///
/// type Id = Int
/// ```
///
/// From this, we can infer that there are two namespaces in the language:
/// 1. The expression namespace, which contains [`ExprItem`].
/// 2. The type namespace, which contains [`TypeItem`].
///
/// <div class="warning">
///
/// **NOTE**: Constructors exist in the expression namespace.
///
/// </div>
///
/// The `NominalIndex` provides a mapping from names to namespace items.
/// During the indexing phase, any conflicts between names in a namespace
/// would emit the pertinent error.
/// - [`DuplicateExprItem`] for the expression namespace.
/// - [`DuplicateTypeItem`] for the type namespace.
///
/// These errors contain [`ExprItemId`] and [`TypeItemId`] respectively, which
/// can be exchanged to [`ExprItem`] and [`TypeItem`] using the `NominalIndex`.
/// Subsequently, the IDs contained within these item types can be exchanged
/// from the [`SourceMap`] to obtain source span information for error reports;
/// or the [`RelationalIndex`] to obtain related module items.
///
/// Internally, each namespace is implemented as an [`IndexMap`]. This enables
/// the `NominalIndex` to provide both hash-based and index-based lookups for
/// module items. Likewise, these indices can also be used as semi-stable keys
/// for a query-based incremental compilation system.
///
/// [`DuplicateExprItem`]: crate::IndexingError::DuplicateExprItem
/// [`DuplicateTypeItem`]: crate::IndexingError::DuplicateTypeItem
/// [`SourceMap`]: crate::SourceMap
#[derive(Debug, Default)]
pub struct NominalIndex {
    qualified: IndexMap<SmolStr, Vec<ImportId>>,
    expr_item: IndexMap<SmolStr, ExprItem>,
    type_item: IndexMap<SmolStr, TypeItem>,
}

pub(crate) type MutableItem<'t, T> = (&'t mut T, Id<T>);

impl NominalIndex {
    pub(crate) fn insert_qualified(&mut self, name: SmolStr, import: ImportId) {
        self.qualified.entry(name).or_default().push(import);
    }

    pub(crate) fn expr_get_mut(&mut self, name: &str) -> Option<MutableItem<ExprItem>> {
        self.expr_item.get_full_mut(name).map(|(index, _, item)| (item, Id::from_raw(index)))
    }

    pub(crate) fn insert_expr(&mut self, name: SmolStr, item: ExprItem) -> ExprItemId {
        let (index, _) = self.expr_item.insert_full(name, item);
        Id::from_raw(index)
    }

    pub(crate) fn iter_expr(&self) -> impl Iterator<Item = (ExprItemId, &SmolStr, &ExprItem)> {
        self.expr_item.iter().enumerate().map(|(i, (k, v))| (Id::from_raw(i), k, v))
    }

    pub(crate) fn type_get_mut(&mut self, name: &str) -> Option<MutableItem<TypeItem>> {
        self.type_item.get_full_mut(name).map(|(index, _, item)| (item, Id::from_raw(index)))
    }

    pub(crate) fn insert_type(&mut self, name: SmolStr, item: TypeItem) -> TypeItemId {
        let (index, _) = self.type_item.insert_full(name, item);
        Id::from_raw(index)
    }

    pub(crate) fn iter_type(&self) -> impl Iterator<Item = (TypeItemId, &SmolStr, &TypeItem)> {
        self.type_item.iter().enumerate().map(|(i, (k, v))| (Id::from_raw(i), k, v))
    }
}

impl NominalIndex {
    pub fn lookup_qualified(&self, name: &str) -> Option<&[ImportId]> {
        self.qualified.get(name).map(|v| &v[..])
    }

    pub fn lookup_expr_item(&self, name: &str) -> Option<(ExprItemId, &ExprItem)> {
        self.expr_item.get_full(name).map(|(id, _, item)| (Id::from_raw(id), item))
    }

    pub fn index_expr_item(&self, id: ExprItemId) -> Option<&ExprItem> {
        let index = id.into();
        self.expr_item.get_index(index).map(|(_, item)| item)
    }

    pub fn lookup_type_item(&self, name: &str) -> Option<(TypeItemId, &TypeItem)> {
        self.type_item.get_full(name).map(|(id, _, item)| (Id::from_raw(id), item))
    }

    pub fn index_type_item(&self, id: TypeItemId) -> Option<&TypeItem> {
        let index = id.into();
        self.type_item.get_index(index).map(|(_, item)| item)
    }
}

/// Mapping between related module items.
///
/// In PureScript, there are several one-to-many relationships between module
/// items. For example, classes have methods and data types have constructors.
/// The `RelationalIndex` provides an interface for searching these mappings
/// in both directions.
///
/// Internally, each relationship is implemented as a `Vec<(T, U)>`, where both
/// `T` and `U` are [`Id`]. This representation enables the `RelationalIndex`
/// to provide bidirectional lookups for these relationships using a simple
/// linear search.
///
/// The number of these relationships are often small for any given module, and
/// a linear search is often sufficient for most use cases. In the future, the
/// `RelationalIndex` could make use of SIMD-based lookups especially for large,
/// code-generated PureScript files with hundreds if not thousands of relationships.
#[derive(Debug, Default)]
pub struct RelationalIndex {
    pub(crate) constructor_of: Vec<(TypeItemId, ConstructorId)>,
    pub(crate) class_member_of: Vec<(TypeItemId, ClassMemberId)>,
    pub(crate) instance_of: Vec<(DeclarationId, InstanceId)>,
    pub(crate) instance_member_of: Vec<(InstanceId, InstanceMemberId)>,
}

fn find_t<T, U>(haystack: &[(Id<T>, Id<U>)], needle: Id<U>) -> Option<Id<T>> {
    haystack.iter().find_map(|(t, u)| if *u == needle { Some(*t) } else { None })
}

fn find_u<T, U>(haystack: &[(Id<T>, Id<U>)], needle: Id<T>) -> impl Iterator<Item = Id<U>> + '_ {
    haystack.iter().filter_map(move |(t, u)| if *t == needle { Some(*u) } else { None })
}

impl RelationalIndex {
    pub fn constructors_of(&self, id: TypeItemId) -> impl Iterator<Item = ConstructorId> + '_ {
        find_u(&self.constructor_of, id)
    }

    pub fn of_constructor(&self, id: ConstructorId) -> Option<TypeItemId> {
        find_t(&self.constructor_of, id)
    }

    pub fn class_member_of(&self, id: TypeItemId) -> impl Iterator<Item = ClassMemberId> + '_ {
        find_u(&self.class_member_of, id)
    }

    pub fn of_class_member(&self, id: ClassMemberId) -> Option<TypeItemId> {
        find_t(&self.class_member_of, id)
    }
}
