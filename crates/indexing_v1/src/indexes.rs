use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;
use smol_str::SmolStr;

use crate::{
    ClassMemberId, ConstructorId, DeclarationId, ExportItemId, ImportId, InstanceId,
    InstanceMemberId,
};
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

/// Import declarations qualified with the same name.
///
/// ```purescript
/// import Indexing.Algorithm as Exported
/// import Indexing.Indexes as Exported
/// ```
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ImportGroup(Vec<ImportId>);

impl AsRef<[ImportId]> for ImportGroup {
    fn as_ref(&self) -> &[ImportId] {
        self.0.as_ref()
    }
}

impl ImportGroup {
    pub(crate) fn push(&mut self, import_id: ImportId) {
        self.0.push(import_id);
    }
}

pub type ImportGroupId = Id<ImportGroup>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Exportable<T> {
    pub(crate) value: T,
    pub(crate) export_id: Option<ExportItemId>,
}

impl<T> Exportable<T> {
    fn new(value: T) -> Exportable<T> {
        Exportable { value, export_id: None }
    }
}

type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

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
    qualified: FxIndexMap<SmolStr, Exportable<ImportGroup>>,
    expr_item: FxIndexMap<SmolStr, Exportable<ExprItem>>,
    type_item: FxIndexMap<SmolStr, Exportable<TypeItem>>,
}

pub(crate) type MutableItem<'t, T> = (&'t mut Exportable<T>, Id<T>);

fn get_mut_t<'t, T>(
    map: &'t mut FxIndexMap<SmolStr, Exportable<T>>,
    name: &str,
) -> Option<MutableItem<'t, T>> {
    let (index, _, value) = map.get_full_mut(name)?;
    Some((value, Id::from_raw(index)))
}

fn index_mut_t<T>(
    map: &mut FxIndexMap<SmolStr, Exportable<T>>,
    id: Id<T>,
) -> Option<MutableItem<'_, T>> {
    let index = id.into();
    let (_, value) = map.get_index_mut(index)?;
    Some((value, id))
}

fn insert_t<T>(map: &mut FxIndexMap<SmolStr, Exportable<T>>, name: SmolStr, item: T) -> Id<T> {
    let (index, _) = map.insert_full(name, Exportable::new(item));
    Id::from_raw(index)
}

impl NominalIndex {
    pub(crate) fn insert_qualified(&mut self, name: SmolStr, import: ImportId) {
        self.qualified
            .entry(name)
            .or_insert_with(|| Exportable::new(ImportGroup::default()))
            .value
            .push(import);
    }

    pub(crate) fn qualified_get_mut(&mut self, name: &str) -> Option<MutableItem<ImportGroup>> {
        get_mut_t(&mut self.qualified, name)
    }

    pub(crate) fn expr_get_mut(&mut self, name: &str) -> Option<MutableItem<ExprItem>> {
        get_mut_t(&mut self.expr_item, name)
    }

    pub(crate) fn expr_index_mut(&mut self, id: ExprItemId) -> Option<MutableItem<ExprItem>> {
        index_mut_t(&mut self.expr_item, id)
    }

    pub(crate) fn insert_expr(&mut self, name: SmolStr, item: ExprItem) -> ExprItemId {
        insert_t(&mut self.expr_item, name, item)
    }

    pub(crate) fn type_get_mut(&mut self, name: &str) -> Option<MutableItem<TypeItem>> {
        get_mut_t(&mut self.type_item, name)
    }

    pub(crate) fn insert_type(&mut self, name: SmolStr, item: TypeItem) -> TypeItemId {
        insert_t(&mut self.type_item, name, item)
    }
}

pub type NominalLookupResult<'t, T> = Option<(Id<T>, &'t T, Option<ExportItemId>)>;
pub type NominalIndexResult<'t, T> = Option<(&'t SmolStr, &'t T, Option<ExportItemId>)>;

fn lookup_t<'t, T>(
    map: &'t FxIndexMap<SmolStr, Exportable<T>>,
    name: &str,
) -> NominalLookupResult<'t, T> {
    let (id, _, Exportable { value, export_id }) = map.get_full(name)?;
    Some((Id::from_raw(id), value, *export_id))
}

fn index_t<T>(
    map: &FxIndexMap<SmolStr, Exportable<T>>,
    id: impl Into<usize>,
) -> NominalIndexResult<'_, T> {
    let index = id.into();
    let (key, Exportable { value, export_id }) = map.get_index(index)?;
    Some((key, value, *export_id))
}

fn iter_t<T>(
    map: &FxIndexMap<SmolStr, Exportable<T>>,
) -> impl Iterator<Item = (Id<T>, &SmolStr, &T)> {
    map.iter().enumerate().map(|(index, (name, item))| (Id::from_raw(index), name, &item.value))
}

impl NominalIndex {
    pub fn lookup_qualified(&self, name: &str) -> NominalLookupResult<ImportGroup> {
        lookup_t(&self.qualified, name)
    }

    pub fn index_qualified(&self, id: ImportGroupId) -> NominalIndexResult<ImportGroup> {
        index_t(&self.qualified, id)
    }

    pub fn lookup_expr_item(&self, name: &str) -> NominalLookupResult<ExprItem> {
        lookup_t(&self.expr_item, name)
    }

    pub fn index_expr_item(&self, id: ExprItemId) -> NominalIndexResult<ExprItem> {
        index_t(&self.expr_item, id)
    }

    pub fn iter_expr(&self) -> impl Iterator<Item = (ExprItemId, &SmolStr, &ExprItem)> {
        iter_t(&self.expr_item)
    }

    pub fn lookup_type_item(&self, name: &str) -> NominalLookupResult<TypeItem> {
        lookup_t(&self.type_item, name)
    }

    pub fn index_type_item(&self, id: TypeItemId) -> NominalIndexResult<TypeItem> {
        index_t(&self.type_item, id)
    }

    pub fn iter_type(&self) -> impl Iterator<Item = (TypeItemId, &SmolStr, &TypeItem)> {
        iter_t(&self.type_item)
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
    pub(crate) constructor_of: Vec<(TypeItemId, ExprItemId)>,
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
    pub fn constructors_of(&self, id: TypeItemId) -> impl Iterator<Item = ExprItemId> + '_ {
        find_u(&self.constructor_of, id)
    }

    pub fn of_constructor(&self, id: ExprItemId) -> Option<TypeItemId> {
        find_t(&self.constructor_of, id)
    }

    pub fn class_member_of(&self, id: TypeItemId) -> impl Iterator<Item = ClassMemberId> + '_ {
        find_u(&self.class_member_of, id)
    }

    pub fn of_class_member(&self, id: ClassMemberId) -> Option<TypeItemId> {
        find_t(&self.class_member_of, id)
    }
}
