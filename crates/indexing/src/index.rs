use std::sync::Arc;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::source::*;

#[derive(Debug, PartialEq, Eq)]
pub enum TermItem {
    ClassMember { id: ClassMemberId },
    Constructor { id: DataConstructorId },
    Derive { id: DeriveId },
    Foreign { id: ForeignValueId },
    Instance { id: InstanceId },
    Operator { id: InfixId },
    Value { signature: Option<ValueSignatureId>, equations: Vec<ValueEquationId> },
}

pub type TermItemId = Idx<TermItem>;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeItem {
    Data {
        signature: Option<DataSignatureId>,
        equation: Option<DataEquationId>,
        role: Option<TypeRoleId>,
    },
    Newtype {
        signature: Option<NewtypeSignatureId>,
        equation: Option<NewtypeEquationId>,
        role: Option<TypeRoleId>,
    },
    Synonym {
        signature: Option<TypeSignatureId>,
        equation: Option<TypeEquationId>,
    },
    Class {
        signature: Option<ClassSignatureId>,
        declaration: Option<ClassDeclarationId>,
    },
    Foreign {
        id: ForeignDataId,
    },
    Operator {
        id: InfixId,
    },
}

pub type TypeItemId = Idx<TypeItem>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImplicitItems {
    Everything,
    Enumerated(Arc<[SmolStr]>),
}

pub type ImportedTerms = FxHashMap<SmolStr, ImportItemId>;
pub type ImportedTypes = FxHashMap<SmolStr, (ImportItemId, Option<ImplicitItems>)>;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ImportItems {
    pub name: Option<SmolStr>,
    pub alias: Option<SmolStr>,
    pub kind: ImportKind,
    pub terms: ImportedTerms,
    pub types: ImportedTypes,
    pub exported: bool,
}

impl ImportItems {
    pub(crate) fn new(name: Option<SmolStr>, alias: Option<SmolStr>) -> ImportItems {
        ImportItems { name, alias, ..Default::default() }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Index {
    pub export_kind: ExportKind,

    term_item: Arena<TermItem>,
    type_item: Arena<TypeItem>,

    term_export: FxHashMap<TermItemId, ExportItemId>,
    type_export: FxHashMap<TypeItemId, ExportItemId>,

    import_items: FxHashMap<ImportId, ImportItems>,
    term_nominal: FxHashMap<SmolStr, TermItemId>,
    type_nominal: FxHashMap<SmolStr, TypeItemId>,
}

impl std::ops::Index<TermItemId> for Index {
    type Output = TermItem;

    fn index(&self, id: TermItemId) -> &TermItem {
        &self.term_item[id]
    }
}

impl std::ops::Index<TypeItemId> for Index {
    type Output = TypeItem;

    fn index(&self, id: TypeItemId) -> &TypeItem {
        &self.type_item[id]
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Relational {
    data: Vec<(TypeItemId, TermItemId)>,
    class: Vec<(TypeItemId, TermItemId)>,
    chain: Vec<(InstanceChainId, InstanceId)>,
    instance: Vec<(InstanceId, InstanceMemberId)>,
}

impl Index {
    pub(crate) fn insert_term_item(&mut self, k: Option<SmolStr>, v: TermItem) -> TermItemId {
        let id = self.term_item.alloc(v);
        let Some(k) = k else { return id };
        self.term_nominal.insert(k, id);
        id
    }

    pub(crate) fn term_item_mut(&mut self, k: &str) -> Option<(&mut TermItem, TermItemId)> {
        let &id = self.term_nominal.get(k)?;
        Some((&mut self.term_item[id], id))
    }

    pub(crate) fn export_term_item(&mut self, item_id: TermItemId, id: ExportItemId) {
        self.term_export.insert(item_id, id);
    }

    pub(crate) fn term_item_export(&self, id: TermItemId) -> Option<ExportItemId> {
        self.term_export.get(&id).copied()
    }

    pub(crate) fn insert_type_item(&mut self, k: Option<SmolStr>, v: TypeItem) -> TypeItemId {
        let id = self.type_item.alloc(v);
        let Some(k) = k else { return id };
        self.type_nominal.insert(k, id);
        id
    }

    pub(crate) fn type_item_mut(&mut self, k: &str) -> Option<(&mut TypeItem, TypeItemId)> {
        let &id = self.type_nominal.get(k)?;
        Some((&mut self.type_item[id], id))
    }

    pub(crate) fn export_type_item(&mut self, item_id: TypeItemId, id: ExportItemId) {
        self.type_export.insert(item_id, id);
    }

    pub(crate) fn type_item_export(&self, id: TypeItemId) -> Option<ExportItemId> {
        self.type_export.get(&id).copied()
    }

    pub(crate) fn insert_import_items(&mut self, k: ImportId, v: ImportItems) {
        self.import_items.insert(k, v);
    }

    /// Marks [`ImportItems`] matching a module name as exported.
    ///
    /// PureScript supports the following export forms:
    ///
    /// 1. Using the alias:
    ///
    /// ```purescript
    /// module Main (module Maybe) where
    ///
    /// import Data.Maybe as Maybe
    /// ```
    ///
    /// 2. Using the name:
    ///
    /// ```purescript
    /// module Main (module Data.Maybe) where
    ///
    /// import Data.Maybe (isJust)
    /// ```
    ///
    /// Modules can only be exported using its full name if it's not aliased.
    /// As a result, the following export form is invalid:
    ///
    /// ```purescript
    /// module Main (module Data.Maybe) where
    ///
    /// import Data.Maybe as Maybe
    /// ```
    pub(crate) fn export_import_items(&mut self, k: &str) {
        for (_, items) in self.import_items.iter_mut() {
            let alias = items.alias.as_deref();
            let name = items.name.as_deref();

            let using_alias = alias == Some(k);
            let using_name = alias.is_none() && name == Some(k);

            if using_alias || using_name {
                items.exported = true;
            }
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum ImportKind {
    #[default]
    /// import Lib
    Implicit,
    /// import Lib (value, Type, ...)
    Explicit,
    /// import Lib hiding (value, Type, ...)
    Hidden,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub enum ExportKind {
    #[default]
    /// module Main where
    Implicit,
    /// module Main (value, Type, ...) where
    Explicit,
    /// module Main (module Main, ...) where
    ExplicitSelf,
}

impl Index {
    pub fn iter_import_items(&self) -> impl Iterator<Item = (ImportId, &ImportItems)> {
        self.import_items.iter().map(|(k, v)| (*k, v))
    }

    pub fn iter_term_nominal(&self) -> impl Iterator<Item = (&str, TermItemId)> {
        self.term_nominal.iter().map(|(k, v)| (k.as_str(), *v))
    }

    pub fn iter_type_nominal(&self) -> impl Iterator<Item = (&str, TypeItemId)> {
        self.type_nominal.iter().map(|(k, v)| (k.as_str(), *v))
    }

    pub fn iter_term_item(&self) -> impl Iterator<Item = (TermItemId, &TermItem)> {
        self.term_item.iter()
    }

    pub fn iter_type_item(&self) -> impl Iterator<Item = (TypeItemId, &TypeItem)> {
        self.type_item.iter()
    }

    pub fn iter_exported_terms(&self) -> impl Iterator<Item = (&str, TermItemId)> {
        self.iter_term_nominal().filter(|(_, id)| self.term_export.contains_key(id))
    }

    pub fn iter_exported_types(&self) -> impl Iterator<Item = (&str, TypeItemId)> {
        self.iter_type_nominal().filter(|(_, id)| self.type_export.contains_key(id))
    }
}

impl Relational {
    pub(crate) fn insert_data_relation(&mut self, type_id: TypeItemId, term_id: TermItemId) {
        self.data.push((type_id, term_id));
    }

    pub(crate) fn insert_class_relation(&mut self, type_id: TypeItemId, term_id: TermItemId) {
        self.class.push((type_id, term_id));
    }

    pub(crate) fn insert_chain_relation(&mut self, c_id: InstanceChainId, i_id: InstanceId) {
        self.chain.push((c_id, i_id));
    }

    pub(crate) fn insert_instance_relation(&mut self, i_id: InstanceId, m_id: InstanceMemberId) {
        self.instance.push((i_id, m_id));
    }
}

impl Relational {
    pub fn constructors_of(&self, id: TypeItemId) -> impl Iterator<Item = TermItemId> + '_ {
        self.data.iter().filter_map(
            move |(type_id, term_id)| {
                if id == *type_id {
                    Some(*term_id)
                } else {
                    None
                }
            },
        )
    }

    pub fn class_members_of(&self, id: TypeItemId) -> impl Iterator<Item = TermItemId> + '_ {
        self.class.iter().filter_map(
            move |(type_id, term_id)| {
                if id == *type_id {
                    Some(*term_id)
                } else {
                    None
                }
            },
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemKind {
    DataSignature(DataSignatureId),
    DataEquation(DataEquationId),

    NewtypeSignature(NewtypeSignatureId),
    NewtypeEquation(NewtypeEquationId),

    Constructor(DataConstructorId),

    SynonymSignature(TypeSignatureId),
    SynonymEquation(TypeEquationId),

    ClassSignature(ClassSignatureId),
    ClassDeclaration(ClassDeclarationId),
    ClassMember(ClassMemberId),

    ValueSignature(ValueSignatureId),
    ValueEquation(ValueEquationId),

    ForeignData(ForeignDataId),
    ForeignValue(ForeignValueId),
    Operator(InfixId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExistingKind {
    Term(TermItemId),
    Type(TypeItemId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum IndexError {
    DuplicateItem { kind: ItemKind, existing: ExistingKind },
    MismatchedItem { kind: ItemKind, existing: ExistingKind },
    InvalidRole { id: TypeRoleId, existing: Option<TypeItemId> },
    InvalidExport { id: ExportItemId },
    DuplicateExport { id: ExportItemId, existing: ExportItemId },
    DuplicateImport { id: ImportItemId, existing: ImportItemId },
}
