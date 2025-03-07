use std::sync::Arc;

use la_arena::{Arena, ArenaMap, Idx};
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

#[derive(Debug, PartialEq, Eq)]
pub enum ImplicitItems {
    Everything,
    Enumerated(Arc<[SmolStr]>),
}

pub type ImportedTerms = FxHashMap<SmolStr, ImportItemId>;
pub type ImportedTypes = FxHashMap<SmolStr, (ImportItemId, Option<ImplicitItems>)>;

#[derive(Debug, PartialEq, Eq)]
pub struct ImportedItems {
    pub terms: ImportedTerms,
    pub types: ImportedTypes,
    pub exported: bool,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Index {
    term_item: Arena<TermItem>,
    type_item: Arena<TypeItem>,
    term_export: ArenaMap<TermItemId, ExportItemId>,
    type_export: ArenaMap<TypeItemId, ExportItemId>,
    alias_nominal: FxHashMap<SmolStr, Vec<ImportId>>,
    imported_items: FxHashMap<ImportId, ImportedItems>,
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
        self.term_export.get(id).copied()
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
        self.type_export.get(id).copied()
    }

    pub(crate) fn insert_import_alias(&mut self, k: SmolStr, v: ImportId) {
        self.alias_nominal.entry(k).or_default().push(v);
    }

    pub(crate) fn export_import_alias(&mut self, k: &str) {
        if let Some(imports) = self.alias_nominal.get(k) {
            for import in imports {
                if let Some(items) = self.imported_items.get_mut(import) {
                    items.exported = true;
                }
            }
        }
    }

    pub(crate) fn insert_imported_items(&mut self, k: ImportId, v: ImportedItems) {
        self.imported_items.insert(k, v);
    }

    pub fn lookup_import_alias(&self, k: &str) -> Option<&[ImportId]> {
        Some(self.alias_nominal.get(k)?)
    }

    pub fn iter_term_item(&self) -> impl Iterator<Item = (TermItemId, &TermItem)> {
        self.term_item.iter()
    }

    pub fn iter_type_item(&self) -> impl Iterator<Item = (TypeItemId, &TypeItem)> {
        self.type_item.iter()
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
