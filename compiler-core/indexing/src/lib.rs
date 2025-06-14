mod algorithm;
mod error;
mod items;
mod source;

pub use error::*;
pub use items::*;
pub use source::*;

use std::ops;

use la_arena::Arena;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use syntax::cst;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct FullIndexedModule {
    pub kind: ExportKind,
    pub items: IndexingItems,
    pub imports: IndexingImports,
    pub pairs: IndexingPairs,
    pub source: IndexingSource,
    pub errors: Vec<IndexingError>,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct IndexingItems {
    terms: Arena<TermItem>,
    types: Arena<TypeItem>,
}

impl IndexingItems {
    pub fn iter_terms(&self) -> impl Iterator<Item = (TermItemId, &TermItem)> {
        self.terms.iter()
    }

    pub fn iter_types(&self) -> impl Iterator<Item = (TypeItemId, &TypeItem)> {
        self.types.iter()
    }
}

impl ops::Index<TermItemId> for IndexingItems {
    type Output = TermItem;

    fn index(&self, index: TermItemId) -> &TermItem {
        &self.terms[index]
    }
}

impl ops::Index<TypeItemId> for IndexingItems {
    type Output = TypeItem;

    fn index(&self, index: TypeItemId) -> &TypeItem {
        &self.types[index]
    }
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

#[derive(Debug, PartialEq, Eq)]
pub enum ImplicitItems {
    Everything,
    Enumerated(Box<[SmolStr]>),
}

pub type ImportedTerms = FxHashMap<SmolStr, ImportItemId>;
pub type ImportedTypes = FxHashMap<SmolStr, (ImportItemId, Option<ImplicitItems>)>;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct IndexingImport {
    pub name: Option<SmolStr>,
    pub alias: Option<SmolStr>,
    pub kind: ImportKind,
    pub terms: ImportedTerms,
    pub types: ImportedTypes,
    pub exported: bool,
}

pub type IndexingImports = FxHashMap<ImportId, IndexingImport>;

impl IndexingImport {
    pub(crate) fn new(name: Option<SmolStr>, alias: Option<SmolStr>) -> IndexingImport {
        IndexingImport { name, alias, ..Default::default() }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct IndexingPairs {
    class_members: Vec<(TypeItemId, TermItemId)>,
    data_constructors: Vec<(TypeItemId, TermItemId)>,
    instance_chain: Vec<(InstanceChainId, InstanceId)>,
    instance_members: Vec<(InstanceId, InstanceMemberId)>,
}

impl IndexingPairs {
    pub fn data_constructors(&self, id: TypeItemId) -> impl Iterator<Item = TermItemId> {
        self.data_constructors.iter().filter_map(
            move |(type_id, term_id)| if *type_id == id { Some(*term_id) } else { None },
        )
    }

    pub fn constructor_type(&self, id: TermItemId) -> Option<TypeItemId> {
        self.data_constructors
            .iter()
            .find_map(|(type_id, term_id)| if *term_id == id { Some(*type_id) } else { None })
    }

    pub fn class_members(&self, id: TypeItemId) -> impl Iterator<Item = TermItemId> {
        self.class_members.iter().filter_map(
            move |(type_id, term_id)| if *type_id == id { Some(*term_id) } else { None },
        )
    }
}

pub fn index_module(cst: &cst::Module) -> FullIndexedModule {
    let algorithm::State { kind, items, imports, pairs, source, errors, .. } =
        algorithm::index_module(cst);
    FullIndexedModule { kind, items, imports, pairs, source, errors }
}
