mod algorithm;
mod error;

pub use error::*;

use files::FileId;
use indexing::{FullIndexedModule, ImportKind, TermItemId, TypeItemId};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use std::sync::Arc;

/// External dependencies used in name resolution.
pub trait External {
    fn indexed(&mut self, id: FileId) -> Arc<FullIndexedModule>;

    fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule>;

    fn file_id(&mut self, name: &str) -> FileId;
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct FullResolvedModule {
    unqualified: ResolvedImportsUnqualified,
    qualified: ResolvedImportsQualified,
    exports: ResolvedItems,
    locals: ResolvedItems,
    errors: Vec<ResolvingError>,
}

type ResolvedImportsUnqualified = Vec<ResolvedImport>;
type ResolvedImportsQualified = FxHashMap<SmolStr, ResolvedImport>;

type TermMap = FxHashMap<SmolStr, (FileId, TermItemId)>;
type TypeMap = FxHashMap<SmolStr, (FileId, TypeItemId)>;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ResolvedItems {
    terms: TermMap,
    types: TypeMap,
}

impl ResolvedItems {
    fn lookup_term(&self, name: &str) -> Option<(FileId, TermItemId)> {
        self.terms.get(name).copied()
    }

    fn lookup_type(&self, name: &str) -> Option<(FileId, TypeItemId)> {
        self.types.get(name).copied()
    }

    fn iter_terms(&self) -> impl Iterator<Item = (&SmolStr, FileId, TermItemId)> {
        self.terms.iter().map(|(k, (f, i))| (k, *f, *i))
    }

    fn iter_types(&self) -> impl Iterator<Item = (&SmolStr, FileId, TypeItemId)> {
        self.types.iter().map(|(k, (f, i))| (k, *f, *i))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ResolvedImport {
    pub file: FileId,
    pub kind: ImportKind,
    pub exported: bool,
    items: ResolvedItems,
}

impl ResolvedImport {
    fn new(file: FileId, kind: ImportKind, exported: bool) -> ResolvedImport {
        let items = ResolvedItems::default();
        ResolvedImport { file, kind, exported, items }
    }

    fn contains_term(&self, name: &str) -> bool {
        self.items.terms.contains_key(name)
    }

    fn contains_type(&self, name: &str) -> bool {
        self.items.types.contains_key(name)
    }

    fn iter_terms(&self) -> impl Iterator<Item = (&SmolStr, FileId, TermItemId)> {
        self.items.terms.iter().map(|(k, (f, i))| (k, *f, *i))
    }

    fn iter_types(&self) -> impl Iterator<Item = (&SmolStr, FileId, TypeItemId)> {
        self.items.types.iter().map(|(k, (f, i))| (k, *f, *i))
    }
}

pub fn resolve_module(external: &mut impl External, file: FileId) -> FullResolvedModule {
    let algorithm::State { unqualified, qualified, exports, locals, errors } =
        algorithm::resolve_module(external, file);
    FullResolvedModule { unqualified, qualified, exports, locals, errors }
}
