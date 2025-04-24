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
    exports: ResolvedExports,
    errors: Vec<ResolvingError>,
}

impl FullResolvedModule {
    pub fn lookup_term(&self, name: &str) -> Option<(FileId, TermItemId)> {
        self.exports.terms.get(name).copied()
    }

    fn iter_terms(&self) -> impl Iterator<Item = (&SmolStr, FileId, TermItemId)> {
        self.exports.terms.iter().map(|(k, (f, i))| (k, *f, *i))
    }

    pub fn lookup_type(&self, name: &str) -> Option<(FileId, TypeItemId)> {
        self.exports.types.get(name).copied()
    }

    fn iter_types(&self) -> impl Iterator<Item = (&SmolStr, FileId, TypeItemId)> {
        self.exports.types.iter().map(|(k, (f, i))| (k, *f, *i))
    }
}

type ResolvedImportsUnqualified = Vec<ResolvedImport>;
type ResolvedImportsQualified = FxHashMap<SmolStr, ResolvedImport>;

type TermMap = FxHashMap<SmolStr, (FileId, TermItemId)>;
type TypeMap = FxHashMap<SmolStr, (FileId, TypeItemId)>;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ResolvedExports {
    terms: TermMap,
    types: TypeMap,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ResolvedImport {
    pub file: FileId,
    pub kind: ImportKind,
    pub exported: bool,
    terms: TermMap,
    types: TypeMap,
}

impl ResolvedImport {
    fn new(file: FileId, kind: ImportKind, exported: bool) -> ResolvedImport {
        let terms = TermMap::default();
        let types = TypeMap::default();
        ResolvedImport { file, kind, exported, terms, types }
    }

    fn contains_term(&self, name: &str) -> bool {
        self.terms.contains_key(name)
    }

    fn iter_terms(&self) -> impl Iterator<Item = (&SmolStr, FileId, TermItemId)> {
        self.terms.iter().map(|(k, (f, i))| (k, *f, *i))
    }

    fn contains_type(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }

    fn iter_types(&self) -> impl Iterator<Item = (&SmolStr, FileId, TypeItemId)> {
        self.types.iter().map(|(k, (f, i))| (k, *f, *i))
    }
}

pub fn resolve_module(external: &mut impl External, file: FileId) -> FullResolvedModule {
    let algorithm::State { unqualified, qualified, exports, errors } =
        algorithm::resolve_module(external, file);
    FullResolvedModule { unqualified, qualified, exports, errors }
}
