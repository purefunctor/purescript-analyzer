mod algorithm;

use lowering::FullModuleLower;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use std::sync::Arc;

use files::FileId;
use indexing::{FullModuleIndex, ImportId, ImportItemId, ImportKind, TermItemId, TypeItemId};

/// External dependencies used in name resolution.
pub trait External {
    fn indexed(&mut self, id: FileId) -> Arc<FullModuleIndex>;

    fn lowered(&mut self, id: FileId) -> Arc<FullModuleLower>;

    fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule>;

    fn file_id(&mut self, name: &str) -> FileId;
}

/// The kind of errors produced during name resolution.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    ExistingTerm { existing: (FileId, TermItemId), duplicate: (FileId, TermItemId) },
    ExistingType { existing: (FileId, TypeItemId), duplicate: (FileId, TypeItemId) },
    InvalidImportStatement { id: ImportId },
    InvalidImportItem { id: ImportItemId },
    DuplicateImportItem { id: ImportItemId },
}

type UnqualifiedResolvedImports = Vec<ResolvedImport>;
type QualifiedResolvedImports = FxHashMap<SmolStr, ResolvedImport>;

type TermMap = FxHashMap<SmolStr, (FileId, TermItemId)>;
type TypeMap = FxHashMap<SmolStr, (FileId, TypeItemId)>;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct FullResolvedModule {
    unqualified: UnqualifiedResolvedImports,
    qualified: QualifiedResolvedImports,
    exports: ResolvedExports,
    errors: Vec<Error>,
}

impl FullResolvedModule {
    pub fn lookup_term(&self, name: &str) -> Option<(FileId, TermItemId)> {
        self.exports.terms.get(name).copied()
    }

    pub fn iter_terms(&self) -> impl Iterator<Item = (&str, FileId, TermItemId)> {
        self.exports.terms.iter().map(|(k, (f, i))| (k.as_str(), *f, *i))
    }

    pub fn lookup_type(&self, name: &str) -> Option<(FileId, TypeItemId)> {
        self.exports.types.get(name).copied()
    }

    pub fn iter_types(&self) -> impl Iterator<Item = (&str, FileId, TypeItemId)> {
        self.exports.types.iter().map(|(k, (f, i))| (k.as_str(), *f, *i))
    }
}

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

    pub fn contains_term(&self, name: &str) -> bool {
        self.terms.contains_key(name)
    }

    pub fn iter_terms(&self) -> impl Iterator<Item = (&str, FileId, TermItemId)> {
        self.terms.iter().map(|(k, (f, i))| (k.as_str(), *f, *i))
    }

    pub fn contains_type(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }

    pub fn iter_types(&self) -> impl Iterator<Item = (&str, FileId, TypeItemId)> {
        self.types.iter().map(|(k, (f, i))| (k.as_str(), *f, *i))
    }
}

pub fn resolve_module(external: &mut impl External, file: FileId) -> FullResolvedModule {
    let mut errors = vec![];

    let index = external.indexed(file);
    let (unqualified, qualified) = algorithm::resolve_imports(external, &mut errors, &index.index);

    let imports = unqualified.iter().chain(qualified.values());
    let exports = algorithm::resolve_exports(external, &mut errors, &index.index, imports, file);

    FullResolvedModule { unqualified, qualified, exports, errors }
}
