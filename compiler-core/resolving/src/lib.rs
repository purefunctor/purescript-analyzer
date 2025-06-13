mod algorithm;
mod error;

pub use error::*;

use files::FileId;
use indexing::{FullIndexedModule, ImportId, ImportKind, TermItemId, TypeItemId};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use std::sync::Arc;

/// External dependencies used in name resolution.
pub trait External {
    fn indexed(&mut self, id: FileId) -> Arc<FullIndexedModule>;

    fn resolved(&mut self, id: FileId) -> Arc<FullResolvedModule>;

    fn module_file(&mut self, name: &str) -> Option<FileId>;
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct FullResolvedModule {
    pub unqualified: ResolvedImportsUnqualified,
    pub qualified: ResolvedImportsQualified,
    pub exports: ResolvedItems,
    pub locals: ResolvedItems,
    pub errors: Vec<ResolvingError>,
}

impl FullResolvedModule {
    pub fn lookup_term(&self, prefix: Option<&str>, name: &str) -> Option<(FileId, TermItemId)> {
        if let Some(prefix) = prefix {
            let import = self.qualified.get(prefix)?;
            let (file, id, kind) = import.lookup_term(name)?;
            if matches!(kind, ImportKind::Hidden) { None } else { Some((file, id)) }
        } else {
            let local = self.locals.lookup_term(name);
            let unqualified = || {
                let imports = self.unqualified.iter();
                // Collect candidates first then match the first non-Hidden.
                let (file, id, _) = imports
                    .filter_map(|import| import.lookup_term(name))
                    .find(|(_, _, kind)| !matches!(kind, ImportKind::Hidden))?;
                Some((file, id))
            };
            local.or_else(unqualified)
        }
    }

    pub fn lookup_type(&self, prefix: Option<&str>, name: &str) -> Option<(FileId, TypeItemId)> {
        if let Some(prefix) = prefix {
            let import = self.qualified.get(prefix)?;
            let (file, id, kind) = import.lookup_type(name)?;
            if matches!(kind, ImportKind::Hidden) { None } else { Some((file, id)) }
        } else {
            let local = self.locals.lookup_type(name);
            let unqualified = || {
                let imports = self.unqualified.iter();
                // Collect candidates first then match the first non-Hidden.
                let (file, id, _) = imports
                    .filter_map(|import| import.lookup_type(name))
                    .find(|(_, _, kind)| !matches!(kind, ImportKind::Hidden))?;
                Some((file, id))
            };
            local.or_else(unqualified)
        }
    }
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
    pub fn lookup_term(&self, name: &str) -> Option<(FileId, TermItemId)> {
        self.terms.get(name).copied()
    }

    pub fn lookup_type(&self, name: &str) -> Option<(FileId, TypeItemId)> {
        self.types.get(name).copied()
    }

    pub fn iter_terms(&self) -> impl Iterator<Item = (&SmolStr, FileId, TermItemId)> {
        self.terms.iter().map(|(k, (f, i))| (k, *f, *i))
    }

    pub fn iter_types(&self) -> impl Iterator<Item = (&SmolStr, FileId, TypeItemId)> {
        self.types.iter().map(|(k, (f, i))| (k, *f, *i))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ResolvedImport {
    pub id: ImportId,
    pub file: FileId,
    pub kind: ImportKind,
    pub exported: bool,
    terms: FxHashMap<SmolStr, (FileId, TermItemId, ImportKind)>,
    types: FxHashMap<SmolStr, (FileId, TypeItemId, ImportKind)>,
}

impl ResolvedImport {
    fn new(id: ImportId, file: FileId, kind: ImportKind, exported: bool) -> ResolvedImport {
        let terms = FxHashMap::default();
        let types = FxHashMap::default();
        ResolvedImport { id, file, kind, exported, terms, types }
    }

    pub fn lookup_term(&self, name: &str) -> Option<(FileId, TermItemId, ImportKind)> {
        self.terms.get(name).copied()
    }

    fn lookup_type(&self, name: &str) -> Option<(FileId, TypeItemId, ImportKind)> {
        self.types.get(name).copied()
    }

    pub fn iter_terms(&self) -> impl Iterator<Item = (&SmolStr, FileId, TermItemId, ImportKind)> {
        self.terms.iter().map(|(k, (f, i, d))| (k, *f, *i, *d))
    }

    pub fn iter_types(&self) -> impl Iterator<Item = (&SmolStr, FileId, TypeItemId, ImportKind)> {
        self.types.iter().map(|(k, (f, i, d))| (k, *f, *i, *d))
    }
}

pub fn resolve_module(external: &mut impl External, file: FileId) -> FullResolvedModule {
    let algorithm::State { unqualified, qualified, exports, locals, errors } =
        algorithm::resolve_module(external, file);
    FullResolvedModule { unqualified, qualified, exports, locals, errors }
}
