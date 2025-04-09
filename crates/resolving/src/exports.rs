//! Implements name resolution for export lists.
use std::collections::hash_map::Entry;

use files::FileId;
use indexing::{ImplicitItems, ImportItems, ImportKind, Index, TermItemId, TypeItemId};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

use crate::{Error, External};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct FullModuleExports {
    pub errors: Vec<Error>,
    terms: FxHashMap<SmolStr, (FileId, TermItemId)>,
    types: FxHashMap<SmolStr, (FileId, TypeItemId)>,
}

impl FullModuleExports {
    pub fn iter_terms(&self) -> impl Iterator<Item = (&str, FileId, TermItemId)> {
        self.terms.iter().map(|(k, (f, t))| (k.as_str(), *f, *t))
    }

    pub fn iter_types(&self) -> impl Iterator<Item = (&str, FileId, TypeItemId)> {
        self.types.iter().map(|(k, (f, t))| (k.as_str(), *f, *t))
    }
}

pub fn module_exports(external: &mut impl External, file: FileId) -> FullModuleExports {
    let mut state = State::default();
    let index = &external.index(file).index;
    match index.export_kind {
        indexing::ExportKind::Implicit => {
            implicit_module_exports(&mut state, file, index);
        }
        indexing::ExportKind::Explicit => {
            explicit_module_exports(external, &mut state, file, index, false);
        }
        indexing::ExportKind::ExplicitSelf => {
            explicit_module_exports(external, &mut state, file, index, true);
        }
    }
    let State { errors, terms, types } = state;
    FullModuleExports { errors, terms, types }
}

#[derive(Debug, Default)]
struct State {
    errors: Vec<Error>,
    terms: FxHashMap<SmolStr, (FileId, TermItemId)>,
    types: FxHashMap<SmolStr, (FileId, TypeItemId)>,
}

impl State {
    fn add_terms<'a>(&mut self, iterator: impl Iterator<Item = (&'a str, FileId, TermItemId)>) {
        let (additional, _) = iterator.size_hint();
        self.terms.reserve(additional);
        iterator.for_each(move |(name, file, id)| {
            self.add_term(name, file, id);
        });
    }

    fn add_types<'a>(&mut self, iterator: impl Iterator<Item = (&'a str, FileId, TypeItemId)>) {
        let (additional, _) = iterator.size_hint();
        self.types.reserve(additional);
        iterator.for_each(move |(name, file, id)| {
            self.add_type(name, file, id);
        });
    }

    fn add_term(&mut self, name: &str, file: FileId, id: TermItemId) {
        let k = SmolStr::from(name);
        match self.terms.entry(k) {
            Entry::Occupied(o) => {
                let existing = *o.get();
                let duplicate = (file, id);
                if existing != duplicate {
                    self.errors.push(Error::ExistingTerm { existing, duplicate });
                }
            }
            Entry::Vacant(v) => {
                v.insert((file, id));
            }
        }
    }

    fn add_type(&mut self, name: &str, file: FileId, id: TypeItemId) {
        let k = SmolStr::from(name);
        match self.types.entry(k) {
            Entry::Occupied(o) => {
                let existing = *o.get();
                let duplicate = (file, id);
                if existing != duplicate {
                    self.errors.push(Error::ExistingType { existing, duplicate });
                }
            }
            Entry::Vacant(v) => {
                v.insert((file, id));
            }
        }
    }
}

fn implicit_module_exports(state: &mut State, file: FileId, index: &Index) {
    let terms = index.iter_term_nominal().map(|(name, id)| (name, file, id));
    let types = index.iter_type_nominal().map(|(name, id)| (name, file, id));
    state.add_terms(terms);
    state.add_types(types);
}

fn explicit_module_exports(
    external: &mut impl External,
    state: &mut State,
    file: FileId,
    index: &Index,
    including_self: bool,
) {
    if including_self {
        implicit_module_exports(state, file, index);
    } else {
        let terms = index.iter_exported_terms().map(|(name, id)| (name, file, id));
        let types = index.iter_exported_types().map(|(name, id)| (name, file, id));
        state.add_terms(terms);
        state.add_types(types);
    }

    let imports: Vec<_> = index
        .iter_import_items()
        .filter_map(|(_, items)| elaborate_import_items(external, state, items))
        .collect();

    for import in &imports {
        match import.kind {
            ImportKind::Implicit => {
                let exports = external.exports(import.file);
                let terms = exports.iter_terms();
                let types = exports.iter_types();
                state.add_terms(terms);
                state.add_types(types);
            }
            ImportKind::Explicit => {
                let terms = import.iter_terms();
                let types = import.iter_types();
                state.add_terms(terms);
                state.add_types(types);
            }
            ImportKind::Hidden => {
                let exports = external.exports(import.file);
                let terms = exports.iter_terms().filter(|(k, _, _)| !import.terms.contains_key(*k));
                let types = exports.iter_types().filter(|(k, _, _)| !import.types.contains_key(*k));
                state.add_terms(terms);
                state.add_types(types);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElaboratedImportItems {
    pub file: FileId,
    pub kind: ImportKind,
    pub exported: bool,
    terms: FxHashMap<SmolStr, (FileId, TermItemId)>,
    types: FxHashMap<SmolStr, (FileId, TypeItemId)>,
}

impl ElaboratedImportItems {
    fn new(file: FileId, exported: bool, kind: ImportKind) -> ElaboratedImportItems {
        let terms = FxHashMap::default();
        let types = FxHashMap::default();
        ElaboratedImportItems { file, kind, exported, terms, types }
    }

    pub fn iter_terms(&self) -> impl Iterator<Item = (&str, FileId, TermItemId)> {
        self.terms.iter().map(|(k, (f, t))| (k.as_str(), *f, *t))
    }

    pub fn iter_types(&self) -> impl Iterator<Item = (&str, FileId, TypeItemId)> {
        self.types.iter().map(|(k, (f, t))| (k.as_str(), *f, *t))
    }
}

fn elaborate_import_items(
    external: &mut impl External,
    state: &mut State,
    items: &ImportItems,
) -> Option<ElaboratedImportItems> {
    let Some(name) = &items.name else { return None };

    let import = external.file_id(name);
    let mut elaborated = ElaboratedImportItems::new(import, items.exported, items.kind);

    let index = external.index(import);
    let exports = external.exports(import);

    for (name, &id) in &items.terms {
        if let Some((term_file, term_id)) = exports.terms.get(name) {
            let name = SmolStr::clone(name);
            elaborated.terms.insert(name, (*term_file, *term_id));
        } else {
            state.errors.push(Error::InvalidImport { id });
        }
    }

    for (name, &(id, ref implicit)) in &items.types {
        if let Some((type_file, type_id)) = exports.types.get(name) {
            let name = SmolStr::clone(name);
            elaborated.types.insert(name, (*type_file, *type_id));
        } else {
            state.errors.push(Error::InvalidImport { id });
        }
        match implicit {
            Some(ImplicitItems::Everything) => {
                if let Some((_, type_id)) = exports.types.get(name) {
                    for term_id in index.relational.constructors_of(*type_id) {
                        if let Some((name, (term_file, term_id))) =
                            exports.terms.iter().find(|(_, (_, id))| *id == term_id)
                        {
                            let name = SmolStr::clone(name);
                            elaborated.terms.insert(name, (*term_file, *term_id));
                        }
                    }
                } else {
                    state.errors.push(Error::InvalidImport { id });
                }
            }
            Some(ImplicitItems::Enumerated(names)) => {
                for name in names.iter() {
                    if let Some((term_file, term_id)) = exports.terms.get(name) {
                        let name = SmolStr::clone(name);
                        elaborated.terms.insert(name, (*term_file, *term_id));
                    } else {
                        state.errors.push(Error::InvalidImport { id });
                    }
                }
            }
            None => {}
        }
    }

    Some(elaborated)
}
