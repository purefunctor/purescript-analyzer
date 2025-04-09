//! Implements name resolution for export lists.
use std::collections::hash_map::Entry;

use files::FileId;
use indexing::{ImplicitItems, ImportKind, Index, TermItemId, TypeItemId};
use rustc_hash::{FxHashMap, FxHashSet};
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

    for (_, items) in index.iter_import_items() {
        if !items.exported {
            continue;
        }
        let Some(name) = &items.name else {
            continue;
        };

        let import = external.file_id(name);
        let index = external.index(import);
        let exports = external.exports(import);

        match items.kind {
            ImportKind::Implicit => {
                let terms = exports.iter_terms();
                let types = exports.iter_types();
                state.add_terms(terms);
                state.add_types(types);
            }
            ImportKind::Explicit => {
                for (name, &id) in items.terms.iter() {
                    if let Some((term_file, term_id)) = exports.terms.get(name) {
                        state.add_term(name, *term_file, *term_id);
                    } else {
                        state.errors.push(Error::InvalidImport { id });
                    }
                }
                for (name, &(id, ref implicit)) in items.types.iter() {
                    if let Some((type_file, type_id)) = exports.types.get(name) {
                        state.add_type(name, *type_file, *type_id);
                        match implicit {
                            Some(ImplicitItems::Everything) => {
                                for term_id in index.relational.constructors_of(*type_id) {
                                    if let Some((name, (term_file, term_id))) =
                                        exports.terms.iter().find(|(_, (_, id))| *id == term_id)
                                    {
                                        state.add_term(name, *term_file, *term_id);
                                    } else {
                                        state.errors.push(Error::InvalidImport { id });
                                    }
                                }
                            }
                            Some(ImplicitItems::Enumerated(names)) => {
                                for name in names.iter() {
                                    if let Some((term_file, term_id)) = exports.terms.get(name) {
                                        state.add_term(name, *term_file, *term_id);
                                    } else {
                                        state.errors.push(Error::InvalidImport { id });
                                    }
                                }
                            }
                            None => {}
                        }
                    } else {
                        state.errors.push(Error::InvalidImport { id });
                    }
                }
            }
            ImportKind::Hidden => {
                let mut constructors = FxHashSet::default();
                for (name, (_, implicit)) in items.types.iter() {
                    match implicit {
                        Some(ImplicitItems::Everything) => {
                            if let Some((_, type_id)) = exports.types.get(name) {
                                for term_id in index.relational.constructors_of(*type_id) {
                                    if let Some((name, (term_file, term_id))) =
                                        exports.terms.iter().find(|(_, (_, id))| *id == term_id)
                                    {
                                        constructors.insert((name.as_str(), *term_file, *term_id));
                                    }
                                }
                            }
                        }
                        Some(ImplicitItems::Enumerated(names)) => {
                            for name in names.iter() {
                                if let Some((term_file, term_id)) = exports.terms.get(name) {
                                    constructors.insert((name.as_str(), *term_file, *term_id));
                                }
                            }
                        }
                        None => (),
                    }
                }

                for (name, file, id) in exports.iter_terms() {
                    if !constructors.contains(&(name, file, id)) && !items.terms.contains_key(name)
                    {
                        state.add_term(name, file, id);
                    }
                }
                for (name, file, id) in exports.iter_types() {
                    if !items.types.contains_key(name) {
                        state.add_type(name, file, id);
                    }
                }
            }
        }
    }
}
