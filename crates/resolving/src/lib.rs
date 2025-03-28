use std::{collections::hash_map::Entry, sync::Arc};

use files::FileId;
use indexing::{FullModuleIndex, Index, TermItemId, TypeItemId};
use rustc_hash::FxHashMap;
use smol_str::SmolStr;

pub trait External {
    fn index(&mut self, id: FileId) -> Arc<FullModuleIndex>;
}

pub fn exports(external: &mut impl External, file: FileId) {
    let mut state = State::default();
    let index = &external.index(file).index;
    match index.export_kind {
        indexing::ExportKind::Implicit => {
            implicit_module_exports(&mut state, file, index);
        }
        indexing::ExportKind::Explicit => {
            explicit_module_exports(&mut state, file, index, false);
        }
        indexing::ExportKind::ExplicitSelf => {
            explicit_module_exports(&mut state, file, index, true);
        }
    }
    dbg!(&state);
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    ExistingTerm { existing: (FileId, TermItemId), duplicate: (FileId, TermItemId) },
    ExistingType { existing: (FileId, TypeItemId), duplicate: (FileId, TypeItemId) },
}

#[derive(Debug, Default)]
struct State {
    errors: Vec<Error>,
    terms: FxHashMap<SmolStr, (FileId, TermItemId)>,
    types: FxHashMap<SmolStr, (FileId, TypeItemId)>,
}

impl State {
    fn add_term(&mut self, name: &str, file: FileId, id: TermItemId) {
        let k = SmolStr::from(name);
        match self.terms.entry(k) {
            Entry::Occupied(o) => {
                let existing = *o.get();
                let duplicate = (file, id);
                self.errors.push(Error::ExistingTerm { existing, duplicate });
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
                self.errors.push(Error::ExistingType { existing, duplicate });
            }
            Entry::Vacant(v) => {
                v.insert((file, id));
            }
        }
    }
}

fn implicit_module_exports(state: &mut State, file: FileId, index: &Index) {
    for (name, id) in index.iter_term_nominal() {
        state.add_term(name, file, id);
    }
    for (name, id) in index.iter_type_nominal() {
        state.add_type(name, file, id);
    }
}

fn explicit_module_exports(state: &mut State, file: FileId, index: &Index, including_self: bool) {
    if including_self {
        implicit_module_exports(state, file, index);
    }
}
