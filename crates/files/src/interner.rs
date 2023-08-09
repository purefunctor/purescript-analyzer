use std::{hash::BuildHasherDefault, path::PathBuf};

use indexmap::IndexSet;
use rustc_hash::FxHasher;

#[derive(Default)]
pub(crate) struct Interner {
    paths: IndexSet<PathBuf, BuildHasherDefault<FxHasher>>,
}

impl Interner {
    pub(crate) fn intern(&mut self, path: PathBuf) -> super::FileId {
        let (id, _) = self.paths.insert_full(path);
        assert!(id < u32::MAX as usize);
        super::FileId(id as u32)
    }

    pub(crate) fn get(&self, path: PathBuf) -> Option<super::FileId> {
        self.paths.get_index_of(&path).map(|i| super::FileId(i as u32))
    }

    pub(crate) fn lookup(&self, file_id: super::FileId) -> &PathBuf {
        self.paths.get_index(file_id.0 as usize).unwrap()
    }
}
