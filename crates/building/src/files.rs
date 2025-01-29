use core::str;
use std::sync::Arc;

use id::Id;
use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;

#[derive(Default)]
pub struct Files {
    files: IndexMap<Arc<str>, Arc<str>, FxBuildHasher>,
}

pub struct File;

pub type FileId = Id<File>;

impl Files {
    pub fn insert(&mut self, k: impl Into<Arc<str>>, v: impl Into<Arc<str>>) -> FileId {
        let k = k.into();
        let v = v.into();
        let (index, _) = self.files.insert_full(k, v);
        Id::from_raw(index)
    }

    pub fn id(&self, k: &str) -> Option<FileId> {
        self.files.get_full(k).map(|(index, _, _)| Id::from_raw(index))
    }

    pub fn path(&self, id: FileId) -> Arc<str> {
        let index = id.into();
        let (path, _) = self.files.get_index(index).expect("valid FileId");
        Arc::clone(path)
    }

    pub fn content(&self, id: FileId) -> Arc<str> {
        let index = id.into();
        let (_, contents) = self.files.get_index(index).expect("valid FileId");
        Arc::clone(contents)
    }
}

#[cfg(test)]
mod tests {
    use super::Files;

    #[test]
    fn test_basic() {
        let mut files = Files::default();

        let k = "src/Main.purs";
        let v = "module Main where\n\n";

        let id = files.insert(k, v);

        assert_eq!(files.id(k), Some(id));
        assert_eq!(files.path(id).as_ref(), k);
        assert_eq!(files.content(id).as_ref(), v);
    }
}
