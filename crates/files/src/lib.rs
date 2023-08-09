//! A virtual file system.
//!
//! This crate is adapted from rust-analyzer's `vfs` crate.
//!
//! In summary, this crate implements a virtual file system, [`Files`], used
//! to store the contents of the source files currently being analyzed. This
//! crate does not implement IO operations nor does it integrate directly
//! into the `salsa` database for the analyzer.
//!
//! Instead, the [`set_file_contents`] method handles file creation, deletion,
//! and modification inside the virtual file system. Likewise, changes are
//! logged and can be seen using the [`take_changes`] method.
//!
//! [`set_file_contents`]: Files::set_file_contents
//! [`take_changes`]: Files::take_changes

mod interner;

use std::{mem, path::PathBuf};

use interner::Interner;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Change {
    Created,
    Deleted,
    Modified,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChangedFile {
    pub file_id: FileId,
    pub change: Change,
}

#[derive(Default)]
pub struct Files {
    interner: Interner,
    files: Vec<Option<Vec<u8>>>,
    changes: Vec<ChangedFile>,
}

impl Files {
    /// Returns the [`FileID`] for a `path`.
    pub fn file_id(&self, path: PathBuf) -> Option<FileId> {
        self.interner.get(path).filter(|&file_id| self.get(file_id).is_some())
    }

    /// Returns the file path given a `file_id`.
    pub fn file_path(&self, file_id: FileId) -> PathBuf {
        self.interner.lookup(file_id).clone()
    }

    /// Returns the file contents given a `file_id`.
    pub fn file_contents(&self, file_id: FileId) -> &[u8] {
        self.get(file_id).as_deref().unwrap()
    }

    /// Sets the contents for a given `path`, with `None` meaning deletion.
    ///
    /// Returns `true` if the file was modified and saves the [change](ChangedFile).
    ///
    /// Allocates a new `FileId` if the path does not currently exist.
    pub fn set_file_contents(&mut self, path: PathBuf, mut contents: Option<Vec<u8>>) -> bool {
        let file_id = self.allocate_file_id(path);
        let change = match (self.get(file_id), &contents) {
            (None, None) => return false,
            (Some(q), Some(p)) if q == p => return false,
            (None, Some(_)) => Change::Created,
            (Some(_), None) => Change::Deleted,
            (Some(_), Some(_)) => Change::Modified,
        };
        if let Some(contents) = &mut contents {
            contents.shrink_to_fit();
        }
        *self.get_mut(file_id) = contents;
        self.changes.push(ChangedFile { file_id, change });
        true
    }

    /// Returns `true` if the file system logged changes.
    pub fn has_changes(&self) -> bool {
        !self.changes.is_empty()
    }

    /// Takes all changes logged so far by the file system.
    pub fn take_changes(&mut self) -> Vec<ChangedFile> {
        mem::take(&mut self.changes)
    }

    fn allocate_file_id(&mut self, path: PathBuf) -> FileId {
        let file_id = self.interner.intern(path);
        let idx = file_id.0 as usize;
        let len = self.files.len().max(idx + 1);
        self.files.resize_with(len, || None);
        file_id
    }

    fn get(&self, file_id: FileId) -> &Option<Vec<u8>> {
        &self.files[file_id.0 as usize]
    }

    fn get_mut(&mut self, file_id: FileId) -> &mut Option<Vec<u8>> {
        &mut self.files[file_id.0 as usize]
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{Change, ChangedFile, Files};

    #[test]
    fn api() {
        let mut files = Files::default();

        let path = PathBuf::from("./src/Main.purs");

        // Identity

        assert!(!files.set_file_contents(path.clone(), None));

        // Creation

        let contents = "module Main where".as_bytes();

        assert!(files.set_file_contents(path.clone(), Some(contents.to_vec())));
        let changes = files.take_changes();

        let file_id = files.file_id(path.clone()).unwrap();
        let file_contents = files.file_contents(file_id);

        assert_eq!(changes, &[ChangedFile { file_id, change: Change::Created }]);
        assert_eq!(contents, file_contents);

        // Identity
        assert!(!files.set_file_contents(path.clone(), Some(contents.to_vec())));

        // Modification

        let contents = "module Hello where".as_bytes();

        assert!(files.set_file_contents(path.clone(), Some(contents.to_vec())));
        let changes = files.take_changes();

        let file_contents = files.file_contents(file_id);

        assert_eq!(changes, &[ChangedFile { file_id, change: Change::Modified }]);
        assert_eq!(contents, file_contents);

        // Deletion

        assert!(files.set_file_contents(path.clone(), None));
        let changes = files.take_changes();
        assert_eq!(changes, &[ChangedFile { file_id, change: Change::Deleted }]);
    }
}
