pub mod exports;
pub use exports::*;

use std::sync::Arc;

use files::FileId;
use indexing::{FullModuleIndex, TermItemId, TypeItemId};

/// External dependencies used in name resolution.
pub trait External {
    fn index(&mut self, id: FileId) -> Arc<FullModuleIndex>;

    fn exports(&mut self, id: FileId) -> Arc<FullModuleExports>;
}

/// The kind of errors produced during name resolution.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    ExistingTerm { existing: (FileId, TermItemId), duplicate: (FileId, TermItemId) },
    ExistingType { existing: (FileId, TypeItemId), duplicate: (FileId, TypeItemId) },
}
