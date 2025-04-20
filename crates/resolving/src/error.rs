use files::FileId;
use indexing::{ImportId, ImportItemId, TermItemId, TypeItemId};

/// The kind of errors produced during name resolution.
#[derive(Debug, PartialEq, Eq)]
pub enum ResolvingError {
    ExistingTerm { existing: (FileId, TermItemId), duplicate: (FileId, TermItemId) },
    ExistingType { existing: (FileId, TypeItemId), duplicate: (FileId, TypeItemId) },
    InvalidImportStatement { id: ImportId },
    InvalidImportItem { id: ImportItemId },
    DuplicateImportItem { id: ImportItemId },
}
