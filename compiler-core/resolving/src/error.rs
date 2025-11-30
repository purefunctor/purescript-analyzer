use files::FileId;
use indexing::{ImportId, ImportItemId, TermItemId, TypeItemId};

use crate::ExportSource;

/// The kind of errors produced during name resolution.
#[derive(Debug, PartialEq, Eq)]
pub enum ResolvingError {
    TermImportConflict {
        existing: (FileId, TermItemId, ImportId),
        duplicate: (FileId, TermItemId, ImportId),
    },
    TypeImportConflict {
        existing: (FileId, TypeItemId, ImportId),
        duplicate: (FileId, TypeItemId, ImportId),
    },

    TermExportConflict {
        existing: (FileId, TermItemId, ExportSource),
        duplicate: (FileId, TermItemId, ExportSource),
    },
    TypeExportConflict {
        existing: (FileId, TypeItemId, ExportSource),
        duplicate: (FileId, TypeItemId, ExportSource),
    },

    ExistingTerm {
        existing: (FileId, TermItemId),
        duplicate: (FileId, TermItemId),
    },
    ExistingType {
        existing: (FileId, TypeItemId),
        duplicate: (FileId, TypeItemId),
    },

    InvalidImportStatement {
        id: ImportId,
    },
    InvalidImportItem {
        id: ImportItemId,
    },
    DuplicateImportItem {
        id: ImportItemId,
    },
}
