pub mod module_name_map;
pub use module_name_map::*;

use std::sync::Arc;

use files::FileId;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryKey {
    Content(FileId),
    Module(ModuleNameId),
    Parsed(FileId),
    Stabilized(FileId),
    Indexed(FileId),
    Lowered(FileId),
    Resolved(FileId),
    Bracketed(FileId),
    Sectioned(FileId),
    Checked(FileId),
}

#[derive(Error, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryError {
    #[error("Cancelled")]
    Cancelled,
    #[error("Cycle detected")]
    Cycle { stack: Arc<[QueryKey]> },
}

pub type QueryResult<T> = Result<T, QueryError>;

pub trait QueryProxy {
    type Parsed;
    type Stabilized;
    type Indexed;
    type Lowered;
    type Resolved;
    type Bracketed;
    type Sectioned;
    type Checked;

    fn parsed(&self, id: FileId) -> QueryResult<Self::Parsed>;

    fn stabilized(&self, id: FileId) -> QueryResult<Self::Stabilized>;

    fn indexed(&self, id: FileId) -> QueryResult<Self::Indexed>;

    fn lowered(&self, id: FileId) -> QueryResult<Self::Lowered>;

    fn resolved(&self, id: FileId) -> QueryResult<Self::Resolved>;

    fn bracketed(&self, id: FileId) -> QueryResult<Self::Bracketed>;

    fn sectioned(&self, id: FileId) -> QueryResult<Self::Sectioned>;

    fn checked(&self, id: FileId) -> QueryResult<Self::Checked>;

    fn prim_id(&self) -> FileId;

    fn module_file(&self, name: &str) -> Option<FileId>;
}
