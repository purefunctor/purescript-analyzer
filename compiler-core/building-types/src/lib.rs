pub mod module_name_map;
pub use module_name_map::*;

use files::FileId;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryKey {
    Content(FileId),
    Module(ModuleNameId),
    Parsed(FileId),
    Indexed(FileId),
    Lowered(FileId),
    Resolved(FileId),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryError {
    Cancelled,
    Cycle { stack: Arc<[QueryKey]> },
}

pub type QueryResult<T> = Result<T, QueryError>;
