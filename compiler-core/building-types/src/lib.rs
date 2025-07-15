#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryError {
    Cancelled { cleanup: bool },
}

pub type QueryResult<T> = Result<T, QueryError>;
