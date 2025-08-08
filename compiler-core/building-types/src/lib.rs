#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QueryError {
    Cancelled,
}

pub type QueryResult<T> = Result<T, QueryError>;
