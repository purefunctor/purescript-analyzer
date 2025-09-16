pub use building::QueryError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum AnalyzerError {
    #[error("Non-fatal error")]
    NonFatal,
    #[error("QueryError: {0}")]
    QueryError(#[from] QueryError),
    #[error("UrlParseError: {0}")]
    UrlParseError(#[from] url::ParseError),
}
