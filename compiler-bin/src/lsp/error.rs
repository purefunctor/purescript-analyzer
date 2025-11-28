use std::path::PathBuf;
use std::{io, str};

use analyzer::{AnalyzerError, QueryError};
use async_lsp::ErrorCode;
use spago::LockfileGlobSetError;
use thiserror::Error;
use tokio::task;

#[derive(Error, Debug)]
pub enum LspError {
    #[error("AnalyzerError: {0}")]
    AnalyzerError(#[from] AnalyzerError),
    #[error("QueryError: {0}")]
    QueryError(#[from] QueryError),
    #[error("Failed to parse file {0}")]
    PathParseFail(PathBuf),
    #[error("Invalid or missing workspace root")]
    MissingRoot,
    #[error("Invalid or missing --source-command")]
    InvalidSourceCommand,
    #[error("SpagoError: {0}")]
    SpagoLock(#[from] LockfileGlobSetError),
    #[error("IoError: {0}")]
    IoError(#[from] io::Error),
    #[error("JoinError: {0}")]
    JoinError(#[from] task::JoinError),
    #[error("Utf8Error: {0}")]
    Utf8Error(#[from] str::Utf8Error),
    #[error("GlobSetError: {0}")]
    GlobSetError(#[from] globset::Error),
}

impl LspError {
    #[inline]
    fn as_query_error(&self) -> Option<&QueryError> {
        match self {
            LspError::AnalyzerError(AnalyzerError::QueryError(q)) => Some(q),
            LspError::QueryError(q) => Some(q),
            _ => None,
        }
    }

    pub fn code(&self) -> ErrorCode {
        if let Some(QueryError::Cancelled) = self.as_query_error() {
            return ErrorCode::REQUEST_CANCELLED;
        }
        ErrorCode::REQUEST_FAILED
    }

    pub fn message(&self) -> &str {
        if let Some(QueryError::Cancelled) = self.as_query_error() {
            return "Request cancelled";
        }
        "Request failed"
    }

    pub fn emit_trace(&self) {
        if let Some(QueryError::Cancelled) = self.as_query_error() {
            tracing::warn!("{self}")
        } else {
            tracing::error!("{self}")
        }
    }
}

pub trait AnalyzerResultExt<T> {
    /// Convenience method for handling an [`AnalyzerError::NonFatal`]
    /// error, turning it into [`Result::Ok`] with the given item.
    fn on_non_fatal(self, item: T) -> Result<T, LspError>;
}

impl<T> AnalyzerResultExt<T> for Result<T, AnalyzerError> {
    fn on_non_fatal(self, item: T) -> Result<T, LspError> {
        self.or_else(|error| match error {
            AnalyzerError::NonFatal => Ok(item),
            _ => Err(LspError::from(error)),
        })
    }
}
