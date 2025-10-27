use std::{io, path::PathBuf, str};

use analyzer::{AnalyzerError, QueryError};
use async_lsp::{ErrorCode, ResponseError};
use spago::LockfileGlobSetError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LspError {
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
    #[error("Utf8Error: {0}")]
    Utf8Error(#[from] str::Utf8Error),
    #[error("GlobSetError: {0}")]
    GlobSetError(#[from] globset::Error),
}

pub trait IntoResponseError {
    fn into_response_error(self) -> ResponseError;
}

impl IntoResponseError for QueryError {
    fn into_response_error(self) -> ResponseError {
        match self {
            QueryError::Cancelled => {
                tracing::warn!("{self}");
                ResponseError::new(ErrorCode::REQUEST_CANCELLED, "Request cancelled")
            }
            QueryError::Cycle { .. } => {
                tracing::error!("{self}");
                ResponseError::new(ErrorCode::REQUEST_FAILED, "Request failed")
            }
        }
    }
}

impl IntoResponseError for AnalyzerError {
    fn into_response_error(self) -> ResponseError {
        match self {
            AnalyzerError::NonFatal => {
                tracing::warn!("{self}");
                ResponseError::new(ErrorCode::REQUEST_CANCELLED, "Request cancelled")
            }
            AnalyzerError::QueryError(error) => error.into_response_error(),
            AnalyzerError::UrlParseError(_) => {
                tracing::error!("{self}");
                ResponseError::new(ErrorCode::REQUEST_FAILED, "Request failed")
            }
        }
    }
}

impl IntoResponseError for LspError {
    fn into_response_error(self) -> ResponseError {
        match self {
            LspError::QueryError(error) => error.into_response_error(),
            _ => {
                tracing::error!("{self}");
                ResponseError::new(ErrorCode::REQUEST_FAILED, "Request failed")
            }
        }
    }
}

pub trait NonFatalResult<T> {
    /// Convenience method for handling an [`AnalyzerError::NonFatal`]
    /// error, turning it into [`Result::Ok`] with the given item.
    fn on_non_fatal(self, item: T) -> Result<T, ResponseError>;
}

impl<T> NonFatalResult<T> for Result<T, AnalyzerError> {
    fn on_non_fatal(self, item: T) -> Result<T, ResponseError> {
        self.or_else(|error| match error {
            AnalyzerError::NonFatal => Ok(item),
            _ => Err(error.into_response_error()),
        })
    }
}
