use std::path::PathBuf;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum VerifierError {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Registry(#[from] registry::RegistryError),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Http(#[from] reqwest::Error),
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error("failed to convert path to file URL: {0}")]
    FileUrl(PathBuf),
    #[error("archive entry escapes extraction directory: {0}")]
    UnsafeArchivePath(PathBuf),
}
