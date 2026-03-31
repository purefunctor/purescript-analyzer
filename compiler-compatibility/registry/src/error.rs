use thiserror::Error;

#[derive(Error, Debug)]
pub enum RegistryError {
    #[error("io: {0}")]
    Io(#[from] std::io::Error),

    #[error("json: {0}")]
    Json(#[from] serde_json::Error),

    #[error("semver: {0}")]
    Semver(#[from] semver::Error),

    #[error("no package sets found")]
    NoPackageSets,

    #[error("package set not found: {0}")]
    PackageSetNotFound(String),
}

pub type Result<T> = std::result::Result<T, RegistryError>;
