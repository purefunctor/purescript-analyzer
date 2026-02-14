use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompatError {
    #[error("io: {0}")]
    Io(#[from] std::io::Error),

    #[error("git: {0}")]
    Git(#[from] git2::Error),

    #[error("http: {0}")]
    Http(#[from] reqwest::Error),

    #[error("semver: {0}")]
    Semver(#[from] semver::Error),

    #[error("registry: {0}")]
    Registry(#[from] registry::RegistryError),

    #[error("package set missing package: {0}")]
    MissingFromPackageSet(String),

    #[error("hash mismatch for {name}@{version}")]
    HashMismatch { name: String, version: String },

    #[error("manifest not found for {name}@{version}")]
    ManifestNotFound { name: String, version: String },

    #[error("{0}")]
    Other(String),
}

pub type Result<T> = std::result::Result<T, CompatError>;
