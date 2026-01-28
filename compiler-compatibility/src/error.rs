use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompatError {
    #[error("io: {0}")]
    Io(#[from] std::io::Error),

    #[error("json: {0}")]
    Json(#[from] serde_json::Error),

    #[error("git: {0}")]
    Git(#[from] git2::Error),

    #[error("http: {0}")]
    Http(#[from] reqwest::Error),

    #[error("semver: {0}")]
    Semver(#[from] semver::Error),

    #[error("package set missing package: {0}")]
    MissingFromPackageSet(String),

    #[error("version mismatch for {name}: package-set {set_version} not in range {range}")]
    VersionMismatch { name: String, set_version: String, range: String },

    #[error("hash mismatch for {name}@{version}")]
    HashMismatch { name: String, version: String },

    #[error("manifest not found for {name}@{version}")]
    ManifestNotFound { name: String, version: String },

    #[error("no package sets found")]
    NoPackageSets,

    #[error("package set not found: {0}")]
    PackageSetNotFound(String),

    #[error("{0}")]
    Other(String),
}

pub type Result<T> = std::result::Result<T, CompatError>;
