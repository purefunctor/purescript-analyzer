pub mod lockfile;

use std::{
    fs, io,
    path::{Path, PathBuf},
};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum LockfileGlobSetError {
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("{0}")]
    Json(#[from] serde_json::Error),
}

pub fn source_files(root: impl AsRef<Path>) -> Result<Vec<PathBuf>, LockfileGlobSetError> {
    let root = root.as_ref();
    let lockfile = root.join("spago.lock");
    let lockfile = fs::read_to_string(lockfile)?;
    let lockfile: lockfile::Lockfile = serde_json::from_str(&lockfile)?;
    Ok(lockfile.walk(root).collect())
}
