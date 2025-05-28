pub mod lockfile;

use std::{
    env, fs, io,
    path::{Path, PathBuf},
};

use globset::{Glob, GlobSetBuilder};
use thiserror::Error;
use walkdir::WalkDir;

#[derive(Debug, Error)]
pub enum LockfileGlobSetError {
    #[error("{0}")]
    Io(#[from] io::Error),
    #[error("{0}")]
    Json(#[from] serde_json::Error),
    #[error("{0}")]
    Glob(#[from] globset::Error),
}

pub fn source_files() -> Result<Vec<PathBuf>, LockfileGlobSetError> {
    let cwd = env::current_dir()?;
    let lockfile = Path::new(&cwd).join("spago.lock");
    let lockfile = fs::read_to_string(lockfile)?;
    let lockfile: lockfile::Lockfile = serde_json::from_str(&lockfile)?;

    let mut builder = GlobSetBuilder::new();
    for source in lockfile.sources() {
        let source = Path::new(&cwd).join(source);
        let Some(source) = source.to_str() else { continue };
        let glob = Glob::new(source)?;
        builder.add(glob);
    }

    let globset = builder.build()?;

    Ok(WalkDir::new(&cwd)
        .into_iter()
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.is_dir() {
                return None;
            }
            if !globset.is_match(path) {
                return None;
            }
            Some(path.to_path_buf())
        })
        .collect())
}
