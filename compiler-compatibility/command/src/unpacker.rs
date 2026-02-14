use std::fs::{self, File};
use std::path::{Path, PathBuf};

use flate2::read::GzDecoder;
use tar::Archive;

use crate::error::{CompatError, Result};

pub fn unpack_tarball(tarball: &Path, dest_dir: &Path) -> Result<PathBuf> {
    fs::create_dir_all(dest_dir)?;

    let file = File::open(tarball)?;
    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);

    for entry in archive.entries()? {
        let mut entry = entry?;
        let path = entry.path()?;

        if path.components().any(|c| c == std::path::Component::ParentDir) {
            return Err(CompatError::Other(format!("path traversal detected: {}", path.display())));
        }

        if path.is_absolute() {
            return Err(CompatError::Other(format!(
                "absolute path in archive: {}",
                path.display()
            )));
        }

        entry.unpack_in(dest_dir)?;
    }

    Ok(dest_dir.to_path_buf())
}
