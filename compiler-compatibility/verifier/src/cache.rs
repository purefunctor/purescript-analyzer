use std::fs;
use std::path::{Path, PathBuf};

use flate2::read::GzDecoder;
use tar::Archive;

use crate::error::VerifierError;

pub struct PackageCache {
    root: PathBuf,
}

impl PackageCache {
    pub fn new(root: PathBuf) -> PackageCache {
        PackageCache { root }
    }

    pub fn download_path(&self, package: &str, version: &str) -> PathBuf {
        self.root.join("downloads").join(format!("{package}-{version}.tar.gz"))
    }

    pub fn source_path(&self, package: &str, version: &str) -> PathBuf {
        self.root.join("sources").join(package).join(version)
    }

    pub fn ensure_package(&self, package: &str, version: &str) -> Result<PathBuf, VerifierError> {
        let download_path = self.download_path(package, version);
        if !download_path.exists() {
            fs::create_dir_all(download_path.parent().expect("download path has parent"))?;
            let url =
                format!("https://packages.registry.purescript.org/{package}/{version}.tar.gz");
            let bytes = reqwest::blocking::get(url)?.error_for_status()?.bytes()?;
            fs::write(&download_path, bytes)?;
        }

        let source_path = self.source_path(package, version);
        let stamp = source_path.join(".tarball-size");
        let tarball_size = fs::metadata(&download_path)?.len().to_string();
        let current_stamp = fs::read_to_string(&stamp).ok();

        if current_stamp.as_deref() != Some(tarball_size.as_str()) {
            if source_path.exists() {
                fs::remove_dir_all(&source_path)?;
            }
            fs::create_dir_all(&source_path)?;
            extract_tarball(&download_path, &source_path)?;
            fs::write(stamp, tarball_size)?;
        }

        Ok(source_path)
    }
}

fn extract_tarball(tarball: &Path, destination: &Path) -> Result<(), VerifierError> {
    let file = fs::File::open(tarball)?;
    let decoder = GzDecoder::new(file);
    let mut archive = Archive::new(decoder);

    for entry in archive.entries()? {
        let mut entry = entry?;
        let path = entry.path()?.into_owned();
        let stripped = path.components().skip(1).collect::<PathBuf>();
        if stripped.as_os_str().is_empty() {
            continue;
        }
        if stripped
            .components()
            .any(|component| matches!(component, std::path::Component::ParentDir))
        {
            return Err(VerifierError::UnsafeArchivePath(path));
        }
        entry.unpack(destination.join(stripped))?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::PackageCache;

    #[test]
    fn deterministic_cache_paths() {
        let cache = PackageCache::new("target/compatibility".into());
        assert_eq!(
            cache.download_path("prelude", "6.0.2").to_string_lossy(),
            "target/compatibility/downloads/prelude-6.0.2.tar.gz"
        );
        assert_eq!(
            cache.source_path("prelude", "6.0.2").to_string_lossy(),
            "target/compatibility/sources/prelude/6.0.2"
        );
    }
}
