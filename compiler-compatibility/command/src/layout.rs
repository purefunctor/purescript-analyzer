use std::path::{Path, PathBuf};

use registry::RegistryLayout;

/// Layout for compiler-compatibility tool directories.
///
/// Includes paths for repository checkouts, tarball cache, and unpacked packages.
#[derive(Debug, Clone)]
pub struct Layout {
    pub registry: RegistryLayout,
    pub cache_tarballs: PathBuf,
    pub packages: PathBuf,
}

impl Layout {
    pub fn new(root: impl AsRef<Path>) -> Layout {
        let root = root.as_ref().to_path_buf();
        let repos_dir = root.join("repos");
        let registry_dir = repos_dir.join("registry");
        let index_dir = repos_dir.join("registry-index");
        let cache_tarballs = root.join("cache").join("tarballs");
        let packages = root.join("packages");

        Layout {
            registry: RegistryLayout::new(&registry_dir, &index_dir),
            cache_tarballs,
            packages,
        }
    }

    pub fn tarball_cache_path(&self, name: &str, version: &str) -> PathBuf {
        self.cache_tarballs.join(format!("{}-{}.tar.gz", name, version))
    }
}
