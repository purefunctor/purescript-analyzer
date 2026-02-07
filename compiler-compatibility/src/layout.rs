use std::path::{Path, PathBuf};

use purescript_registry::RegistryLayout;

/// Layout for compiler-compatibility tool directories.
///
/// Includes paths for repository checkouts, tarball cache, and unpacked packages.
#[derive(Debug, Clone)]
pub struct Layout {
    pub repos_dir: PathBuf,
    pub registry_dir: PathBuf,
    pub index_dir: PathBuf,
    pub cache_tarballs_dir: PathBuf,
    pub packages_dir: PathBuf,
}

impl Layout {
    pub fn new(root: impl AsRef<Path>) -> Layout {
        let root = root.as_ref().to_path_buf();
        let repos_dir = root.join("repos");
        let registry_dir = repos_dir.join("registry");
        let index_dir = repos_dir.join("registry-index");
        let cache_tarballs_dir = root.join("cache").join("tarballs");
        let packages_dir = root.join("packages");

        Layout { repos_dir, registry_dir, index_dir, cache_tarballs_dir, packages_dir }
    }

    pub fn registry_layout(&self) -> RegistryLayout {
        RegistryLayout::new(&self.registry_dir, &self.index_dir)
    }

    pub fn tarball_cache_path(&self, name: &str, version: &str) -> PathBuf {
        self.cache_tarballs_dir.join(format!("{}-{}.tar.gz", name, version))
    }
}
