use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct Layout {
    pub root: PathBuf,
    pub repos_dir: PathBuf,
    pub registry_dir: PathBuf,
    pub index_dir: PathBuf,
    pub cache_tarballs_dir: PathBuf,
    pub packages_dir: PathBuf,
}

impl Layout {
    pub fn new(root: impl AsRef<Path>) -> Self {
        let root = root.as_ref().to_path_buf();
        let repos_dir = root.join("repos");
        let registry_dir = repos_dir.join("registry");
        let index_dir = repos_dir.join("registry-index");
        let cache_tarballs_dir = root.join("cache").join("tarballs");
        let packages_dir = root.join("packages");

        Self { root, repos_dir, registry_dir, index_dir, cache_tarballs_dir, packages_dir }
    }

    pub fn package_sets_dir(&self) -> PathBuf {
        self.registry_dir.join("package-sets")
    }

    pub fn metadata_dir(&self) -> PathBuf {
        self.registry_dir.join("metadata")
    }

    pub fn index_path(&self, name: &str) -> PathBuf {
        let chars: Vec<char> = name.chars().collect();
        match chars.len() {
            1 => self.index_dir.join("1").join(name),
            2 => self.index_dir.join("2").join(name),
            3 => {
                let first_char: String = chars[..1].iter().collect();
                self.index_dir.join("3").join(first_char).join(name)
            }
            _ => {
                let first_two: String = chars[..2].iter().collect();
                let second_two: String = chars[2..4].iter().collect();
                self.index_dir.join(first_two).join(second_two).join(name)
            }
        }
    }

    pub fn tarball_cache_path(&self, name: &str, version: &str) -> PathBuf {
        self.cache_tarballs_dir.join(format!("{}-{}.tar.gz", name, version))
    }

    pub fn package_dir(&self, name: &str, version: &str) -> PathBuf {
        self.packages_dir.join(format!("{}-{}", name, version))
    }
}
