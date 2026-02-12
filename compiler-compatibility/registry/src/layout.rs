use std::path::{Path, PathBuf};

/// Layout for PureScript registry and registry-index directories.
///
/// This represents the on-disk structure of the registry repositories,
/// independent of where they are located or how they are managed.
#[derive(Debug, Clone)]
pub struct RegistryLayout {
    pub registry_dir: PathBuf,
    pub index_dir: PathBuf,
}

impl RegistryLayout {
    pub fn new(registry_dir: impl AsRef<Path>, index_dir: impl AsRef<Path>) -> RegistryLayout {
        RegistryLayout {
            registry_dir: registry_dir.as_ref().to_path_buf(),
            index_dir: index_dir.as_ref().to_path_buf(),
        }
    }

    pub fn package_sets_dir(&self) -> PathBuf {
        self.registry_dir.join("package-sets")
    }

    pub fn metadata_dir(&self) -> PathBuf {
        self.registry_dir.join("metadata")
    }

    /// Returns the path to a package's manifest in the registry index.
    ///
    /// The index uses a sharding scheme based on package name length:
    /// - 1-char names: `1/{name}`
    /// - 2-char names: `2/{name}`
    /// - 3-char names: `3/{first-char}/{name}`
    /// - 4+ char names: `{first-two}/{chars-3-4}/{name}`
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
}
