use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct ResolvedPackage {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Clone, Default)]
pub struct ResolvedSet {
    pub packages: std::collections::BTreeMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct CompatConfig {
    pub root: PathBuf,
    pub no_cache: bool,
    pub update_repos: bool,
    pub package_set: Option<String>,
}

impl Default for CompatConfig {
    fn default() -> Self {
        Self {
            root: PathBuf::from("target/compiler-compatibility"),
            no_cache: false,
            update_repos: false,
            package_set: None,
        }
    }
}
