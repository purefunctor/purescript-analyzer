use serde::Deserialize;
use std::{collections::HashMap, path::PathBuf};

#[derive(Debug, Clone, Deserialize)]
pub struct PackageSet {
    pub version: String,
    pub compiler: String,
    pub published: String,
    #[serde(flatten)]
    pub packages: PackageSetPackages,
}

#[derive(Debug, Clone, Deserialize)]
pub struct PackageSetPackages {
    pub packages: HashMap<String, String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Manifest {
    pub name: String,
    pub version: semver::Version,
    pub license: String,
    #[serde(default)]
    pub description: Option<String>,
    pub location: Location,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Location {
    GitHub {
        #[serde(rename = "githubOwner")]
        owner: String,
        #[serde(rename = "githubRepo")]
        repo: String,
        subdir: Option<String>,
    },
    Git {
        #[serde(rename = "gitUrl")]
        url: String,
        subdir: Option<String>,
    },
}

#[derive(Debug, Clone, Deserialize)]
pub struct Metadata {
    pub location: Location,
    pub owners: Option<Vec<String>>,
    pub published: HashMap<String, PublishedVersion>,
    #[serde(default)]
    pub unpublished: HashMap<String, UnpublishedVersion>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct PublishedVersion {
    pub hash: String,
    pub ref_: Option<String>,
    pub bytes: Option<u64>,
    #[serde(rename = "publishedTime")]
    pub published_time: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct UnpublishedVersion {
    pub ref_: Option<String>,
    pub reason: String,
    #[serde(rename = "publishedTime")]
    pub published_time: String,
    #[serde(rename = "unpublishedTime")]
    pub unpublished_time: String,
}

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
