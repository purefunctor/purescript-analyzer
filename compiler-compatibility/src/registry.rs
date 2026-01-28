use std::{fs, io::BufRead};

use git2::{build::RepoBuilder, FetchOptions, Repository};

use crate::{
    error::{CompatError, Result},
    layout::Layout,
    types::{Manifest, Metadata, PackageSet},
};

const REGISTRY_URL: &str = "https://github.com/purescript/registry";
const REGISTRY_INDEX_URL: &str = "https://github.com/purescript/registry-index";

pub trait RegistryReader {
    fn list_package_sets(&self) -> Result<Vec<String>>;
    fn read_package_set(&self, version: Option<&str>) -> Result<PackageSet>;
    fn read_manifest_versions(&self, name: &str) -> Result<Vec<Manifest>>;
    fn read_metadata(&self, name: &str) -> Result<Metadata>;
}

pub struct Registry {
    layout: Layout,
}

impl Registry {
    pub fn new(layout: Layout) -> Self {
        Self { layout }
    }

    pub fn ensure_repos(&self, update: bool) -> Result<()> {
        fs::create_dir_all(&self.layout.repos_dir)?;

        self.ensure_repo(REGISTRY_URL, &self.layout.registry_dir, update)?;
        self.ensure_repo(REGISTRY_INDEX_URL, &self.layout.index_dir, update)?;

        Ok(())
    }

    fn ensure_repo(&self, url: &str, path: &std::path::Path, update: bool) -> Result<()> {
        if path.exists() {
            if update {
                let repo = Repository::open(path)?;
                let mut remote = repo.find_remote("origin")?;
                remote.fetch(&["master"], None, None)?;

                let fetch_head = repo.find_reference("FETCH_HEAD")?;
                let commit = repo.reference_to_annotated_commit(&fetch_head)?;
                let (analysis, _) = repo.merge_analysis(&[&commit])?;

                if analysis.is_fast_forward() || analysis.is_normal() {
                    let refname = "refs/heads/master";
                    let mut reference = repo.find_reference(refname)?;
                    reference.set_target(commit.id(), "pull: fast-forward")?;
                    repo.set_head(refname)?;
                    repo.checkout_head(Some(git2::build::CheckoutBuilder::default().force()))?;
                }
            }
        } else {
            let mut fetch_opts = FetchOptions::new();
            fetch_opts.depth(1);

            RepoBuilder::new().fetch_options(fetch_opts).clone(url, path)?;
        }

        Ok(())
    }

    pub fn layout(&self) -> &Layout {
        &self.layout
    }
}

impl RegistryReader for Registry {
    fn list_package_sets(&self) -> Result<Vec<String>> {
        let package_sets_dir = self.layout.package_sets_dir();
        let mut versions: Vec<semver::Version> = Vec::new();

        for entry in fs::read_dir(&package_sets_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().is_some_and(|ext| ext == "json") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    if let Ok(version) = stem.parse::<semver::Version>() {
                        versions.push(version);
                    }
                }
            }
        }

        if versions.is_empty() {
            return Err(CompatError::NoPackageSets);
        }

        versions.sort_by(|a, b| b.cmp(a));

        Ok(versions.into_iter().map(|v| v.to_string()).collect())
    }

    fn read_package_set(&self, version: Option<&str>) -> Result<PackageSet> {
        let version = match version {
            Some(v) => v.to_string(),
            None => self.list_package_sets()?.into_iter().next().ok_or(CompatError::NoPackageSets)?,
        };

        let path = self.layout.package_sets_dir().join(format!("{}.json", version));

        if !path.exists() {
            return Err(CompatError::PackageSetNotFound(version));
        }

        let content = fs::read_to_string(&path)?;
        let package_set: PackageSet = serde_json::from_str(&content)?;

        Ok(package_set)
    }

    fn read_manifest_versions(&self, name: &str) -> Result<Vec<Manifest>> {
        let path = self.layout.index_path(name);
        let file = fs::File::open(&path)?;
        let reader = std::io::BufReader::new(file);

        let mut manifests = Vec::new();
        for line in reader.lines() {
            let line = line?;
            if !line.is_empty() {
                let manifest: Manifest = serde_json::from_str(&line)?;
                manifests.push(manifest);
            }
        }

        Ok(manifests)
    }

    fn read_metadata(&self, name: &str) -> Result<Metadata> {
        let path = self.layout.metadata_dir().join(format!("{}.json", name));
        let content = fs::read_to_string(&path)?;
        let metadata: Metadata = serde_json::from_str(&content)?;

        Ok(metadata)
    }
}
