use std::{fs, io::BufRead};

use crate::{
    error::{RegistryError, Result},
    layout::RegistryLayout,
    types::{Manifest, Metadata, PackageSet},
};

pub trait RegistryReader {
    fn list_package_sets(&self) -> Result<Vec<String>>;
    fn read_package_set(&self, version: Option<&str>) -> Result<PackageSet>;
    fn read_manifest_versions(&self, name: &str) -> Result<Vec<Manifest>>;
    fn read_metadata(&self, name: &str) -> Result<Metadata>;
}

pub struct FsRegistry {
    layout: RegistryLayout,
}

impl FsRegistry {
    pub fn new(layout: RegistryLayout) -> FsRegistry {
        FsRegistry { layout }
    }

    pub fn layout(&self) -> &RegistryLayout {
        &self.layout
    }
}

impl RegistryReader for FsRegistry {
    fn list_package_sets(&self) -> Result<Vec<String>> {
        let package_sets_dir = self.layout.package_sets_dir();
        let mut versions: Vec<semver::Version> = Vec::new();

        for entry in fs::read_dir(&package_sets_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().is_some_and(|ext| ext == "json")
                && let Some(stem) = path.file_stem().and_then(|s| s.to_str())
                && let Ok(version) = stem.parse::<semver::Version>()
            {
                versions.push(version);
            }
        }

        if versions.is_empty() {
            return Err(RegistryError::NoPackageSets);
        }

        versions.sort_by(|a, b| b.cmp(a));

        Ok(versions.into_iter().map(|v| v.to_string()).collect())
    }

    fn read_package_set(&self, version: Option<&str>) -> Result<PackageSet> {
        let version = match version {
            Some(v) => v.to_string(),
            None => {
                self.list_package_sets()?.into_iter().next().ok_or(RegistryError::NoPackageSets)?
            }
        };

        let path = self.layout.package_sets_dir().join(format!("{}.json", version));

        if !path.exists() {
            return Err(RegistryError::PackageSetNotFound(version));
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
