use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::path::Path;
use std::{fs, io};

use registry::{Manifest, PackageSet, RegistryError, RegistryReader};
use serde::{Deserialize, Serialize};

use crate::error::VerifierError;
use crate::report::VerifierIssue;

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum SelectionMode {
    Core,
    Packages,
    Combined,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct SelectedPackage {
    pub name: String,
    pub version: String,
}

#[derive(Debug)]
pub struct ResolvedSelection {
    pub mode: SelectionMode,
    pub packages: Vec<SelectedPackage>,
    pub errors: Vec<VerifierIssue>,
}

pub fn package_map(package_set: &PackageSet) -> HashMap<String, String> {
    package_set.packages.packages.clone()
}

pub fn normalize_core_package(name: &str) -> String {
    name.strip_prefix("purescript-").unwrap_or(name).to_string()
}

pub fn read_core_packages(path: &Path) -> Result<Vec<String>, VerifierError> {
    let content = fs::read_to_string(path)?;
    let names: Vec<String> = serde_json::from_str(&content)?;
    Ok(names.into_iter().map(|name| normalize_core_package(&name)).collect())
}

pub fn resolve_selection(
    registry: &impl RegistryReader,
    package_versions: &HashMap<String, String>,
    requested: &[String],
    include_core: bool,
) -> Result<ResolvedSelection, VerifierError> {
    let mut roots: BTreeSet<String> = requested.iter().cloned().collect();
    let mode = match (requested.is_empty(), include_core) {
        (true, false) | (true, true) => SelectionMode::Core,
        (false, false) => SelectionMode::Packages,
        (false, true) => SelectionMode::Combined,
    };

    if requested.is_empty() || include_core {
        let core_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(Path::parent)
            .expect("verifier crate is under compiler-compatibility/verifier")
            .join("tests-package-set/purescript-core.json");
        roots.extend(read_core_packages(&core_path)?);
    }

    let mut manifests: HashMap<String, Manifest> = HashMap::new();
    let mut selection = resolve_closure_with(roots, package_versions, |name| {
        if let Some(manifest) = manifests.get(name) {
            return Ok(Some(manifest.clone()));
        }

        let Some(version) = package_versions.get(name) else {
            return Ok(None);
        };

        let manifest = match registry.read_manifest_versions(name) {
            Ok(manifests) => {
                manifests.into_iter().find(|manifest| manifest.version.to_string() == *version)
            }
            Err(RegistryError::Io(error)) if error.kind() == io::ErrorKind::NotFound => None,
            Err(error) => return Err(error.into()),
        };
        if let Some(manifest) = &manifest {
            manifests.insert(name.to_string(), manifest.clone());
        }

        Ok(manifest)
    })?;
    selection.mode = mode;
    Ok(selection)
}

pub fn resolve_closure_with<F>(
    roots: BTreeSet<String>,
    package_versions: &HashMap<String, String>,
    mut manifest_for: F,
) -> Result<ResolvedSelection, VerifierError>
where
    F: FnMut(&str) -> Result<Option<Manifest>, VerifierError>,
{
    let mut selected = BTreeSet::new();
    let mut queue: VecDeque<String> = roots.into_iter().collect();
    let mut errors = Vec::new();

    while let Some(package) = queue.pop_front() {
        if !selected.insert(package.clone()) {
            continue;
        }

        let Some(version) = package_versions.get(&package) else {
            errors.push(VerifierIssue::missing_package(&package));
            continue;
        };

        let Some(manifest) = manifest_for(&package)? else {
            errors.push(VerifierIssue::missing_manifest(&package, version));
            continue;
        };

        for dependency in manifest.dependencies.keys() {
            if package_versions.contains_key(dependency) {
                queue.push_back(dependency.clone());
            } else {
                errors.push(VerifierIssue::missing_dependency(&package, dependency));
            }
        }
    }

    let ordered = selected
        .into_iter()
        .filter_map(|name| package_versions.get(&name).map(|version| (name, version.clone())))
        .collect::<BTreeMap<_, _>>()
        .into_iter()
        .map(|(name, version)| SelectedPackage { name, version })
        .collect();

    Ok(ResolvedSelection { mode: SelectionMode::Packages, packages: ordered, errors })
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeSet, HashMap};

    use registry::{Location, Manifest};

    use super::{normalize_core_package, resolve_closure_with};

    fn manifest(name: &str, version: &str, deps: &[&str]) -> Manifest {
        Manifest {
            name: name.to_string(),
            version: version.parse().unwrap(),
            license: "MIT".to_string(),
            description: None,
            location: Location::Git { url: "https://example.test".to_string(), subdir: None },
            dependencies: deps.iter().map(|dep| (dep.to_string(), "*".to_string())).collect(),
        }
    }

    #[test]
    fn normalizes_purescript_prefix() {
        assert_eq!(normalize_core_package("purescript-prelude"), "prelude");
        assert_eq!(normalize_core_package("effect"), "effect");
    }

    #[test]
    fn resolves_transitive_dependencies() {
        let versions = HashMap::from([
            ("effect".to_string(), "4.0.0".to_string()),
            ("prelude".to_string(), "6.0.2".to_string()),
        ]);
        let manifests = HashMap::from([
            ("effect", manifest("effect", "4.0.0", &["prelude"])),
            ("prelude", manifest("prelude", "6.0.2", &[])),
        ]);

        let resolved =
            resolve_closure_with(BTreeSet::from(["effect".to_string()]), &versions, |name| {
                Ok(manifests.get(name).cloned())
            })
            .unwrap();

        let names: Vec<_> = resolved.packages.into_iter().map(|package| package.name).collect();
        assert_eq!(names, ["effect", "prelude"]);
        assert!(resolved.errors.is_empty());
    }

    #[test]
    fn reports_missing_package_set_dependency() {
        let versions = HashMap::from([("effect".to_string(), "4.0.0".to_string())]);
        let manifests = HashMap::from([("effect", manifest("effect", "4.0.0", &["prelude"]))]);

        let resolved =
            resolve_closure_with(BTreeSet::from(["effect".to_string()]), &versions, |name| {
                Ok(manifests.get(name).cloned())
            })
            .unwrap();

        assert_eq!(resolved.errors[0].kind, "MissingPackageSetDependency");
    }

    #[test]
    fn reports_missing_manifest() {
        let versions = HashMap::from([("effect".to_string(), "4.0.0".to_string())]);

        let resolved =
            resolve_closure_with(BTreeSet::from(["effect".to_string()]), &versions, |_| Ok(None))
                .unwrap();

        assert_eq!(resolved.errors[0].kind, "MissingManifest");
    }
}
