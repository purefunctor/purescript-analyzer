use std::collections::{BTreeMap, HashSet};

use purescript_registry::{PackageSet, RegistryReader};
use semver::Version;

use crate::{
    error::{CompatError, Result},
    types::ResolvedSet,
};

pub fn resolve(
    root_packages: &[String],
    package_set: &PackageSet,
    registry: &impl RegistryReader,
) -> Result<ResolvedSet> {
    let mut resolved = BTreeMap::new();
    let mut visited = HashSet::new();

    for name in root_packages {
        let version = package_set
            .packages
            .packages
            .get(name)
            .ok_or_else(|| CompatError::MissingFromPackageSet(name.clone()))?;
        resolve_recursive(name, version, package_set, registry, &mut resolved, &mut visited)?;
    }

    Ok(ResolvedSet { packages: resolved })
}

fn resolve_recursive(
    name: &str,
    version: &str,
    package_set: &PackageSet,
    registry: &impl RegistryReader,
    resolved: &mut BTreeMap<String, String>,
    visited: &mut HashSet<String>,
) -> Result<()> {
    if !visited.insert(name.to_string()) {
        return Ok(());
    }

    resolved.insert(name.to_string(), version.to_string());

    let manifests = registry.read_manifest_versions(name)?;
    let parsed_version: Version = version.parse()?;
    let manifest = manifests
        .iter()
        .find(|m| m.version == parsed_version)
        .ok_or_else(|| CompatError::ManifestNotFound {
            name: name.to_string(),
            version: version.to_string(),
        })?;

    for (dep_name, range) in &manifest.dependencies {
        let dep_version = package_set
            .packages
            .packages
            .get(dep_name)
            .ok_or_else(|| CompatError::MissingFromPackageSet(dep_name.clone()))?;

        if !version_satisfies_range(dep_version, range)? {
            return Err(CompatError::VersionMismatch {
                name: dep_name.clone(),
                set_version: dep_version.clone(),
                range: range.clone(),
            });
        }

        resolve_recursive(dep_name, dep_version, package_set, registry, resolved, visited)?;
    }

    Ok(())
}

fn version_satisfies_range(version: &str, range: &str) -> Result<bool> {
    let parsed_version: Version = version.parse()?;
    let parts: Vec<&str> = range.split_whitespace().collect();

    if parts.len() != 2 {
        return Err(CompatError::Other(format!("invalid version range format: {}", range)));
    }

    let lower = parts[0]
        .strip_prefix(">=")
        .ok_or_else(|| CompatError::Other(format!("expected >= prefix in range: {}", range)))?;
    let upper = parts[1]
        .strip_prefix('<')
        .ok_or_else(|| CompatError::Other(format!("expected < prefix in range: {}", range)))?;

    let lower_version: Version = lower.parse()?;
    let upper_version: Version = upper.parse()?;

    Ok(parsed_version >= lower_version && parsed_version < upper_version)
}
