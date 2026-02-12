use std::collections::{BTreeMap, HashSet, VecDeque};

use petgraph::graphmap::DiGraphMap;
use registry::{PackageSet, RegistryReader};
use semver::Version;

use crate::error::{CompatError, Result};
use crate::types::ResolvedSet;

pub fn resolve(
    root_packages: &[String],
    package_set: &PackageSet,
    registry: &impl RegistryReader,
) -> Result<ResolvedSet> {
    let mut resolved = BTreeMap::new();
    let mut dependencies = BTreeMap::new();
    let mut visited = HashSet::new();

    for name in root_packages {
        let version = package_set
            .packages
            .packages
            .get(name)
            .ok_or_else(|| CompatError::MissingFromPackageSet(name.clone()))?;
        resolve_recursive(
            name,
            version,
            package_set,
            registry,
            &mut resolved,
            &mut dependencies,
            &mut visited,
        )?;
    }

    Ok(ResolvedSet { packages: resolved, dependencies })
}

fn resolve_recursive(
    name: &str,
    version: &str,
    package_set: &PackageSet,
    registry: &impl RegistryReader,
    resolved: &mut BTreeMap<String, String>,
    dependencies: &mut BTreeMap<String, Vec<String>>,
    visited: &mut HashSet<String>,
) -> Result<()> {
    if !visited.insert(name.to_string()) {
        return Ok(());
    }

    resolved.insert(name.to_string(), version.to_string());

    let manifests = registry.read_manifest_versions(name)?;
    let parsed_version: Version = version.parse()?;
    let manifest = manifests.iter().find(|m| m.version == parsed_version).ok_or_else(|| {
        CompatError::ManifestNotFound { name: name.to_string(), version: version.to_string() }
    })?;

    let mut dependency_names = Vec::new();

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

        dependency_names.push(dep_name.clone());
        resolve_recursive(
            dep_name,
            dep_version,
            package_set,
            registry,
            resolved,
            dependencies,
            visited,
        )?;
    }

    dependencies.insert(name.to_string(), dependency_names);

    Ok(())
}

/// Returns packages grouped into topological layers.
///
/// Layer 0 contains packages with no dependencies, layer 1 contains packages
/// whose dependencies are all in layer 0, and so on.
pub fn topological_order(resolved: &ResolvedSet) -> Vec<Vec<String>> {
    let all_packages: HashSet<&String> = resolved.packages.keys().collect();

    let mut graph = DiGraphMap::new();

    for name in &all_packages {
        graph.add_node(name.as_str());
    }

    // Edge direction: dependent -> dependency (name depends on dep).
    for (name, deps) in &resolved.dependencies {
        if all_packages.contains(name) {
            for dep in deps {
                if all_packages.contains(dep) {
                    graph.add_edge(name.as_str(), dep.as_str(), ());
                }
            }
        }
    }

    topological_layers(&graph)
        .into_iter()
        .map(|layer| layer.into_iter().map(String::from).collect())
        .collect()
}

/// Computes topological layers from a dependency graph using Kahn's algorithm.
///
/// Edges point from dependent to dependency. Layer 0 contains nodes with no
/// outgoing edges (no dependencies), layer 1 contains nodes whose dependencies
/// are all in layer 0, and so on.
pub fn topological_layers<N: Copy + Eq + Ord + std::hash::Hash>(
    graph: &DiGraphMap<N, ()>,
) -> Vec<Vec<N>> {
    // Count of unprocessed dependencies per node.
    let mut dependency_count: BTreeMap<N, usize> = BTreeMap::new();
    for node in graph.nodes() {
        dependency_count.insert(node, graph.neighbors(node).count());
    }

    // Reverse edges: dependency -> list of dependents.
    let mut dependents: BTreeMap<N, Vec<N>> = BTreeMap::new();
    for node in graph.nodes() {
        for dependency in graph.neighbors(node) {
            dependents.entry(dependency).or_default().push(node);
        }
    }

    let mut layers = Vec::new();
    let mut queue: VecDeque<N> = VecDeque::new();

    for (&node, &count) in &dependency_count {
        if count == 0 {
            queue.push_back(node);
        }
    }

    while !queue.is_empty() {
        let layer: Vec<N> = queue.drain(..).collect();
        let mut next_queue = VecDeque::new();

        for &node in &layer {
            if let Some(dependent_nodes) = dependents.get(&node) {
                for &dependent in dependent_nodes {
                    let count = dependency_count.get_mut(&dependent).unwrap();
                    *count -= 1;
                    if *count == 0 {
                        next_queue.push_back(dependent);
                    }
                }
            }
        }

        layers.push(layer);
        queue = next_queue;
    }

    layers
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
