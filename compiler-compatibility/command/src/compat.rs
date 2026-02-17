//! Package compatibility checking with QueryEngine.

use std::collections::{BTreeMap, HashSet};
use std::path::Path;

use analyzer::QueryEngine;
use building_types::QueryResult;
use diagnostics::{DiagnosticsContext, Severity, ToDiagnostics, format_rustc_with_path};
use files::{FileId, Files};
use petgraph::graphmap::DiGraphMap;
use rayon::prelude::*;
use url::Url;

use crate::types::ResolvedSet;
use crate::{loader, resolver};

/// Result of checking a single file.
pub struct FileResult {
    pub error_count: usize,
    pub warning_count: usize,
    pub output: String,
}

/// Result of checking a package.
pub struct CheckResult {
    pub files: Vec<FileResult>,
    pub total_errors: usize,
    pub total_warnings: usize,
}

/// Outcome for a single package in `check_all` mode.
pub struct PackageOutcome {
    pub version: String,
    pub total_errors: usize,
    pub total_warnings: usize,
    pub topo_layer: usize,
    pub root_cause: bool,
    pub cascaded_from: Vec<String>,
    pub cascaded_from_root_causes: Vec<String>,
}

/// Result of checking all packages.
pub struct AllCheckResult {
    pub outcomes: BTreeMap<String, PackageOutcome>,
}

/// Primes dependency caches by processing files through the query pipeline in
/// topological order. Each layer is processed in parallel using rayon, so that
/// packages within the same layer are primed concurrently.
fn prime_dependencies(
    engine: &QueryEngine,
    files: &Files,
    file_ids: &[FileId],
    target_package: &str,
    resolved: &ResolvedSet,
    packages_dir: &Path,
) {
    let layers = resolver::topological_order(resolved);

    for (layer_index, layer) in layers.iter().enumerate() {
        let dependency_packages: Vec<&String> =
            layer.iter().filter(|name| *name != target_package).collect();

        if dependency_packages.is_empty() {
            continue;
        }

        let layer_files = dependency_packages.iter().flat_map(|package_name| {
            let package_directory = find_package_dir(packages_dir, package_name);
            if let Some(directory) = package_directory {
                loader::filter_package_files(files, file_ids, &directory)
            } else {
                vec![]
            }
        });

        let layer_files: Vec<FileId> = layer_files.collect();
        let package_names: Vec<&str> = dependency_packages.iter().map(|s| s.as_str()).collect();

        tracing::debug!(
            target: "compiler_compatibility",
            layer = layer_index,
            file_count = layer_files.len(),
            packages = ?package_names,
            "Priming"
        );

        // Prime each file in parallel through the full query pipeline.
        layer_files.par_iter().for_each(|&id| {
            let snapshot = engine.snapshot();
            let _ = snapshot.lowered(id);
            let _ = snapshot.resolved(id);
            let _ = snapshot.checked(id);
        });
    }
}

/// Checks all packages in the packages directory and returns diagnostics.
///
/// `packages` should point to the directory containing unpacked packages.
/// `target_package` is the specific package to report diagnostics for.
/// `resolved` provides the dependency graph for cache priming.
pub fn check_package(packages: &Path, target_package: &str, resolved: &ResolvedSet) -> CheckResult {
    let _span =
        tracing::info_span!(target: "compiler_compatibility", "check_package", target_package)
            .entered();

    let (engine, files, file_ids) = loader::load_packages(packages);

    prime_dependencies(&engine, &files, &file_ids, target_package, resolved, packages);

    let target_directory = find_package_dir(packages, target_package);
    let target_files = if let Some(directory) = &target_directory {
        loader::filter_package_files(&files, &file_ids, directory)
    } else {
        vec![]
    };

    target_files.par_iter().for_each(|&id| {
        let snapshot = engine.snapshot();
        let _ = snapshot.indexed(id);
    });

    let layers = module_topological_layers(&engine, &target_files);

    tracing::debug!(
        target: "compiler_compatibility",
        layer_count = layers.len(),
        layers = ?layers.iter().map(|layer| layer.len()).collect::<Vec<_>>(),
        "Layers"
    );

    {
        for (layer_index, layer) in layers.iter().enumerate() {
            tracing::debug!(
                target: "compiler_compatibility",
                layer = layer_index,
                file_count = layer.len(),
                "Priming"
            );
            layer.par_iter().for_each(|&id| {
                let snapshot = engine.snapshot();
                let _ = snapshot.lowered(id);
                let _ = snapshot.resolved(id);
            });
        }
    }

    let mut results = vec![];
    let mut total_errors = 0;
    let mut total_warnings = 0;

    for layer in &layers {
        for &id in layer {
            let relative_path = compute_relative_path(&files, id, packages);
            let file_result = collect_diagnostics(&engine, id, &relative_path);

            total_errors += file_result.error_count;
            total_warnings += file_result.warning_count;
            results.push(file_result);
        }
    }

    CheckResult { files: results, total_errors, total_warnings }
}

/// Checks a single package using an already-loaded engine.
///
/// Files for the package must already be loaded in the engine. The caller
/// is responsible for ensuring dependencies are already cached via
/// topological ordering.
fn check_loaded_package(
    engine: &QueryEngine,
    files: &Files,
    package_files: &[FileId],
    packages_dir: &Path,
    _quiet: bool,
) -> CheckResult {
    // Index target files
    package_files.par_iter().for_each(|&id| {
        let snapshot = engine.snapshot();
        let _ = snapshot.indexed(id);
    });

    // Compute module-level topological layers within the package
    let layers = module_topological_layers(engine, package_files);

    tracing::debug!(
        target: "compiler_compatibility",
        layer_count = layers.len(),
        layers = ?layers.iter().map(|layer| layer.len()).collect::<Vec<_>>(),
        "Module layers"
    );

    // Prime in module-layer order
    for layer in &layers {
        layer.par_iter().for_each(|&id| {
            let snapshot = engine.snapshot();
            let _ = snapshot.lowered(id);
            let _ = snapshot.resolved(id);
        });
    }

    // Collect diagnostics
    let mut results = vec![];
    let mut total_errors = 0;
    let mut total_warnings = 0;

    for layer in &layers {
        for &id in layer {
            let relative_path = compute_relative_path(files, id, packages_dir);
            let file_result = collect_diagnostics(engine, id, &relative_path);

            total_errors += file_result.error_count;
            total_warnings += file_result.warning_count;
            results.push(file_result);
        }
    }

    CheckResult { files: results, total_errors, total_warnings }
}

/// Checks all packages using a single shared engine, processing in
/// topological order to maximize cache reuse.
pub fn check_all(packages_dir: &Path, resolved: &ResolvedSet, quiet: bool) -> AllCheckResult {
    let _span = tracing::info_span!(target: "compiler_compatibility", "check_all").entered();

    // Load all files once
    let (engine, files, file_ids) = loader::load_packages(packages_dir);

    // Build package -> files map
    let mut package_files: BTreeMap<String, Vec<FileId>> = BTreeMap::new();
    for name in resolved.packages.keys() {
        let dir = find_package_dir(packages_dir, name);
        let pkg_files = if let Some(directory) = dir {
            loader::filter_package_files(&files, &file_ids, &directory)
        } else {
            tracing::warn!(target: "compiler_compatibility", package = name, "Package directory not found");
            vec![]
        };
        package_files.insert(name.clone(), pkg_files);
    }

    // Index all files upfront
    tracing::info!(target: "compiler_compatibility", file_count = file_ids.len(), "Indexing all files");
    file_ids.par_iter().for_each(|&id| {
        let snapshot = engine.snapshot();
        let _ = snapshot.indexed(id);
    });

    // Process in topological layers
    let layers = resolver::topological_order(resolved);
    let mut outcomes: BTreeMap<String, PackageOutcome> = BTreeMap::new();

    for (layer_index, layer) in layers.iter().enumerate() {
        tracing::info!(
            target: "compiler_compatibility",
            layer = layer_index,
            package_count = layer.len(),
            "Processing layer"
        );

        // Check each package in the layer
        // (packages within a layer have no mutual dependencies)
        for package_name in layer {
            let _pkg_span = tracing::info_span!(
                target: "compiler_compatibility",
                "check_package",
                package = package_name
            )
            .entered();

            let pkg_files = package_files.get(package_name).map(|v| v.as_slice()).unwrap_or(&[]);
            let version = resolved.packages.get(package_name).cloned().unwrap_or_default();

            let result = if pkg_files.is_empty() {
                // Synthetic error for missing package directory
                CheckResult {
                    files: vec![FileResult {
                        error_count: 1,
                        warning_count: 0,
                        output: format!("{}: error: package directory not found\n", package_name),
                    }],
                    total_errors: 1,
                    total_warnings: 0,
                }
            } else {
                check_loaded_package(&engine, &files, pkg_files, packages_dir, quiet)
            };

            // Print diagnostics if not quiet
            if !quiet {
                for file_result in &result.files {
                    if !file_result.output.is_empty() {
                        print!("{}", file_result.output);
                    }
                }
            }

            // Log summary
            let summary = format!(
                "{}: {} errors, {} warnings",
                package_name, result.total_errors, result.total_warnings
            );
            if result.total_errors > 0 {
                tracing::error!(target: "compiler_compatibility", "{}", summary);
            } else if result.total_warnings > 0 {
                tracing::warn!(target: "compiler_compatibility", "{}", summary);
            } else {
                tracing::info!(target: "compiler_compatibility", "{}", summary);
            }

            // Classify: root cause vs cascaded
            let deps = resolved.dependencies.get(package_name).cloned().unwrap_or_default();
            let failed_deps: Vec<String> = deps
                .iter()
                .filter(|d| outcomes.get(*d).is_some_and(|o| o.total_errors > 0))
                .cloned()
                .collect();

            let failed = result.total_errors > 0;
            let root_cause = failed && failed_deps.is_empty();
            let cascaded_from = if failed { failed_deps } else { vec![] };

            // Transitive root-cause attribution
            let cascaded_from_root_causes = if !cascaded_from.is_empty() {
                collect_root_causes(&cascaded_from, &outcomes, resolved)
            } else {
                vec![]
            };

            outcomes.insert(
                package_name.clone(),
                PackageOutcome {
                    version,
                    total_errors: result.total_errors,
                    total_warnings: result.total_warnings,
                    topo_layer: layer_index,
                    root_cause,
                    cascaded_from,
                    cascaded_from_root_causes,
                },
            );
        }
    }

    AllCheckResult { outcomes }
}

/// Walks transitive dependencies to find all root-cause packages.
fn collect_root_causes(
    failed_deps: &[String],
    outcomes: &BTreeMap<String, PackageOutcome>,
    resolved: &ResolvedSet,
) -> Vec<String> {
    let mut root_causes = Vec::new();
    let mut visited = HashSet::new();
    let mut stack: Vec<String> = failed_deps.to_vec();

    while let Some(dep) = stack.pop() {
        if !visited.insert(dep.clone()) {
            continue;
        }
        if let Some(outcome) = outcomes.get(&dep) {
            if outcome.root_cause {
                root_causes.push(dep.clone());
            }
            // Continue walking through failed transitive deps
            if let Some(transitive_deps) = resolved.dependencies.get(&dep) {
                for td in transitive_deps {
                    if outcomes.get(td).is_some_and(|o| o.total_errors > 0) {
                        stack.push(td.clone());
                    }
                }
            }
        }
    }

    root_causes.sort();
    root_causes.dedup();
    root_causes
}

/// Builds a module-level dependency graph from indexed imports and returns
/// files grouped into topological layers.
///
/// Only tracks edges between files in the provided set. Files with no
/// intra-package dependencies go in layer 0.
fn module_topological_layers(engine: &QueryEngine, file_ids: &[FileId]) -> Vec<Vec<FileId>> {
    let file_set: HashSet<FileId> = file_ids.iter().copied().collect();
    let mut graph = DiGraphMap::new();

    for &id in file_ids {
        graph.add_node(id);
    }

    for &id in file_ids {
        let Ok(indexed) = engine.indexed(id) else { continue };
        for import in indexed.imports.values() {
            let Some(name) = &import.name else { continue };
            let Some(dependency_id) = engine.module_file(name) else { continue };
            if file_set.contains(&dependency_id) {
                graph.add_edge(id, dependency_id, ());
            }
        }
    }

    resolver::topological_layers(&graph)
}

fn find_package_dir(packages_dir: &Path, package_name: &str) -> Option<std::path::PathBuf> {
    let prefix = format!("{}-", package_name);
    let entries = std::fs::read_dir(packages_dir).ok()?;
    for entry in entries.filter_map(Result::ok) {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if let Some(suffix) = name_str.strip_prefix(&prefix) {
            if suffix.parse::<semver::Version>().is_ok() && entry.file_type().ok()?.is_dir() {
                return entry.path().canonicalize().ok();
            }
        }
    }
    None
}

fn compute_relative_path(files: &Files, id: FileId, base_dir: &Path) -> String {
    let uri = files.path(id);
    Url::parse(&uri)
        .ok()
        .and_then(|u| u.to_file_path().ok())
        .and_then(|p| p.strip_prefix(base_dir).ok().map(|r| r.to_path_buf()))
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| uri.to_string())
}

trait QueryResultExt<T> {
    fn or_file_error(self, path: &str, code: &str, message: &str) -> Result<T, FileResult>;
}

impl<T> QueryResultExt<T> for QueryResult<T> {
    fn or_file_error(self, path: &str, code: &str, message: &str) -> Result<T, FileResult> {
        self.map_err(|_| FileResult {
            error_count: 1,
            warning_count: 0,
            output: format!("{path}:1:1: error[{code}]: {message}\n"),
        })
    }
}

fn collect_diagnostics(engine: &QueryEngine, id: FileId, relative_path: &str) -> FileResult {
    match collect_diagnostics_inner(engine, id, relative_path) {
        Ok(result) | Err(result) => result,
    }
}

fn collect_diagnostics_inner(
    engine: &QueryEngine,
    id: FileId,
    relative_path: &str,
) -> Result<FileResult, FileResult> {
    let content = engine.content(id);

    let (parsed, _) =
        engine.parsed(id).or_file_error(relative_path, "ParseError", "Failed to parse file")?;

    if let Some(module_name) = parsed.module_name() {
        tracing::info!(target: "compiler_compatibility", module_name = %module_name);
    } else {
        tracing::warn!(target: "compiler_compatibility", path = ?relative_path, "Invalid module name");
    };

    let root = parsed.syntax_node();

    let stabilized = engine.stabilized(id).or_file_error(
        relative_path,
        "StabilizeError",
        "Failed to stabilize",
    )?;

    let indexed =
        engine.indexed(id).or_file_error(relative_path, "IndexError", "Failed to index")?;

    let lowered =
        engine.lowered(id).or_file_error(relative_path, "LowerError", "Failed to lower")?;

    let resolved = engine.resolved(id);

    let checked =
        engine.checked(id).or_file_error(relative_path, "CheckError", "Failed to check")?;

    let context =
        DiagnosticsContext::new(&content, &root, &stabilized, &indexed, &lowered, &checked);

    let mut all_diagnostics = vec![];

    for error in &lowered.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    if let Ok(ref resolved) = resolved {
        for error in &resolved.errors {
            all_diagnostics.extend(error.to_diagnostics(&context));
        }
    }

    for error in &checked.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    let mut error_count = 0;
    let mut warning_count = 0;

    for diagnostic in &all_diagnostics {
        match diagnostic.severity {
            Severity::Error => error_count += 1,
            Severity::Warning => warning_count += 1,
        }
    }

    let output = format_rustc_with_path(&all_diagnostics, &content, relative_path);

    Ok(FileResult { error_count, warning_count, output })
}
