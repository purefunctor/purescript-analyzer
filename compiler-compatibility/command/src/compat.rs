//! Package compatibility checking with QueryEngine.

use std::collections::HashSet;
use std::path::Path;

use analyzer::QueryEngine;
use building_types::QueryResult;
use diagnostics::{DiagnosticsContext, Severity, ToDiagnostics, format_rustc_with_path};
use files::{FileId, Files};
use petgraph::graphmap::DiGraphMap;
use rayon::prelude::*;
use url::Url;

use crate::loader;
use crate::resolver;
use crate::types::ResolvedSet;

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
    let entries = std::fs::read_dir(packages_dir).ok()?;
    for entry in entries.filter_map(Result::ok) {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if name_str.starts_with(package_name)
            && name_str[package_name.len()..].starts_with('-')
            && entry.file_type().ok()?.is_dir()
        {
            return entry.path().canonicalize().ok();
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
