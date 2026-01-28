//! Package compatibility checking with QueryEngine.

use std::path::Path;

use analyzer::QueryEngine;
use diagnostics::{DiagnosticsContext, Severity, ToDiagnostics};
use files::{FileId, Files};
use line_index::LineIndex;
use rowan::TextSize;

use tracing::field;
use url::Url;

use crate::loader;

/// Result of checking a single file.
pub struct FileResult {
    pub relative_path: String,
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

/// Checks all packages in the packages directory and returns diagnostics.
///
/// `packages_dir` should point to the directory containing unpacked packages.
/// `target_package` is the specific package to report diagnostics for (others are loaded as deps).
pub fn check_package(packages_dir: &Path, target_package: &str) -> CheckResult {
    let _span =
        tracing::info_span!(target: "compiler_compatibility", "check_package", target_package)
            .entered();

    let (engine, files, file_ids) = loader::load_packages(packages_dir);

    let target_directory = find_package_dir(packages_dir, target_package);
    let target_files = if let Some(directory) = &target_directory {
        loader::filter_package_files(&files, &file_ids, directory)
    } else {
        vec![]
    };

    let mut results = Vec::new();
    let mut total_errors = 0;
    let mut total_warnings = 0;

    tracing::info!(target: "compiler_compatibility", count = target_files.len());

    for id in target_files {
        let relative_path = compute_relative_path(&files, id, packages_dir);
        let file_result = check_file(&engine, &files, id, &relative_path);

        total_errors += file_result.error_count;
        total_warnings += file_result.warning_count;
        results.push(file_result);
    }

    CheckResult { files: results, total_errors, total_warnings }
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

fn check_file(engine: &QueryEngine, _files: &Files, id: FileId, relative_path: &str) -> FileResult {
    let _span = tracing::info_span!(target: "compiler_compatibility", "check_file").entered();

    let content = engine.content(id);
    let line_index = LineIndex::new(&content);

    let Ok((parsed, _)) = engine.parsed(id) else {
        return FileResult {
            relative_path: relative_path.to_string(),
            error_count: 1,
            warning_count: 0,
            output: format!("{relative_path}:1:1: error[ParseError]: Failed to parse file\n"),
        };
    };

    if let Some(module_name) = parsed.module_name() {
        tracing::info!(target: "compiler_compatibility", module_name = %module_name);
    } else {
        tracing::warn!(target: "compiler_compatibility", path = ?relative_path, "Invalid module name");
    };

    let root = parsed.syntax_node();
    let stabilized = match engine.stabilized(id) {
        Ok(s) => s,
        Err(_) => {
            return FileResult {
                relative_path: relative_path.to_string(),
                error_count: 1,
                warning_count: 0,
                output: format!(
                    "{relative_path}:1:1: error[StabilizeError]: Failed to stabilize\n"
                ),
            };
        }
    };

    let indexed = match engine.indexed(id) {
        Ok(i) => i,
        Err(_) => {
            return FileResult {
                relative_path: relative_path.to_string(),
                error_count: 1,
                warning_count: 0,
                output: format!("{relative_path}:1:1: error[IndexError]: Failed to index\n"),
            };
        }
    };

    let lowered = engine.lowered(id);
    let resolved = engine.resolved(id);
    let checked = engine.checked(id);

    let checked_ref = match &checked {
        Ok(c) => c,
        Err(_) => {
            return FileResult {
                relative_path: relative_path.to_string(),
                error_count: 1,
                warning_count: 0,
                output: format!("{relative_path}:1:1: error[CheckError]: Failed to check\n"),
            };
        }
    };

    let context = DiagnosticsContext::new(&content, &root, &stabilized, &indexed, checked_ref);

    let mut all_diagnostics = vec![];

    if let Ok(ref lowered) = lowered {
        for error in &lowered.errors {
            all_diagnostics.extend(error.to_diagnostics(&context));
        }
    }

    if let Ok(ref resolved) = resolved {
        for error in &resolved.errors {
            all_diagnostics.extend(error.to_diagnostics(&context));
        }
    }

    for error in &checked_ref.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    let mut output = String::new();
    let mut error_count = 0;
    let mut warning_count = 0;

    for diagnostic in &all_diagnostics {
        match diagnostic.severity {
            Severity::Error => error_count += 1,
            Severity::Warning => warning_count += 1,
        }

        let severity = match diagnostic.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
        };

        let offset = TextSize::from(diagnostic.primary.start);
        let (line, col) = offset_to_line_col(&line_index, &content, offset);

        output.push_str(&format!(
            "{relative_path}:{}:{}: {severity}[{}]: {}\n",
            line + 1,
            col + 1,
            diagnostic.code,
            diagnostic.message
        ));
    }

    FileResult { relative_path: relative_path.to_string(), error_count, warning_count, output }
}

fn offset_to_line_col(line_index: &LineIndex, content: &str, offset: TextSize) -> (u32, u32) {
    let line_col = line_index.line_col(offset);
    let line = line_col.line;

    let line_range = match line_index.line(line) {
        Some(r) => r,
        None => return (line, 0),
    };
    let line_content = &content[line_range];
    let until_col = &line_content[..line_col.col as usize];
    let character = until_col.chars().count() as u32;

    (line, character)
}
