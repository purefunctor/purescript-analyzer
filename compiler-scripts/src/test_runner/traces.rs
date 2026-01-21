use std::fs;
use std::path::{Path, PathBuf};

pub const TRACE_DIR: &str = "target/compiler-tracing";

pub fn collect_trace_paths(filters: &[String], debug: bool) -> Vec<PathBuf> {
    if !debug {
        return Vec::new();
    }

    let trace_dir = PathBuf::from(TRACE_DIR);
    if !trace_dir.exists() {
        return Vec::new();
    }

    let Ok(entries) = fs::read_dir(&trace_dir) else {
        return Vec::new();
    };

    let mut entries: Vec<_> = entries.filter_map(|e| e.ok()).collect();
    entries.sort_by_key(|e| e.path());

    let mut trace_paths = Vec::new();
    for entry in entries {
        let path = entry.path();
        if path.extension().is_some_and(|ext| ext == "jsonl") {
            let file_name = path.file_name().unwrap_or_default().to_string_lossy();

            if !filters.is_empty() && !filters.iter().any(|f| file_name.contains(f)) {
                continue;
            }

            trace_paths.push(path);
        }
    }

    trace_paths
}

/// Finds a trace file that matches the given snapshot path.
///
/// Snapshot paths look like: `.../fixtures/checking/200_int_compare_transitive/Main.snap`
/// Trace files look like: `200_int_compare_transitive_Main.jsonl`
///
/// We extract the test identifier (e.g., `200_int_compare_transitive`) from the snapshot's
/// parent directory and the module name from the file, then find a matching trace file.
pub fn match_checking_trace(snap_path: &Path, trace_paths: &[PathBuf]) -> Option<PathBuf> {
    let module_name = snap_path.file_stem()?.to_str()?;
    let test_id = snap_path.parent()?.file_name()?.to_str()?;

    let expected_trace_name = format!("{}_{}.jsonl", test_id, module_name);

    trace_paths
        .iter()
        .find(|trace_path| {
            trace_path
                .file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name == expected_trace_name)
        })
        .cloned()
}
