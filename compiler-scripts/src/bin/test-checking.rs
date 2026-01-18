use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};
use std::{env, fs};

use clap::Parser;
use compiler_scripts::console::style;
use compiler_scripts::fixtures::fixture_env;
use compiler_scripts::snapshots::{print_diff, strip_frontmatter};
use serde::Deserialize;

const TRACE_DIR: &str = "target/compiler-tracing";

#[derive(Deserialize)]
struct PendingSnapshot {
    path: String,
}

#[derive(Parser)]
#[command(about = "Run type checker integration tests with snapshot diffing")]
struct Config {
    /// Test name or number filters
    #[arg(num_args = 0..)]
    filters: Vec<String>,

    /// Verbose output (show test progress)
    #[arg(short, long)]
    verbose: bool,

    /// Enable tracing output for debugging
    #[arg(long)]
    debug: bool,
}

fn main() {
    let config = Config::parse();

    let (fixture_hashes, hash_duration) = hash_fixtures();
    println!("{}", style(format!("Hashed fixtures in {}ms", hash_duration.as_millis())).dim());

    run_tests(&config, &fixture_hashes);

    let trace_paths = collect_trace_paths(&config);
    process_pending_snapshots(&config, &trace_paths);
}

fn hash_fixtures() -> (HashMap<String, String>, Duration) {
    let start = Instant::now();
    let hashes = fixture_env();
    (hashes, start.elapsed())
}

fn build_nextest_command(config: &Config, fixture_hashes: &HashMap<String, String>) -> Command {
    let mut cmd = Command::new("cargo");
    cmd.arg("nextest").arg("run").arg("-p").arg("tests-integration").arg("--test").arg("checking");

    if config.debug {
        cmd.env("TRACE_LEVEL", "debug");
    }

    for filter in &config.filters {
        cmd.arg(filter);
    }

    if config.verbose {
        cmd.arg("--status-level=fail");
        cmd.arg("--color=always");
    } else {
        cmd.arg("--status-level=none");
    }

    cmd.env("INSTA_FORCE_PASS", "1");
    for (key, value) in fixture_hashes {
        cmd.env(key, value);
    }

    cmd
}

fn run_tests(config: &Config, fixture_hashes: &HashMap<String, String>) {
    let mut cmd = build_nextest_command(config, fixture_hashes);

    if config.verbose {
        cmd.status().expect("Failed to run cargo nextest");
    } else {
        cmd.stdout(Stdio::null()).stderr(Stdio::null());
        let status = cmd.status().expect("Failed to run cargo nextest");

        if !status.success() {
            eprintln!("{}", style("Tests failed, re-running verbose...").yellow());

            let verbose_config =
                Config { filters: config.filters.clone(), verbose: true, debug: config.debug };
            let mut retry = build_nextest_command(&verbose_config, fixture_hashes);
            let _ = retry.status();
        }
    }
}

fn collect_trace_paths(config: &Config) -> Vec<PathBuf> {
    if !config.debug {
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

            // Skip traces that don't match any filter
            if !config.filters.is_empty() && !config.filters.iter().any(|f| file_name.contains(f)) {
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
fn find_trace_for_snapshot(snap_path: &str, trace_paths: &[PathBuf]) -> Option<PathBuf> {
    let path = Path::new(snap_path);

    // Get module name from file (e.g., "Main" from "Main.snap")
    let module_name = path.file_stem()?.to_str()?;

    // Get test identifier from parent directory (e.g., "200_int_compare_transitive")
    let test_id = path.parent()?.file_name()?.to_str()?;

    // Trace files are named: {test_id}_{module_name}.jsonl
    let expected_trace_name = format!("{}_{}.jsonl", test_id, module_name);

    // Find matching trace file
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

fn process_pending_snapshots(config: &Config, trace_paths: &[PathBuf]) {
    let pending_output = Command::new("cargo")
        .arg("insta")
        .arg("pending-snapshots")
        .arg("--as-json")
        .stderr(Stdio::null())
        .output()
        .expect("Failed to run cargo insta");

    let pending = String::from_utf8_lossy(&pending_output.stdout);
    let pending = pending.trim();

    if pending.is_empty() {
        println!("{}", style("No pending snapshots.").dim());
        return;
    }

    println!();

    let cwd = env::current_dir().unwrap();

    for line in pending.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let snapshot_path = if let Ok(snapshot) = serde_json::from_str::<PendingSnapshot>(line) {
            snapshot.path
        } else {
            continue;
        };

        // Skip snapshots that don't match any filter
        if !config.filters.is_empty() && !config.filters.iter().any(|f| snapshot_path.contains(f)) {
            continue;
        }

        let short_path = snapshot_path
            .strip_prefix(cwd.to_str().unwrap_or(""))
            .unwrap_or(&snapshot_path)
            .trim_start_matches('/');

        let snap = Path::new(&snapshot_path);
        let snap_new = PathBuf::from(format!("{}.new", snapshot_path));
        let trace_path = find_trace_for_snapshot(&snapshot_path, trace_paths);

        if snap.exists() {
            display_snapshot_diff(snap, &snap_new, short_path, trace_path.as_deref());
        } else {
            display_new_snapshot(&snap_new, short_path, trace_path.as_deref());
        }

        println!();
    }
}

fn display_snapshot_diff(
    snap: &Path,
    snap_new: &Path,
    short_path: &str,
    trace_path: Option<&Path>,
) {
    println!("{} {}", style("UPDATED").yellow().bold(), style(short_path).cyan());

    if let Some(trace) = trace_path {
        println!("  {} {}", style("TRACE").magenta().bold(), style(trace.display()).cyan());
    }

    println!();

    let old_content = fs::read_to_string(snap).unwrap_or_default();
    let new_content = fs::read_to_string(snap_new).unwrap_or_default();

    let old_stripped = strip_frontmatter(&old_content);
    let new_stripped = strip_frontmatter(&new_content);

    print_diff(old_stripped, new_stripped);
}

fn display_new_snapshot(snap_new: &Path, short_path: &str, trace_path: Option<&Path>) {
    println!("{} {}", style("CREATED").green().bold(), style(short_path).cyan());

    if let Some(trace) = trace_path {
        println!("  {} {}", style("TRACE").magenta().bold(), style(trace.display()).cyan());
    }

    println!();

    let new_content = fs::read_to_string(snap_new).unwrap_or_default();
    for (i, line) in strip_frontmatter(&new_content).lines().enumerate() {
        println!("{}  {}", style(format!("{:3}", i + 1)).dim(), line);
    }
}
