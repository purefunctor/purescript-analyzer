use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use console::style;
use serde::Deserialize;

use crate::test_runner::category::TestCategory;
use crate::test_runner::cli::RunArgs;
use crate::test_runner::decision;
use crate::test_runner::decision::DecisionInput;
use crate::test_runner::ui;

#[derive(Deserialize)]
struct PendingSnapshotJson {
    path: String,
}

pub struct PendingResult {
    pub count: usize,
    pub total_lines_changed: usize,
}

pub struct SnapshotInfo {
    pub snapshot_path: String,
    pub short_path: String,
    pub snap_new: PathBuf,
    pub is_update: bool,
    pub trace_path: Option<PathBuf>,
}

/// Collect pending snapshots for a category, optionally filtered.
pub fn collect_pending_snapshots(category: TestCategory, filters: &[String]) -> Vec<SnapshotInfo> {
    let pending_output = Command::new("cargo")
        .arg("insta")
        .arg("pending-snapshots")
        .arg("--as-json")
        .stderr(Stdio::null())
        .output()
        .expect("Failed to run cargo insta");

    let pending = String::from_utf8_lossy(&pending_output.stdout);
    let pending = pending.trim();

    let cwd = env::current_dir().unwrap();
    let fixtures_fragment = category.fixtures_subdir_fragment();

    let mut snapshots = Vec::new();

    for line in pending.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let snapshot_path = if let Ok(snapshot) = serde_json::from_str::<PendingSnapshotJson>(line)
        {
            snapshot.path
        } else {
            continue;
        };

        if !snapshot_path.contains(&fixtures_fragment) {
            continue;
        }

        if !filters.is_empty() && !filters.iter().any(|f| snapshot_path.contains(f)) {
            continue;
        }

        let short_path = snapshot_path
            .strip_prefix(cwd.to_str().unwrap_or(""))
            .unwrap_or(&snapshot_path)
            .trim_start_matches('/')
            .to_string();

        let snap_new = PathBuf::from(format!("{}.new", snapshot_path));
        let is_update = Path::new(&snapshot_path).exists();

        snapshots.push(SnapshotInfo {
            snapshot_path,
            short_path,
            snap_new,
            is_update,
            trace_path: None, // Populated separately if needed
        });
    }

    snapshots
}

pub fn process_pending_snapshots(
    category: TestCategory,
    args: &RunArgs,
    trace_paths: &[PathBuf],
) -> PendingResult {
    let mut snapshots = collect_pending_snapshots(category, &args.filters);

    // Populate trace paths
    for info in &mut snapshots {
        info.trace_path = category.trace_for_snapshot(Path::new(&info.snapshot_path), trace_paths);
    }

    let count = snapshots.len();
    let mut total_lines_changed = 0;

    let limits = decision::decide_snapshot_limits(&DecisionInput {
        tests_passed: true, // not relevant for snapshot limits
        pending_count: count,
        total_lines_changed: 0, // not known yet, not relevant for limits
        showed_diffs: args.diff,
        ran_all: args.filters.is_empty(),
        debug: args.debug,
        trace_count: trace_paths.len(),
    });

    let max_shown = limits.max_shown;

    for info in snapshots.iter().take(max_shown) {
        let snap = Path::new(&info.snapshot_path);
        let stats = if info.is_update {
            ui::display_snapshot_diff(
                snap,
                &info.snap_new,
                &info.short_path,
                info.trace_path.as_deref(),
                args.diff,
            )
        } else {
            ui::display_new_snapshot(
                &info.snap_new,
                &info.short_path,
                info.trace_path.as_deref(),
                args.diff,
            )
        };
        total_lines_changed += stats.added + stats.removed;
    }

    if count > max_shown {
        let hidden = count - max_shown;
        println!(
            "{}",
            style(format!("...and {} more pending snapshot(s) not shown", hidden)).dim()
        );
    }

    PendingResult { count, total_lines_changed }
}

pub struct AcceptRejectResult {
    pub accepted: usize,
    pub rejected: usize,
    pub failed: usize,
}

pub fn accept_snapshots(snapshots: &[SnapshotInfo]) -> AcceptRejectResult {
    let mut accepted = 0;
    let mut failed = 0;

    for info in snapshots {
        if !info.snap_new.exists() {
            println!(
                "{} {} (missing .new file)",
                style("SKIP").yellow().bold(),
                style(&info.short_path).cyan()
            );
            failed += 1;
            continue;
        }

        let snap_path = Path::new(&info.snapshot_path);

        // Remove existing snapshot if present
        if snap_path.exists()
            && let Err(e) = fs::remove_file(snap_path)
        {
            println!("{} {} ({})", style("FAIL").red().bold(), style(&info.short_path).cyan(), e);
            failed += 1;
            continue;
        }

        // Rename .new to .snap
        if let Err(e) = fs::rename(&info.snap_new, snap_path) {
            println!("{} {} ({})", style("FAIL").red().bold(), style(&info.short_path).cyan(), e);
            failed += 1;
            continue;
        }

        println!("{} {}", style("ACCEPTED").green().bold(), style(&info.short_path).cyan());
        accepted += 1;
    }

    AcceptRejectResult { accepted, rejected: 0, failed }
}

pub fn reject_snapshots(snapshots: &[SnapshotInfo]) -> AcceptRejectResult {
    let mut rejected = 0;
    let mut failed = 0;

    for info in snapshots {
        if !info.snap_new.exists() {
            println!(
                "{} {} (missing .new file)",
                style("SKIP").yellow().bold(),
                style(&info.short_path).cyan()
            );
            failed += 1;
            continue;
        }

        if let Err(e) = fs::remove_file(&info.snap_new) {
            println!("{} {} ({})", style("FAIL").red().bold(), style(&info.short_path).cyan(), e);
            failed += 1;
            continue;
        }

        println!("{} {}", style("REJECTED").red().bold(), style(&info.short_path).cyan());
        rejected += 1;
    }

    AcceptRejectResult { accepted: 0, rejected, failed }
}
