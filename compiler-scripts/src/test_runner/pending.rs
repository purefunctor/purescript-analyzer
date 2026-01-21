use std::env;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use serde::Deserialize;

use crate::test_runner::category::TestCategory;
use crate::test_runner::cli::RunArgs;
use crate::test_runner::ui;

#[derive(Deserialize)]
struct PendingSnapshot {
    path: String,
}

pub fn process_pending_snapshots(
    category: TestCategory,
    args: &RunArgs,
    trace_paths: &[PathBuf],
) -> usize {
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

    let mut count = 0;
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

        if !snapshot_path.contains(&fixtures_fragment) {
            continue;
        }

        if !args.filters.is_empty() && !args.filters.iter().any(|f| snapshot_path.contains(f)) {
            continue;
        }

        let short_path = snapshot_path
            .strip_prefix(cwd.to_str().unwrap_or(""))
            .unwrap_or(&snapshot_path)
            .trim_start_matches('/');

        let snap = Path::new(&snapshot_path);
        let snap_new = PathBuf::from(format!("{}.new", snapshot_path));
        let trace_path = category.trace_for_snapshot(snap, trace_paths);

        if snap.exists() {
            ui::display_snapshot_diff(snap, &snap_new, short_path, trace_path.as_deref());
        } else {
            ui::display_new_snapshot(&snap_new, short_path, trace_path.as_deref());
        }

        count += 1;
        println!();
    }

    count
}
