mod category;
mod cli;
mod decision;
mod nextest;
mod pending;
mod traces;
mod ui;

pub use category::TestCategory;
pub use cli::{CategoryCommand, RunArgs, SnapshotArgs};

use std::path::PathBuf;
use std::time::Instant;

use console::style;

use crate::test_runner::ui::NextActionsArgs;

pub struct TestOutcome {
    pub tests_passed: bool,
    pub pending_count: usize,
    pub trace_paths: Vec<PathBuf>,
}

pub fn run_category(category: TestCategory, args: &RunArgs) -> TestOutcome {
    // 1. Hash fixtures and print timing
    let start = Instant::now();
    let fixture_hashes = crate::fixtures::fixture_env();
    println!("{}", style(format!("Hashed fixtures in {}ms", start.elapsed().as_millis())).dim());

    // 2. Run nextest
    let tests_passed = nextest::run_nextest(category, args, &fixture_hashes);

    // 3. Collect trace paths
    let trace_paths = traces::collect_trace_paths(&args.filters, args.debug);

    // 4. Process pending snapshots
    let pending_result = pending::process_pending_snapshots(category, args, &trace_paths);

    // 5. Print next actions
    ui::print_next_actions(NextActionsArgs {
        category_name: category.as_str(),
        filters: &args.filters,
        tests_passed,
        pending_count: pending_result.count,
        total_lines_changed: pending_result.total_lines_changed,
        trace_paths: &trace_paths,
        debug: args.debug,
        showed_diffs: args.diff,
    });

    TestOutcome { tests_passed, pending_count: pending_result.count, trace_paths }
}

pub struct SnapshotOutcome {
    pub success: bool,
    pub count: usize,
}

pub fn accept_category(category: TestCategory, args: &SnapshotArgs) -> SnapshotOutcome {
    let snapshots = pending::collect_pending_snapshots(category, &args.filters);

    if snapshots.is_empty() {
        println!("{}", style("No pending snapshots found.").dim());
        return SnapshotOutcome { success: true, count: 0 };
    }

    if !args.all && args.filters.is_empty() {
        println!("{} pending snapshot(s) in {}", snapshots.len(), style(category.as_str()).cyan());
        println!();
        for info in &snapshots {
            println!("  {}", style(&info.short_path).dim());
        }
        println!();
        println!(
            "To accept all, run: {}",
            style(format!("just t {} accept --all", category.as_str())).cyan()
        );
        return SnapshotOutcome { success: false, count: 0 };
    }

    let result = pending::accept_snapshots(&snapshots);
    println!();
    println!("{}", style(format!("Accepted {} snapshot(s)", result.accepted)).green());

    SnapshotOutcome { success: result.failed == 0, count: result.accepted }
}

pub fn reject_category(category: TestCategory, args: &SnapshotArgs) -> SnapshotOutcome {
    let snapshots = pending::collect_pending_snapshots(category, &args.filters);

    if snapshots.is_empty() {
        println!("{}", style("No pending snapshots found.").dim());
        return SnapshotOutcome { success: true, count: 0 };
    }

    if !args.all && args.filters.is_empty() {
        println!("{} pending snapshot(s) in {}", snapshots.len(), style(category.as_str()).cyan());
        println!();
        for info in &snapshots {
            println!("  {}", style(&info.short_path).dim());
        }
        println!();
        println!(
            "To reject all, run: {}",
            style(format!("just t {} reject --all", category.as_str())).cyan()
        );
        return SnapshotOutcome { success: false, count: 0 };
    }

    let result = pending::reject_snapshots(&snapshots);
    println!();
    println!("{}", style(format!("Rejected {} snapshot(s)", result.rejected)).red());

    SnapshotOutcome { success: result.failed == 0, count: result.rejected }
}
