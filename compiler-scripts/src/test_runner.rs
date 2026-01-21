mod category;
mod cli;
mod nextest;
mod pending;
mod traces;
mod ui;

pub use category::TestCategory;
pub use cli::RunArgs;

use std::path::PathBuf;
use std::time::Instant;

use console::style;

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
    let pending_count = pending::process_pending_snapshots(category, args, &trace_paths);

    // 5. Print next actions
    ui::print_next_actions(
        category.as_str(),
        &args.filters,
        tests_passed,
        pending_count,
        &trace_paths,
        args.debug,
    );

    TestOutcome { tests_passed, pending_count, trace_paths }
}
