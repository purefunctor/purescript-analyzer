use std::fs;
use std::path::{Path, PathBuf};

use similar::{ChangeTag, TextDiff};

use crate::console::style;
use crate::snapshots::{print_diff, strip_frontmatter};
use crate::test_runner::decision;
use crate::test_runner::decision::{
    DecisionInput, FailureDecision, NextAction, Outcome, PendingDecision,
};

pub struct SnapshotStats {
    pub added: usize,
    pub removed: usize,
}

fn count_diff_lines(old: &str, new: &str) -> SnapshotStats {
    let diff = TextDiff::from_lines(old, new);
    let mut added = 0;
    let mut removed = 0;

    for change in diff.iter_all_changes() {
        match change.tag() {
            ChangeTag::Delete => removed += 1,
            ChangeTag::Insert => added += 1,
            ChangeTag::Equal => {}
        }
    }

    SnapshotStats { added, removed }
}

pub fn display_snapshot_diff(
    snap: &Path,
    snap_new: &Path,
    short_path: &str,
    trace_path: Option<&Path>,
    show_diff: bool,
) -> SnapshotStats {
    let old_content = fs::read_to_string(snap).unwrap_or_default();
    let new_content = fs::read_to_string(snap_new).unwrap_or_default();

    let old_stripped = strip_frontmatter(&old_content);
    let new_stripped = strip_frontmatter(&new_content);

    let stats = count_diff_lines(old_stripped, new_stripped);

    print!("{} {}", style("UPDATED").yellow().bold(), style(short_path).cyan());
    println!(
        " ({}, {})",
        style(format!("+{}", stats.added)).green(),
        style(format!("-{}", stats.removed)).red()
    );

    if let Some(trace) = trace_path {
        println!("  {} {}", style("TRACE").magenta().bold(), style(trace.display()).cyan());
    }

    if show_diff {
        println!();
        print_diff(old_stripped, new_stripped);
        println!();
    }

    stats
}

pub fn display_new_snapshot(
    snap_new: &Path,
    short_path: &str,
    trace_path: Option<&Path>,
    show_diff: bool,
) -> SnapshotStats {
    let new_content = fs::read_to_string(snap_new).unwrap_or_default();
    let new_stripped = strip_frontmatter(&new_content);
    let line_count = new_stripped.lines().count();

    print!("{} {}", style("CREATED").green().bold(), style(short_path).cyan());
    println!(" ({})", style(format!("+{}", line_count)).green());

    if let Some(trace) = trace_path {
        println!("  {} {}", style("TRACE").magenta().bold(), style(trace.display()).cyan());
    }

    if show_diff {
        println!();
        for (i, line) in new_stripped.lines().enumerate() {
            println!("{}  {}", style(format!("{:3}", i + 1)).dim(), line);
        }
        println!();
    }

    SnapshotStats { added: line_count, removed: 0 }
}

pub struct NextActionsArgs<'a> {
    pub category_name: &'a str,
    pub filters: &'a [String],
    pub tests_passed: bool,
    pub pending_count: usize,
    pub excluded_count: usize,
    pub total_lines_changed: usize,
    pub trace_paths: &'a [PathBuf],
    pub debug: bool,
    pub showed_diffs: bool,
}

pub fn print_next_actions(args: NextActionsArgs<'_>) {
    let NextActionsArgs {
        category_name,
        filters,
        tests_passed,
        pending_count,
        excluded_count,
        total_lines_changed,
        trace_paths,
        debug,
        showed_diffs,
    } = args;

    let ran_all = filters.is_empty();
    let filters_str = if ran_all { String::new() } else { format!(" {}", filters.join(" ")) };

    let input = DecisionInput {
        tests_passed,
        pending_count,
        total_lines_changed,
        showed_diffs,
        ran_all,
        debug,
        trace_count: trace_paths.len(),
        max_count: 3, // not used for outcome decisions
    };

    match decision::decide_outcome(&input) {
        Outcome::Clean => {
            println!("{}", style("All tests passed, no pending snapshots.").green());
        }
        Outcome::Failure(decision) => {
            render_failure(&decision, category_name, &filters_str, trace_paths);
        }
        Outcome::Pending(decision) => {
            render_pending(
                &decision,
                category_name,
                &filters_str,
                pending_count,
                excluded_count,
                total_lines_changed,
            );
        }
    }
}

fn render_failure(
    decision: &FailureDecision,
    category_name: &str,
    filters_str: &str,
    trace_paths: &[PathBuf],
) {
    println!("{}", style("-".repeat(60)).dim());
    println!();
    println!("{}", style("Tests failed.").red());

    if decision.show_debug_hint {
        println!(
            "  Next: {}",
            style(format!("just t {} --debug{}", category_name, filters_str)).cyan()
        );
    } else if decision.show_trace_hint {
        println!("  Next: consult trace files below");
    }

    if !trace_paths.is_empty() {
        println!();
        for trace in trace_paths.iter().take(decision.max_traces_to_show) {
            println!("  {} {}", style("TRACE").magenta().bold(), style(trace.display()).cyan());
        }
        if trace_paths.len() > decision.max_traces_to_show {
            let hidden = trace_paths.len() - decision.max_traces_to_show;
            println!("  {}", style(format!("...and {} more trace file(s)", hidden)).dim());
        }
    }

    if let Some(count) = decision.pending_note {
        println!();
        println!(
            "  {}",
            style(format!("Note: {} pending snapshot(s); review after fixing failures", count))
                .dim()
        );
    }

    println!();
}

fn render_pending(
    decision: &PendingDecision,
    category_name: &str,
    filters_str: &str,
    pending_count: usize,
    excluded_count: usize,
    total_lines_changed: usize,
) {
    println!("{}", style("-".repeat(60)).dim());
    println!();

    let header = if decision.show_lines_changed {
        format!("{} pending snapshot(s), {} lines changed", pending_count, total_lines_changed)
    } else if excluded_count > 0 {
        format!(
            "{} pending snapshot{} ({} excluded)",
            pending_count,
            if pending_count == 1 { "" } else { "s" },
            excluded_count
        )
    } else {
        format!("{} pending snapshot{}", pending_count, if pending_count == 1 { "" } else { "s" })
    };
    println!("{}", header);
    println!();

    match decision.next_action {
        NextAction::AcceptOrReject => {
            let accept_cmd = format_accept_reject_cmd(category_name, filters_str, "accept");
            let reject_cmd = format_accept_reject_cmd(category_name, filters_str, "reject");
            println!("  Next: {}", style(&accept_cmd).green());
            println!("    Or: {}", style(&reject_cmd).red());
        }
        NextAction::ReviewSubset => {
            println!("  Next: {}", style(format!("just t {} NNN --diff", category_name)).cyan());
            println!("  {}", style("Hint: Review 1-2 tests at a time").dim());
        }
        NextAction::ShowDiff => {
            println!(
                "  Next: {}",
                style(format!("just t {}{} --diff", category_name, filters_str)).cyan()
            );
        }
    }

    println!();
}

fn format_accept_reject_cmd(category_name: &str, filters_str: &str, action: &str) -> String {
    if filters_str.is_empty() {
        format!("just t {} {} --all", category_name, action)
    } else {
        format!("just t {} {}{}", category_name, action, filters_str)
    }
}
