use std::fs;
use std::path::{Path, PathBuf};

use crate::console::style;
use crate::snapshots::{print_diff, strip_frontmatter};

pub fn display_snapshot_diff(
    snap: &Path,
    snap_new: &Path,
    short_path: &str,
    trace_path: Option<&Path>,
) {
    println!();
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

pub fn display_new_snapshot(snap_new: &Path, short_path: &str, trace_path: Option<&Path>) {
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

pub fn print_next_actions(
    category_name: &str,
    filters: &[String],
    tests_passed: bool,
    pending_count: usize,
    trace_paths: &[PathBuf],
    debug: bool,
) {
    let filters_str =
        if filters.is_empty() { String::new() } else { format!(" {}", filters.join(" ")) };

    if tests_passed && pending_count == 0 {
        println!("{}", style("No pending snapshots.").green());
        return;
    }

    if pending_count > 0 {
        println!("{}", style("-".repeat(60)).dim());
        println!();
        println!("{} pending snapshot{}", pending_count, if pending_count == 1 { "" } else { "s" });
        println!();
        println!("  If this is expected, run {}", style("cargo insta accept").cyan());
        println!("  If this is not expected, run {}", style("cargo insta reject").cyan());
        println!();
    }

    if !tests_passed {
        println!("{}", style("-".repeat(60)).dim());
        println!();
        println!("{}", style("Tests failed, consider consulting trace files.").red());
        if !debug {
            println!(
                "Enable debug tracing for more information: {}",
                style(format!("just t {} --debug{}", category_name, filters_str)).cyan()
            );
        }
        if !trace_paths.is_empty() {
            let maximum_shown = 10;
            for trace in trace_paths.iter().take(maximum_shown) {
                println!("{} {}", style("TRACE").magenta().bold(), style(trace.display()).cyan());
            }
            if trace_paths.len() > maximum_shown {
                let additional = trace_paths.len() - maximum_shown;
                let additional = style(format!(
                    "An additional {additional} is stored in target/compiler-tracing"
                ))
                .dim();
                println!("{additional}");
            }
        }
        println!();
    }
}
