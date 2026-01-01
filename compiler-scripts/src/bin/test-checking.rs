use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Instant;
use std::{env, fs};

use clap::Parser;
use compiler_scripts::console::style;
use compiler_scripts::fixtures::fixture_env;
use compiler_scripts::snapshots::{print_diff, strip_frontmatter};

#[derive(Parser)]
#[command(about = "Run type checker integration tests with snapshot diffing")]
struct Args {
    /// Test name or number filter
    filter: Option<String>,

    /// Verbose output (show test progress)
    #[arg(short, long)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();
    let verbose = args.verbose;
    let test_filter = args.filter.unwrap_or_default();

    // Hash fixtures and print timing
    let start = Instant::now();
    let fixture_hashes = fixture_env();
    println!("{}", style(format!("Hashed fixtures in {}ms", start.elapsed().as_millis())).dim());

    // Build cargo nextest command
    let mut cmd = Command::new("cargo");
    cmd.arg("nextest").arg("run").arg("-p").arg("tests-integration").arg("--test").arg("checking");

    if !test_filter.is_empty() {
        cmd.arg(&test_filter);
    }

    if verbose {
        cmd.arg("--status-level=fail");
        cmd.arg("--color=always");
    } else {
        cmd.arg("--status-level=none");
    }

    // Set environment variables
    cmd.env("INSTA_FORCE_PASS", "1");
    for (key, value) in &fixture_hashes {
        cmd.env(key, value);
    }

    // Run tests
    let _status = if verbose {
        cmd.status().expect("Failed to run cargo nextest")
    } else {
        // Capture output in quiet mode
        cmd.stdout(Stdio::null()).stderr(Stdio::null());
        let status = cmd.status().expect("Failed to run cargo nextest");

        if !status.success() {
            // Re-run verbose to show errors
            eprintln!("{}", style("Tests failed, re-running verbose...").yellow());
            let mut retry = Command::new("cargo");
            retry
                .arg("nextest")
                .arg("run")
                .arg("-p")
                .arg("tests-integration")
                .arg("--test")
                .arg("checking");
            if !test_filter.is_empty() {
                retry.arg(&test_filter);
            }
            retry.arg("--status-level=fail");
            retry.arg("--color=always");
            retry.env("INSTA_FORCE_PASS", "1");
            for (key, value) in &fixture_hashes {
                retry.env(key, value);
            }
            let _ = retry.status();
        }
        status
    };

    // Get pending snapshots (suppress stderr)
    let pending_output = Command::new("cargo")
        .arg("insta")
        .arg("pending-snapshots")
        .stderr(Stdio::null())
        .output()
        .expect("Failed to run cargo insta");

    let pending = String::from_utf8_lossy(&pending_output.stdout);
    let pending = pending.trim();

    if pending.is_empty() {
        println!("{}", style("No pending snapshots!"));
        return;
    }

    println!();

    let cwd = env::current_dir().unwrap();

    for snap_path in pending.lines() {
        let snap_path = snap_path.trim();
        if snap_path.is_empty() {
            continue;
        }

        let short_path = snap_path
            .strip_prefix(cwd.to_str().unwrap_or(""))
            .unwrap_or(snap_path)
            .trim_start_matches('/');

        let snap = Path::new(snap_path);
        let snap_new = format!("{}.new", snap_path);

        if snap.exists() {
            // Updated snapshot
            println!("{} {}\n", style("UPDATED").yellow().bold(), style(short_path).cyan());

            let old_content = fs::read_to_string(snap).unwrap_or_default();
            let new_content = fs::read_to_string(&snap_new).unwrap_or_default();

            let old_stripped = strip_frontmatter(&old_content);
            let new_stripped = strip_frontmatter(&new_content);

            print_diff(old_stripped, new_stripped);
        } else {
            // New snapshot
            println!("{} {}\n", style("CREATED").green().bold(), style(short_path).cyan());

            let new_content = fs::read_to_string(&snap_new).unwrap_or_default();
            for (i, line) in strip_frontmatter(&new_content).lines().enumerate() {
                println!("{}  {}", style(format!("{:3}", i + 1)).dim(), line);
            }
        }

        println!();
    }
}
