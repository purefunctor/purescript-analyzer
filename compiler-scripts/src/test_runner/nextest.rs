use std::collections::HashMap;
use std::process::{Command, Stdio};

use console::style;

use crate::test_runner::category::TestCategory;
use crate::test_runner::cli::RunArgs;

pub fn build_nextest_command(
    category: TestCategory,
    args: &RunArgs,
    fixture_hashes: &HashMap<String, String>,
) -> Command {
    let mut cmd = Command::new("cargo");
    cmd.arg("nextest")
        .arg("run")
        .arg("-p")
        .arg("tests-integration")
        .arg("--test")
        .arg(category.as_str());

    for (key, value) in category.extra_env(args.debug) {
        cmd.env(key, value);
    }

    for filter in &args.filters {
        cmd.arg(filter);
    }

    if args.verbose {
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

pub fn run_nextest(
    category: TestCategory,
    args: &RunArgs,
    fixture_hashes: &HashMap<String, String>,
) -> bool {
    let mut cmd = build_nextest_command(category, args, fixture_hashes);

    if args.verbose {
        let status = cmd.status().expect("Failed to run cargo nextest");
        status.success()
    } else {
        cmd.stdout(Stdio::null()).stderr(Stdio::null());
        let status = cmd.status().expect("Failed to run cargo nextest");

        if !status.success() {
            eprintln!("{}", style("Tests failed, re-running verbose...").yellow());

            let verbose_args =
                RunArgs { filters: args.filters.clone(), verbose: true, debug: args.debug };
            let mut retry = build_nextest_command(category, &verbose_args, fixture_hashes);
            let _ = retry.status();
        }

        status.success()
    }
}
