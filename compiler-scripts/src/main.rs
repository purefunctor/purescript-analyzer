use clap::Parser;
use console::style;

use compiler_scripts::test_runner::{
    CategoryCommand, DeleteFixtureOutcome, RunArgs, TestCategory, accept_category, create_fixture,
    delete_fixture, reject_category, run_category,
};

#[derive(Parser)]
#[command(about = "Compiler development scripts")]
struct Cli {
    /// Test category: checking (c), lowering (l), resolving (r), lsp
    category: TestCategory,

    #[command(flatten)]
    args: RunArgs,
}

fn main() {
    let cli = Cli::parse();

    if let Some(name) = &cli.args.create {
        if let Err(error) = create_fixture(cli.category, name) {
            eprintln!("{:#}", error);
            std::process::exit(1);
        }
        return;
    }

    if let Some(name) = &cli.args.delete {
        match delete_fixture(cli.category, name, cli.args.confirm) {
            Ok(DeleteFixtureOutcome { fixture_paths, snapshot_paths, confirmed }) => {
                if confirmed {
                    for path in &fixture_paths {
                        println!(
                            "{} {}",
                            style("DELETED").red().bold(),
                            style(path.display()).cyan()
                        );
                    }
                    for path in &snapshot_paths {
                        println!(
                            "{} {}",
                            style("DELETED").red().bold(),
                            style(path.display()).cyan()
                        );
                    }
                } else {
                    println!(
                        "{} pending deletion(s) in {}",
                        fixture_paths.len() + snapshot_paths.len(),
                        style(cli.category.as_str()).cyan()
                    );
                    println!();
                    for path in &fixture_paths {
                        println!("  {}", style(path.display()).dim());
                    }
                    for path in &snapshot_paths {
                        println!("  {}", style(path.display()).dim());
                    }
                    println!();
                    println!(
                        "To delete, run: {}",
                        style(format!(
                            "just t {} --delete \"{}\" --confirm",
                            cli.category.as_str(),
                            name
                        ))
                        .cyan()
                    );
                }
            }
            Err(error) => {
                eprintln!("{:#}", error);
                std::process::exit(1);
            }
        }
        return;
    }

    match &cli.args.command {
        Some(CategoryCommand::Accept(args)) => {
            let outcome = match accept_category(cli.category, args) {
                Ok(outcome) => outcome,
                Err(error) => {
                    eprintln!("{:#}", error);
                    std::process::exit(1);
                }
            };
            if !outcome.success {
                std::process::exit(1);
            }
        }
        Some(CategoryCommand::Reject(args)) => {
            let outcome = match reject_category(cli.category, args) {
                Ok(outcome) => outcome,
                Err(error) => {
                    eprintln!("{:#}", error);
                    std::process::exit(1);
                }
            };
            if !outcome.success {
                std::process::exit(1);
            }
        }
        None => {
            let outcome = match run_category(cli.category, &cli.args) {
                Ok(outcome) => outcome,
                Err(error) => {
                    eprintln!("{:#}", error);
                    std::process::exit(1);
                }
            };
            if !outcome.tests_passed {
                std::process::exit(1);
            }
        }
    }
}
