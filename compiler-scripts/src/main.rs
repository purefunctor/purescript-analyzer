use clap::Parser;
use compiler_scripts::test_runner::{
    CategoryCommand, RunArgs, TestCategory, accept_category, reject_category, run_category,
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

    match &cli.args.command {
        Some(CategoryCommand::Accept(args)) => {
            let outcome = accept_category(cli.category, args);
            if !outcome.success {
                std::process::exit(1);
            }
        }
        Some(CategoryCommand::Reject(args)) => {
            let outcome = reject_category(cli.category, args);
            if !outcome.success {
                std::process::exit(1);
            }
        }
        None => {
            let outcome = run_category(cli.category, &cli.args);
            if !outcome.tests_passed {
                std::process::exit(1);
            }
        }
    }
}
