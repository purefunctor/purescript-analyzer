use clap::Parser;
use compiler_scripts::test_runner::{RunArgs, TestCategory, run_category};

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
    let outcome = run_category(cli.category, &cli.args);

    if !outcome.tests_passed {
        std::process::exit(1);
    }
}
