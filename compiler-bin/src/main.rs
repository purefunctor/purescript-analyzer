use std::env;

use clap::Parser;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Parser)]
#[command(about, version(VERSION))]
struct Cli {
    #[arg(long)]
    stdio: bool,
    #[arg(long, help("Print log path"))]
    log_file: bool,
}

#[tokio::main(flavor = "current_thread")]
async fn analyzer_main() {
    purescript_analyzer::analyzer_loop().await
}

fn main() {
    let cli = Cli::parse();
    if cli.log_file {
        eprintln!("Log file: {:?}", env::temp_dir().join("purescript-analyzer.log"));
    }
    analyzer_main();
}
