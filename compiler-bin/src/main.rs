use std::env;

use clap::Parser;
use purescript_analyzer::{analyzer_loop, logging};

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
    analyzer_loop().await
}

fn main() {
    let cli = Cli::parse();
    if cli.log_file {
        eprintln!("Log file: {:?}", logging::temporary_log_file());
    }

    logging::initialize();
    analyzer_main();
}
