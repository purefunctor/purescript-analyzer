use std::env;

use clap::Parser;
use purescript_analyzer::{logging, lsp};

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Parser)]
#[command(about, version(VERSION))]
struct Cli {
    #[arg(long)]
    stdio: bool,
    #[arg(long, help("Print log path"))]
    log_file: bool,
}

fn main() {
    let cli = Cli::parse();

    if cli.log_file {
        eprintln!("Log file: {:?}", logging::temporary_log_file());
    }

    async_main();
}

#[tokio::main(flavor = "current_thread")]
async fn async_main() {
    logging::start();
    lsp::start().await
}
