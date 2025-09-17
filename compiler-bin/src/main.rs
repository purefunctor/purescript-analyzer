use clap::Parser;
use purescript_analyzer::{cli, logging, lsp};

fn main() {
    let config = cli::Config::parse();

    if config.log_file {
        eprintln!("Log file: {:?}", logging::temporary_log_file());
    }

    async_main(config);
}

#[tokio::main(flavor = "current_thread")]
async fn async_main(config: cli::Config) {
    logging::start(config);
    lsp::start().await
}
