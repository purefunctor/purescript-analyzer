use clap::Parser;
use tracing::level_filters::LevelFilter;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Parser)]
#[command(about, version(VERSION))]
pub struct Config {
    #[arg(long)]
    pub stdio: bool,
    #[arg(long, help("Print log path"))]
    pub log_file: bool,
    #[arg(
        long,
        value_name("LevelFilter"),
        help("Log level for the query engine"),
        default_value("off")
    )]
    pub query_log: LevelFilter,
    #[arg(
        long,
        value_name("LevelFilter"),
        help("Log level for the language server"),
        default_value("info")
    )]
    pub lsp_log: LevelFilter,
}
