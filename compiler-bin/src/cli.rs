use clap::{ArgAction, Parser};
use clap::ValueEnum;
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
    #[arg(
        long,
        value_name("LevelFilter"),
        help("Log level for the type checker"),
        default_value("off")
    )]
    pub checking_log: LevelFilter,
    #[arg(
        long,
        help("Command to use to get source files"),
        long_help("This argument also disables the spago.lock integration")
    )]
    pub source_command: Option<String>,

    #[arg(
        long,
        help("Publish diagnostics on textDocument/didOpen"),
        value_name("bool"),
        action = ArgAction::Set,
        default_value_t = true
    )]
    pub diagnostics_on_open: bool,

    #[arg(
        long,
        help("Publish diagnostics on textDocument/didSave"),
        value_name("bool"),
        action = ArgAction::Set,
        default_value_t = true
    )]
    pub diagnostics_on_save: bool,

    #[arg(
        long,
        help("Publish diagnostics on textDocument/didChange (opt-in)"),
        default_value_t = false
    )]
    pub diagnostics_on_change: bool,

    #[arg(
        long,
        value_enum,
        default_value_t = BuildTool::Auto,
        help("Build tool used for purescript.build")
    )]
    pub build_tool: BuildTool,

    #[arg(
        long,
        help("Extra args appended to the build command (repeatable)"),
        value_name("arg")
    )]
    pub build_arg: Vec<String>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum BuildTool {
    Auto,
    Spago,
    Purs,
}
