use clap::Args;

#[derive(Args, Clone, Debug)]
pub struct RunArgs {
    /// Test name or number filters (passed through to nextest)
    #[arg(num_args = 0..)]
    pub filters: Vec<String>,

    /// Verbose output (show test progress)
    #[arg(short, long)]
    pub verbose: bool,

    /// Enable tracing output for debugging
    #[arg(long)]
    pub debug: bool,
}
