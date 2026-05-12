use clap::Args;

#[derive(Args, Clone, Debug)]
pub struct RunArgs {
    /// Create a new fixture directory with a template file
    #[arg(long, value_name = "NAME")]
    pub create: Option<String>,

    /// Delete a fixture directory (dry-run unless --confirm)
    #[arg(long, value_name = "NAME")]
    pub delete: Option<String>,

    /// Confirm destructive/global actions (required for unfiltered --accept and --delete)
    #[arg(long)]
    pub confirm: bool,

    /// Accept pending snapshots matching filters
    #[arg(long)]
    pub accept: bool,

    /// Reject pending snapshots matching filters
    #[arg(long)]
    pub reject: bool,

    /// Test name, timestamp, or slug filters (passed through to nextest)
    #[arg(num_args = 0..)]
    pub filters: Vec<String>,

    /// Verbose output (show test progress)
    #[arg(short, long)]
    pub verbose: bool,

    /// Enable tracing output for debugging
    #[arg(long)]
    pub debug: bool,

    /// Show full diffs (by default only shows summary to reduce output)
    #[arg(long)]
    pub diff: bool,

    /// Maximum number of snapshots to show (default: 3)
    #[arg(long, default_value = "3")]
    pub count: usize,

    /// Exclude snapshots matching pattern (substring match, repeatable)
    #[arg(long)]
    pub exclude: Vec<String>,
}
