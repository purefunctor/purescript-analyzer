use clap::{Args, Subcommand};

#[derive(Subcommand, Clone, Debug)]
pub enum CategoryCommand {
    /// Accept pending snapshots for this category
    Accept(SnapshotArgs),
    /// Reject pending snapshots for this category
    Reject(SnapshotArgs),
}

#[derive(Args, Clone, Debug)]
pub struct SnapshotArgs {
    /// Snapshot filters (same as test filters)
    #[arg(num_args = 0..)]
    pub filters: Vec<String>,

    /// Accept/reject all pending snapshots (required when no filters provided)
    #[arg(long)]
    pub all: bool,
}

#[derive(Args, Clone, Debug)]
pub struct RunArgs {
    /// Subcommand (accept/reject) or test filters
    #[command(subcommand)]
    pub command: Option<CategoryCommand>,

    /// Test name or number filters (passed through to nextest)
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
