use clap::Parser;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Parser)]
#[command(about, version(VERSION))]
struct Cli {
    #[arg(long)]
    stdio: bool,
}

#[tokio::main(flavor = "current_thread")]
async fn analyzer_main() {
    purescript_analyzer::analyzer_loop().await
}

fn main() {
    let _cli = Cli::parse();
    analyzer_main();
}
