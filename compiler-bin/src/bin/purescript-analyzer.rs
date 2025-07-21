use clap::Parser;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Parser)]
#[command(about, version(VERSION))]
struct Cli {}

#[tokio::main(flavor = "current_thread")]
async fn analyzer_main() {
    compiler_bin::analyzer_loop().await
}

fn main() {
    let _cli = Cli::parse();
    analyzer_main();
}
