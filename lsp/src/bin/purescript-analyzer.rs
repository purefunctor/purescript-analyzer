use std::fs::OpenOptions;

use lsp::Backend;
use tower_lsp::{LspService, Server};
use tracing::Level;

#[tokio::main]
async fn main() {
    let log_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("/tmp/purescript-analyzer.log")
        .expect("Failed to open log file");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    tracing_subscriber::fmt().with_max_level(Level::INFO).with_writer(log_file).init();
    let (service, socket) = LspService::new(Backend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}
