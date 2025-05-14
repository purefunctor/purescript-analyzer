use std::sync::{Arc, Mutex};

use building::Runtime;
use files::Files;

use smol_str::SmolStrBuilder;
use tower_lsp::{Client, LanguageServer, LspService, Server, jsonrpc::Result, lsp_types::*};

struct Backend {
    files: Arc<Mutex<Files>>,
    runtime: Arc<Mutex<Runtime>>,
    client: Client,
}

impl Backend {
    fn new(client: Client) -> Backend {
        let files = Arc::new(Mutex::new(Files::default()));
        let runtime = Arc::new(Mutex::new(Runtime::default()));
        Backend { files, runtime, client }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let sources = std::fs::read_to_string(".spago/sources.txt").unwrap();
        for pattern in sources.lines() {
            for path in glob::glob(pattern).unwrap().filter_map(std::result::Result::ok) {
                let path = path.to_str().unwrap();
                self.client.log_message(MessageType::INFO, path).await;
                let uri = format!("file://{}", path);
                let contents: Arc<str> = std::fs::read_to_string(path).unwrap().into();
                self.on_change(&uri, contents).await;
            }
        }
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "purescript-analyzer is ready!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, p: DidOpenTextDocumentParams) {
        self.on_change(p.text_document.uri.as_str(), p.text_document.text.into()).await
    }

    async fn did_change(&self, p: DidChangeTextDocumentParams) {
        self.on_change(p.text_document.uri.as_str(), Arc::from(p.content_changes[0].text.as_str()))
            .await
    }
}

impl Backend {
    async fn on_change(&self, uri: &str, text: Arc<str>) {
        let id = self.files.lock().unwrap().insert(uri, text);
        let content = self.files.lock().unwrap().content(id);
        self.runtime.lock().unwrap().set_content(id, content);

        let _i = self.runtime.lock().unwrap().indexed(id);
        let _r = self.runtime.lock().unwrap().resolved(id);
        let _l = self.runtime.lock().unwrap().lowered(id);

        {
            let (parsed, errors) = self.runtime.lock().unwrap().parsed(id);
            self.client.log_message(MessageType::INFO, format!("Errors: {:?}", errors)).await;

            let cst = parsed.cst();
            if let Some(cst) = cst.header().and_then(|cst| cst.name()) {
                let mut builder = SmolStrBuilder::default();
                if let Some(token) = cst.qualifier().and_then(|cst| cst.text()) {
                    builder.push_str(token.text());
                }
                if let Some(token) = cst.name_token() {
                    builder.push_str(token.text());
                }
                let name = builder.finish();
                self.runtime.lock().unwrap().set_module_file(&name, id);
            }
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
