mod capability;

use std::{
    fs,
    sync::{Arc, Mutex},
};

use building::Runtime;
use files::Files;
use smol_str::SmolStrBuilder;
use tower_lsp::{Client, LanguageServer, jsonrpc::Result, lsp_types::*};

use crate::spago;

pub struct Backend {
    files: Arc<Mutex<Files>>,
    runtime: Arc<Mutex<Runtime>>,
    client: Client,
}

impl Backend {
    pub fn new(client: Client) -> Backend {
        let files = Arc::new(Mutex::new(Files::default()));
        let runtime = Arc::new(Mutex::new(Runtime::default()));
        Backend { files, runtime, client }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                definition_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "purescript-analyzer is looking for files.")
            .await;
        if let Ok(files) = spago::source_files() {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("purescript-analyzer loading {} files.", files.len()),
                )
                .await;
            for file in &files {
                let uri = format!("file://{}", file.to_str().unwrap());
                let contents = fs::read_to_string(file).unwrap().into();
                self.on_change(&uri, contents).await;
            }
            self.client
                .log_message(
                    MessageType::INFO,
                    format!("purescript-analyzer loaded {} files.", files.len()),
                )
                .await;
        }
        self.client.log_message(MessageType::INFO, "purescript-analyzer is ready!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change(&self, p: DidChangeTextDocumentParams) {
        let uri = p.text_document.uri.as_str();
        let text = p.content_changes[0].text.as_str().into();
        self.on_change(uri, text).await
    }

    async fn goto_definition(
        &self,
        p: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = p.text_document_position_params.text_document.uri;
        let position = p.text_document_position_params.position;
        let result = self.definition(uri, position).await;
        Result::Ok(result)
    }
}

impl Backend {
    async fn on_change(&self, uri: &str, text: Arc<str>) {
        let id = self.files.lock().unwrap().insert(uri, text);
        let content = self.files.lock().unwrap().content(id);
        self.runtime.lock().unwrap().set_content(id, content);
        {
            let (parsed, _) = self.runtime.lock().unwrap().parsed(id);
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

    async fn definition(&self, uri: Url, position: Position) -> Option<GotoDefinitionResponse> {
        capability::definition(self, uri, position).await
    }
}
