use std::{
    fs, panic,
    sync::{Arc, Mutex},
};

use building::Runtime;
use files::Files;

use lsp::spago;
use rowan::{TextSize, TokenAtOffset, ast::AstNode};
use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;
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
        let id = self.files.lock().unwrap().id(uri.as_str())?;
        let content = self.files.lock().unwrap().content(id);
        let resolved = self.runtime.lock().unwrap().resolved(id);

        let offset = 'offset: {
            let mut current_line = 0;
            let mut absolute_offset = 0;

            for line in content.split_inclusive("\n") {
                if current_line == position.line {
                    break 'offset absolute_offset + position.character;
                }

                absolute_offset += line.len() as u32;
                current_line += 1;
            }

            return None;
        };

        let (parsed, _) = self.runtime.lock().unwrap().parsed(id);
        let node = parsed.syntax_node();
        let token = node.token_at_offset(TextSize::new(offset));
        match token {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(token) => {
                let node = token.parent()?;
                let cst = cst::QualifiedName::cast(node)?;

                let qualifier = cst.qualifier().and_then(|cst| {
                    let token = cst.text()?;
                    let text = token.text();
                    let text = text.trim_end_matches(".");
                    Some(SmolStr::from(text))
                });

                let token = cst.lower()?;
                let name = token.text();

                let (f_id, t_id) = resolved.lookup_term(qualifier.as_deref(), name)?;

                let uri = Url::parse(&self.files.lock().unwrap().path(f_id)).ok()?;
                let content = self.runtime.lock().unwrap().content(f_id);
                let indexed = self.runtime.lock().unwrap().indexed(f_id);

                let range = indexed.term_item_range(t_id);

                let start: u32 = range.start().into();
                let end: u32 = range.end().into();

                let start = position_from_offset(&content, start);
                let end = position_from_offset(&content, end);
                let range = Range { start, end };

                Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
            }
            TokenAtOffset::Between(left, right) => {
                panic!("{:?} - {:?} - {:?} - {:?}", position, offset, left.text(), right.text());
            }
        }
    }
}

fn position_from_offset(source: &str, offset: u32) -> Position {
    let mut current_offset = 0;
    let mut line_number = 0;

    for line in source.split_inclusive('\n') {
        let line_length = line.len() as u32;

        if current_offset + line_length > offset {
            let line_offset = offset - current_offset;
            return Position::new(line_number, line_offset);
        }

        current_offset += line_length;
        line_number += 1;
    }

    if let Some(last_line) = source.split_inclusive('\n').last() {
        return Position::new(line_number - 1, last_line.len() as u32);
    }

    Position::new(0, 0)
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
