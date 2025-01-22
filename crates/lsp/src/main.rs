
use dashmap::DashMap;
use indexing::IndexingResult;
use lexing::{lex, Lexed};
use log::debug;
use parsing::parse;
use ropey::Rope;
use rowan::ast::AstNode;
use syntax::cst;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{
    lsp_types::{self, *},
    Client, LanguageServer, LspService, Server,
};

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: DashMap::new(),
        tokens_map: DashMap::new(),
        index_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

struct Backend {
    client: Client,
    document_map: DashMap<String, Rope>,
    tokens_map: DashMap<String, Lexed<'static>>,
    index_map: DashMap<String, IndexingResult>,
}

impl Backend {
    async fn on_change<'a>(&self, version: i32, uri: Url, text: &str) {
        let rope = ropey::Rope::from_str(text);
        self.document_map.insert(uri.to_string(), rope);
        let lexed: lexing::Lexed<'static> = lex(Box::leak(Box::new(text.to_string())));
        let lex_errors = lexed.errors();
        if !lex_errors.is_empty() {
            let diags = lex_errors
                .iter()
                .map(|err| Diagnostic {
                    range: Range {
                        start: lsp_position(lexed.position(err.index().try_into().unwrap())),
                        end: lsp_position(lexed.position(err.index().try_into().unwrap())),
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: err.message().to_string(),
                    source: Some("purescript lexing".to_string()),
                    ..Default::default()
                })
                .collect();
            self.client.publish_diagnostics(uri.clone(), diags, Some(version)).await;
        }
        self.tokens_map.insert(uri.to_string(), lexed.clone());
        let tokens: Vec<syntax::SyntaxKind> = lexing::layout(&lexed);
        let (node, parse_errors) = parse(&lexed, &tokens);
        let module = cst::Module::cast(node).unwrap();
        if !parse_errors.is_empty() {
            let client = self.client.clone();
            let uri = uri.clone();
            tokio::spawn(async move {
                let diags = parse_errors
                    .iter()
                    .map(|err| Diagnostic {
                        range: Range {
                            start: lsp_position(err.position),
                            end: lsp_position(err.position),
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: err.message.to_string(),
                        source: Some("purescript parsing".to_string()),
                        ..Default::default()
                    })
                    .collect();
                client.publish_diagnostics(uri, diags, Some(version)).await;
            });
        }
        let (index, index_errors) = indexing::index(&module);
        if !index_errors.is_empty() {
            let client = self.client.clone();
            let uri = uri.clone();
            tokio::spawn(async move {
                let diags = index_errors
                    .iter()
                    .map(|err| Diagnostic {
                        range: NULL_RANGE, // TODO
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: err.message(),
                        source: Some("purescript indexing".to_string()),
                        ..Default::default()
                    })
                    .collect();
                client.publish_diagnostics(uri, diags, Some(version)).await;
            });
        }
        self.index_map.insert(uri.to_string(), index);
    }
}

fn lsp_position(pos: lexing::Position) -> lsp_types::Position {
    let lexing::Position { line, column } = pos;
    lsp_types::Position { line, character: column }
}

const NULL_RANGE: Range =
    Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 0 } };

static LANGUAGE_ID: &str = "Purescript";

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                definition_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "Purescript lsp server initialized").await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        debug!("file opened");
        self.on_change(
            params.text_document.version,
            params.text_document.uri,
            &params.text_document.text,
        )
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(
            params.text_document.version,
            params.text_document.uri,
            &params.content_changes[0].text,
        )
        .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = || -> Option<GotoDefinitionResponse> {
            let uri = params.text_document_position_params.text_document.uri;
            let rope = self.document_map.get(uri.as_str())?;
            let position = params.text_document_position_params.position;
            let offset = position_to_offset(position, &rope)?;
            let tokens = self.tokens_map.get(uri.as_str())?;
            let name = tokens.text(offset);
            let index = self.index_map.get(uri.as_str())?;
            let ptr = index.source_map.lookup(name)?;
            todo!()
        }();

        Ok(definition)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_char_offset = rope.try_line_to_char(position.line as usize).ok()?;
    let slice = rope.slice(0..line_char_offset + position.character as usize);
    Some(slice.len_bytes())
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}
