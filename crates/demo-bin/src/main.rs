use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use demo::infer::InferDatabase;
use demo::source::SourceDatabase;
use demo::surface::SurfaceDatabase;
use demo::RootDatabase;
use files::{ChangedFile, Files};
use glob::glob;
use rowan::ast::AstNode;
use rowan::{TextSize, TokenAtOffset};
use syntax::ast;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{
    DidChangeTextDocumentParams, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializedParams, MarkedString, MessageType, Position, Range,
    ServerCapabilities, TextDocumentSyncKind,
};
use tower_lsp::{lsp_types::InitializeResult, Client, LanguageServer};
use tower_lsp::{LspService, Server};

struct Analyzer {
    client: Client,
    db: Arc<Mutex<RootDatabase>>,
    files: Arc<Mutex<Files>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Analyzer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let mut db = self.db.lock().await;
        let mut files = self.files.lock().await;

        // FIXME: support non-folder editing
        let project_folder =
            params.root_uri.map(|url| PathBuf::from(url.path())).unwrap_or(PathBuf::new());

        let mut source_paths = vec![];
        for path in glob("**/*.purs").unwrap().flatten() {
            let path = project_folder.join(path);
            eprintln!("Loading {:?}", path);
            let contents = fs::read(path.clone()).unwrap();
            files.set_file_contents(path.clone(), Some(contents));
            source_paths.push(files.file_id(path))
        }

        for ChangedFile { file_id, .. } in files.take_changes() {
            let contents = files.file_contents(file_id);
            db.set_file_source(file_id, std::str::from_utf8(contents).unwrap().into());
        }

        eprintln!("Loaded files...");

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(tower_lsp::lsp_types::TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..Default::default()
            },
            server_info: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "purescript-analyzer initialized.").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let db = self.db.lock().await;
        let files = self.files.lock().await;

        let path = params.text_document_position_params.text_document.uri.path();
        let file_id = files.file_id(path.into()).ok_or(Error::internal_error())?;

        let byte_offset = byte_offset(
            &db.file_source(file_id),
            params.text_document_position_params.position.line,
            params.text_document_position_params.position.character,
        );

        let syntax = db.parse_file(file_id).syntax.clone();

        let token_at_offset = syntax.token_at_offset(TextSize::new(byte_offset));

        let significant_token = match dbg!(token_at_offset) {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(m) => Some(m),
            TokenAtOffset::Between(l, r) => {
                if !l.kind().is_whitespace_or_comment() {
                    Some(l)
                } else if !r.kind().is_whitespace_or_comment() {
                    Some(r)
                } else {
                    None
                }
            }
        };

        if let Some(significant_token) = dbg!(significant_token) {
            let declaration: Option<ast::ValueDeclaration> = significant_token
                .parent_ancestors()
                .find_map(|ancestor| ast::ValueDeclaration::cast(ancestor));

            if let Some(declaration) = declaration {
                let declaration_id =
                    db.positional_map(file_id).lookup(&declaration).in_file(file_id);
                let infer_result = db.infer_value_declaration(declaration_id);
                let (data, source_map) = db.lower_value_declaration_with_source_map(declaration_id);

                let expression: Option<ast::Expression> =
                    significant_token.parent_ancestors().find_map(ast::Expression::cast);

                if let Some(expression) = expression {
                    let expr_id = source_map.get_expr_id(&expression.syntax()).unwrap();
                    let expr_ty = infer_result.expr_type.get(&expr_id).unwrap();

                    let text_range = expression.syntax().text_range();
                    let start = line_col(&db.file_source(file_id), text_range.start().into());
                    let end = line_col(&db.file_source(file_id), text_range.end().into());

                    return Ok(Some(Hover {
                        contents: tower_lsp::lsp_types::HoverContents::Scalar(
                            MarkedString::from_language_code(
                                "purescript".into(),
                                format!("{:?}", expr_ty),
                            ),
                        ),
                        range: Some(Range::new(start, end)),
                    }));
                } else {
                    let expr_ty = infer_result.expr_type.get(&data.expr_id).unwrap();

                    let text_range = declaration.syntax().text_range();
                    let start = line_col(&db.file_source(file_id), text_range.start().into());
                    let end = line_col(&db.file_source(file_id), text_range.end().into());

                    return Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::from_language_code(
                            "purescript".into(),
                            format!("{} :: {:?}", data.name, expr_ty),
                        )),
                        range: Some(Range::new(start, end)),
                    }));
                }
            }

            Ok(None)
        } else {
            Ok(None)
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("did_change: ['{}']", params.text_document.uri.path()),
            )
            .await;

        let mut db = self.db.lock().await;
        let mut files = self.files.lock().await;

        for change_event in &params.content_changes {
            let path = params.text_document.uri.path().into();
            let contents = change_event.text.as_bytes().into();
            files.set_file_contents(path, Some(contents));
        }

        for ChangedFile { file_id, .. } in files.take_changes() {
            let contents = std::str::from_utf8(files.file_contents(file_id)).unwrap().into();
            db.set_file_source(file_id, contents);
        }

        let text = {
            let path = params.text_document.uri.path();
            let file_id = files.file_id(path.into()).unwrap();
            let node = db.parse_file(file_id).syntax.clone();
            format!("['{path}']: {node:#?}")
        };

        self.client.log_message(MessageType::INFO, text).await;
    }
}

fn byte_offset(source: &str, line: u32, column: u32) -> u32 {
    let mut byte_offset = 0;
    let mut current_line = 0;
    let mut current_column = 0;

    for (i, c) in source.char_indices() {
        if current_line == line && current_column == column {
            break;
        }

        byte_offset = (i + c.len_utf8()) as u32;

        if c == '\n' {
            current_line += 1;
            current_column = 0;
        } else {
            current_column += 1;
        }
    }

    byte_offset
}

fn line_col(source: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut column = 0;

    for (i, c) in source.char_indices() {
        if i >= offset {
            break;
        }

        if c == '\n' {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }

    Position::new(line, column)
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| {
        let db = Arc::new(Mutex::new(RootDatabase::default()));
        let files = Arc::new(Mutex::new(Files::default()));
        Analyzer { client, db, files }
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
