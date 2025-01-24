use dashmap::DashMap;
use indexing::{ExprItem, IndexingResult};
use lexing::{lex, Lexed};
use log::debug;
use parsing::parse;
use ropey::Rope;
use rowan::ast::AstNode;
use rowan::TextRange;
use syntax::cst;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{
    lsp_types::{self, *},
    Client, LanguageServer,
};

pub struct Backend {
    client: Client,
    document_map: DashMap<String, Rope>,
    tokens_map: DashMap<String, Lexed<'static>>,
    index_map: DashMap<String, IndexingResult>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            document_map: DashMap::new(),
            tokens_map: DashMap::new(),
            index_map: DashMap::new(),
        }
    }

    pub async fn analyze<'a>(&self, version: i32, uri: Url, text: &str) {
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
        println!("lexed inserted");
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
        self.index_map.insert(uri.to_string(), index);
        println!("index inserted");
        if !index_errors.is_empty() {
            let client = self.client.clone();
            let uri = uri.clone();
            tokio::spawn(async move {
                let diags = index_errors
                    .iter()
                    .map(|_err| Diagnostic {
                        range: NULL_RANGE, // TODO
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: "TODO".to_string(), // TODO
                        source: Some("purescript indexing".to_string()),
                        ..Default::default()
                    })
                    .collect();
                client.publish_diagnostics(uri, diags, Some(version)).await;
            });
        }
    }
}

fn lsp_position(pos: lexing::Position) -> lsp_types::Position {
    let lexing::Position { line, column } = pos;
    lsp_types::Position { line, character: column }
}

const NULL_RANGE: Range =
    Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 0 } };

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
        self.analyze(
            params.text_document.version,
            params.text_document.uri,
            &params.text_document.text,
        )
        .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.analyze(
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
            let name = tokens.text_at_offset(offset.try_into().unwrap())?;
            let index = self.index_map.get(uri.as_str())?;
            let (_, expr) = index.nominal.lookup_expr_item(name)?;
            let decl_id = match expr {
                // todo add type level and other exprs
                ExprItem::Derive(id) => *id,
                ExprItem::Operator(id) => *id,
                ExprItem::Value(id) => {
                    let mut decl_ids = id.declaration_ids();
                    decl_ids.next().unwrap()
                }
                _ => return None,
            };
            let ptr = index.source_map.declaration_ptr(decl_id)?;
            let text_range_with_space = ptr.syntax_node_ptr().text_range();
            let text_range = trim_range_whitespace(text_range_with_space, tokens.source);
            let range = text_range_to_range(text_range, &rope)?;
            Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
        }();

        Ok(definition)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_char_offset = rope.try_line_to_char((position.line - 1) as usize).ok()?;
    let slice = rope.slice(1..line_char_offset + position.character as usize);
    Some(slice.len_bytes())
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new((line + 1) as u32, (column + 1) as u32))
}

fn text_range_to_range(text_range: TextRange, rope: &Rope) -> Option<Range> {
    let start = offset_to_position(text_range.start().into(), rope)?;
    let end = offset_to_position(text_range.end().into(), rope)?;
    Some(Range { start, end })
}

fn trim_range_whitespace(text_range: TextRange, str: &str) -> TextRange {
   let mut start: usize = text_range.start().into();
   let mut end: usize = text_range.end().into();
    while str[start..end].starts_with(char::is_whitespace) {
        start += 1;
    }

    while str[start..end].ends_with(char::is_whitespace) {
        end -= 1;
    }
    TextRange::new(start.try_into().unwrap(), end.try_into().unwrap())
}
