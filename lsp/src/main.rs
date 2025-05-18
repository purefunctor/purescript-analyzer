use std::{
    fs,
    sync::{Arc, Mutex},
};

use building::Runtime;
use files::Files;
use lsp::{locate, spago};
use rowan::ast::AstNode;
use smol_str::SmolStrBuilder;
use syntax::{SyntaxKind, SyntaxNode, SyntaxNodePtr, cst};
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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
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

    async fn hover(&self, p: HoverParams) -> Result<Option<Hover>> {
        let uri = p.text_document_position_params.text_document.uri;
        let position = p.text_document_position_params.position;
        let result = self.hover(uri, position).await;
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

    async fn hover(&self, uri: Url, position: Position) -> Option<Hover> {
        let uri = uri.as_str();

        let (id, content) = {
            let files = self.files.lock().unwrap();
            let id = files.id(uri)?;
            let content = files.content(id);
            (id, content)
        };

        let (resolved, parsed) = {
            let mut runtime = self.runtime.lock().unwrap();
            let resolved = runtime.resolved(id);
            let (parsed, _) = runtime.parsed(id);
            (resolved, parsed)
        };

        let thing = locate::thing_at_position(&content, parsed, position);
        let cst = thing.as_qualified_name()?;

        let qualifier_token = cst.qualifier().and_then(|cst| cst.text());
        let name_token = cst.lower().or_else(|| cst.upper())?;

        let prefix = qualifier_token.as_ref().map(|cst| cst.text().trim_end_matches("."));
        let name = name_token.text();

        let (f_id, t_id) = resolved.lookup_term(prefix, name)?;

        let (parsed, indexed) = {
            let mut runtime = self.runtime.lock().unwrap();
            let (parsed, _) = runtime.parsed(f_id);
            let indexed = runtime.indexed(f_id);
            (parsed, indexed)
        };

        let t_ptr = indexed.term_item_ptr(t_id);
        if let [t_ptr, ..] = &t_ptr[..] {
            let root = parsed.syntax_node();
            let value = find_annotation(t_ptr, root)?;
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: None,
            })
        } else {
            None
        }
    }

    async fn definition(&self, uri: Url, position: Position) -> Option<GotoDefinitionResponse> {
        let uri = uri.as_str();

        let (id, content) = {
            let files = self.files.lock().unwrap();
            let id = files.id(uri)?;
            let content = files.content(id);
            (id, content)
        };

        let (resolved, parsed) = {
            let mut runtime = self.runtime.lock().unwrap();
            let resolved = runtime.resolved(id);
            let (parsed, _) = runtime.parsed(id);
            (resolved, parsed)
        };

        let thing = locate::thing_at_position(&content, parsed, position);
        let cst = thing.as_qualified_name()?;

        let qualifier_token = cst.qualifier().and_then(|cst| cst.text());
        let name_token = cst.lower().or_else(|| cst.upper())?;

        let prefix = qualifier_token.as_ref().map(|cst| cst.text().trim_end_matches("."));
        let name = name_token.text();

        let (f_id, t_id) = resolved.lookup_term(prefix, name)?;

        let uri = {
            let files = self.files.lock().unwrap();
            let path = files.path(f_id);
            Url::parse(&path).ok()?
        };

        let (content, parsed, indexed) = {
            let mut runtime = self.runtime.lock().unwrap();
            let content = runtime.content(f_id);
            let (parsed, _) = runtime.parsed(f_id);
            let indexed = runtime.indexed(f_id);
            (content, parsed, indexed)
        };

        let t_ptr = indexed.term_item_ptr(t_id);
        match &t_ptr[..] {
            [t_ptr] => {
                let root = parsed.syntax_node();
                let range = find_range(content, t_ptr, root)?;
                Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
            }
            [s_ptr, .., e_ptr] => {
                let root = parsed.syntax_node();
                let start = find_range(content.clone(), s_ptr, root.clone())?;
                let end = find_range(content.clone(), e_ptr, root.clone())?;
                let range = Range { start: start.start, end: end.end };
                Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
            }
            [] => None,
        }
    }
}

fn find_annotation(ptr: &SyntaxNodePtr, root: SyntaxNode) -> Option<String> {
    let node = ptr.to_node(&root);
    let mut children = node.children();
    let annotation = children.find_map(cst::Annotation::cast)?;
    let token = annotation.text()?;
    let text = token.text().trim();
    Some(text.lines().map(|line| line.trim_start_matches("-- | ")).collect::<Vec<_>>().join("\n"))
}

fn find_range(content: Arc<str>, ptr: &SyntaxNodePtr, root: SyntaxNode) -> Option<Range> {
    let node = ptr.to_node(&root);
    let mut children = node.children_with_tokens().peekable();
    if let Some(child) = children.peek() {
        if matches!(child.kind(), SyntaxKind::Annotation) {
            children.next();
        }
    }
    let start = children.next()?.text_range();
    let end = children.last().map_or(start, |child| child.text_range());
    let start = start.start();
    let end = end.end();
    let start = locate::offset_to_position(&content, start);
    let end = locate::offset_to_position(&content, end);
    let range = Range { start, end };
    Some(range)
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    tracing_subscriber::fmt().init();
    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
