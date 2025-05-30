pub mod definition;
pub mod extension;
pub mod hover;
pub mod locate;

use std::{
    env, fs,
    ops::ControlFlow,
    sync::{Arc, Mutex},
};

use async_lsp::{
    ResponseError, client_monitor::ClientProcessMonitorLayer, concurrency::ConcurrencyLayer,
    lsp_types::*, panic::CatchUnwindLayer, router::Router, server::LifecycleLayer,
    tracing::TracingLayer,
};
use building::Runtime;
use files::Files;
use futures::future::BoxFuture;
use smol_str::SmolStrBuilder;
use tower::ServiceBuilder;
use tracing::Level;

#[derive(Default, Clone)]
pub struct State {
    pub runtime: Arc<Mutex<Runtime>>,
    pub files: Arc<Mutex<Files>>,
}

fn initialize(
    _: &mut State,
    _: extension::CustomInitializeParams,
) -> BoxFuture<'static, Result<InitializeResult, ResponseError>> {
    Box::pin(async move {
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
    })
}

fn initialized(state: &mut State, _: InitializedParams) -> ControlFlow<async_lsp::Result<()>> {
    let root = env::current_dir().unwrap();
    if let Ok(files) = spago::source_files(&root) {
        tracing::info!("Loading {} files.", files.len());
        for file in &files {
            let uri = format!("file://{}", file.to_str().unwrap());
            let text = fs::read_to_string(file).unwrap();
            on_change(state, &uri, &text);
        }
        tracing::info!("Loaded {} files.", files.len());
    }
    ControlFlow::Continue(())
}

fn definition(
    state: &mut State,
    p: GotoDefinitionParams,
) -> BoxFuture<'static, Result<Option<GotoDefinitionResponse>, ResponseError>> {
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    let result = definition::definition(state, uri, position);
    Box::pin(async move { Result::Ok(result) })
}

fn hover(
    state: &mut State,
    p: HoverParams,
) -> BoxFuture<'static, Result<Option<Hover>, ResponseError>> {
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    let result = hover::hover(state, uri, position);
    Box::pin(async move { Result::Ok(result) })
}

fn did_change(
    state: &mut State,
    p: DidChangeTextDocumentParams,
) -> ControlFlow<async_lsp::Result<()>> {
    let uri = p.text_document.uri.as_str();
    let text = p.content_changes[0].text.as_str();
    on_change(state, uri, text);
    ControlFlow::Continue(())
}

fn on_change(state: &mut State, uri: &str, text: &str) {
    let id = state.files.lock().unwrap().insert(uri, text);
    let content = state.files.lock().unwrap().content(id);

    state.runtime.lock().unwrap().set_content(id, content);
    let (parsed, _) = state.runtime.lock().unwrap().parsed(id);

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
        state.runtime.lock().unwrap().set_module_file(&name, id);
    }
}

pub async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let mut router: Router<State, ResponseError> = Router::new(State::default());

        router
            .request::<extension::CustomInitialize, _>(initialize)
            .request::<request::GotoDefinition, _>(definition)
            .request::<request::HoverRequest, _>(hover)
            .notification::<notification::Initialized>(initialized)
            .notification::<notification::DidOpenTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidSaveTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidCloseTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeTextDocument>(did_change);

        ServiceBuilder::new()
            .layer(TracingLayer::default())
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client))
            .service(router)
    });

    let log_file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open("/tmp/purescript-analyzer.log")
        .expect("Failed to open log file");

    tracing_subscriber::fmt().with_max_level(Level::INFO).with_writer(log_file).init();

    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );

    server.run_buffered(stdin, stdout).await.unwrap()
}
