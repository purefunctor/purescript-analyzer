pub mod extension;

use std::{env, fs, ops::ControlFlow, path::PathBuf, time::Instant};

use async_lsp::{
    ClientSocket, ResponseError, client_monitor::ClientProcessMonitorLayer,
    concurrency::ConcurrencyLayer, lsp_types::*, panic::CatchUnwindLayer, router::Router,
    server::LifecycleLayer,
};
use tower::ServiceBuilder;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{
    Layer, Registry,
    layer::{Context, SubscriberExt},
};

pub struct State {
    pub client: ClientSocket,
    pub compiler: analyzer::Compiler,
    pub root: Option<PathBuf>,
}

impl State {
    fn new(client: ClientSocket) -> State {
        let compiler = analyzer::Compiler::default();
        let root = None;
        State { client, compiler, root }
    }
}

fn initialize(
    state: &mut State,
    p: extension::CustomInitializeParams,
) -> impl Future<Output = Result<InitializeResult, ResponseError>> + use<> {
    state.root = p
        .initialize_params
        .workspace_folders
        .and_then(|folders| {
            let folder = folders.first()?;
            folder.uri.to_file_path().ok()
        })
        .or_else(|| env::current_dir().ok());
    async move {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![".".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    completion_item: None,
                }),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..ServerCapabilities::default()
            },
        })
    }
}

fn initialized(state: &mut State, _: InitializedParams) -> ControlFlow<async_lsp::Result<()>> {
    let _span = tracing::info_span!("initialization").entered();

    let files = state.root.as_ref().and_then(|root| spago::source_files(root).ok());
    if let Some(files) = files {
        tracing::info!("Loading {} files.", files.len());
        for file in &files {
            let uri = format!("file://{}", file.to_str().unwrap());
            let text = fs::read_to_string(file).unwrap();
            on_change(&mut state.compiler, &uri, &text);
        }
        tracing::info!("Loaded {} files.", files.len());
    }

    ControlFlow::Continue(())
}

fn definition(
    state: &mut State,
    p: GotoDefinitionParams,
) -> impl Future<Output = Result<Option<GotoDefinitionResponse>, ResponseError>> + use<> {
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    let result = analyzer::definition::implementation(&state.compiler, uri, position);
    async move { Result::Ok(result) }
}

fn hover(
    state: &mut State,
    p: HoverParams,
) -> impl Future<Output = Result<Option<Hover>, ResponseError>> + use<> {
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    let result = analyzer::hover::implementation(&state.compiler, uri, position);
    async move { Ok(result) }
}

fn completion(
    state: &mut State,
    p: CompletionParams,
) -> impl Future<Output = Result<Option<CompletionResponse>, ResponseError>> + use<> {
    let uri = p.text_document_position.text_document.uri;
    let position = p.text_document_position.position;
    let result = analyzer::completion::implementation(&state.compiler, uri, position);
    async move { Ok(result) }
}

fn resolve_completion_item(
    state: &mut State,
    item: CompletionItem,
) -> impl Future<Output = Result<CompletionItem, ResponseError>> + use<> {
    let result = analyzer::completion::resolve::implementation(&state.compiler, item);
    async move { Ok(result) }
}

fn did_change(
    state: &mut State,
    p: DidChangeTextDocumentParams,
) -> ControlFlow<async_lsp::Result<()>> {
    let uri = p.text_document.uri.as_str();
    for content_change in &p.content_changes {
        let text = content_change.text.as_str();
        on_change(&mut state.compiler, uri, text);
    }
    ControlFlow::Continue(())
}

fn on_change(compiler: &mut analyzer::Compiler, uri: &str, text: &str) {
    let id = compiler.files.insert(uri, text);
    let content = compiler.files.content(id);

    compiler.engine.set_content(id, content);
    let Ok((parsed, _)) = compiler.engine.parsed(id) else {
        return;
    };
    if let Some(name) = parsed.module_name() {
        compiler.engine.set_module_file(&name, id);
    }
}

struct SpanTimingLayer;

impl<S> Layer<S> for SpanTimingLayer
where
    S: tracing::Subscriber + for<'lookup> tracing_subscriber::registry::LookupSpan<'lookup>,
{
    fn on_enter(&self, id: &tracing::span::Id, ctx: Context<'_, S>) {
        if let Some(span) = ctx.span(id) {
            span.extensions_mut().insert(Instant::now());
        }
    }

    fn on_close(&self, id: tracing::span::Id, ctx: Context<'_, S>) {
        if let Some(span) = ctx.span(&id) {
            if let Some(start) = span.extensions().get::<Instant>() {
                let duration = start.elapsed();
                let name = span.name();
                tracing::info!(duration = format!("{duration:?}"), "{name}");
            }
        }
    }
}

pub async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let mut router: Router<State, ResponseError> = Router::new(State::new(client.clone()));

        router
            .request::<extension::CustomInitialize, _>(initialize)
            .request::<request::GotoDefinition, _>(definition)
            .request::<request::HoverRequest, _>(hover)
            .request::<request::Completion, _>(completion)
            .request::<request::ResolveCompletionItem, _>(resolve_completion_item)
            .notification::<notification::Initialized>(initialized)
            .notification::<notification::DidOpenTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidSaveTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidCloseTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeTextDocument>(did_change);

        ServiceBuilder::new()
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

    let fmt = tracing_subscriber::fmt::layer().with_writer(log_file).with_filter(LevelFilter::INFO);

    let subscriber = Registry::default().with(fmt).with(SpanTimingLayer);
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );

    server.run_buffered(stdin, stdout).await.unwrap()
}
