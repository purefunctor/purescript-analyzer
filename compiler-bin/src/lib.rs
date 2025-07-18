pub mod extension;

use std::{
    borrow::BorrowMut,
    env, fs,
    ops::{ControlFlow, Deref},
    path::PathBuf,
    sync::Arc,
    time::Instant,
};

use analyzer::{Files, QueryEngine};
use async_lsp::{
    ClientSocket, ErrorCode, ResponseError, client_monitor::ClientProcessMonitorLayer,
    concurrency::ConcurrencyLayer, lsp_types::*, panic::CatchUnwindLayer, router::Router,
    server::LifecycleLayer,
};
use parking_lot::RwLock;
use tokio::task;
use tower::ServiceBuilder;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{
    Layer, Registry,
    layer::{Context, SubscriberExt},
};

pub struct State {
    pub client: ClientSocket,
    pub engine: QueryEngine,
    pub files: Arc<RwLock<Files>>,
    pub root: Option<PathBuf>,
}

impl State {
    fn new(client: ClientSocket) -> State {
        let engine = QueryEngine::default();
        let files = Arc::new(RwLock::new(Files::default()));
        let root = None;
        State { client, engine, files, root }
    }

    fn spawn<T>(&self, f: impl FnOnce(StateSnapshot) -> T + Send + 'static) -> task::JoinHandle<T>
    where
        T: Send + 'static,
    {
        let snapshot =
            StateSnapshot { engine: self.engine.snapshot(), files: Arc::clone(&self.files) };
        task::spawn_blocking(move || f(snapshot))
    }
}

struct StateSnapshot {
    engine: QueryEngine,
    files: Arc<RwLock<Files>>,
}

impl StateSnapshot {
    fn files(&self) -> impl Deref<Target = Files> {
        self.files.read()
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
            on_change(state, &uri, &text);
        }
        tracing::info!("Loaded {} files.", files.len());
    }

    ControlFlow::Continue(())
}

fn definition(
    snapshot: StateSnapshot,
    p: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>, ResponseError> {
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    Ok(analyzer::definition::implementation(&snapshot.engine, &snapshot.files(), uri, position))
}

fn hover(snapshot: StateSnapshot, p: HoverParams) -> Result<Option<Hover>, ResponseError> {
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    Ok(analyzer::hover::implementation(&snapshot.engine, &snapshot.files(), uri, position))
}

fn completion(
    snapshot: StateSnapshot,
    p: CompletionParams,
) -> Result<Option<CompletionResponse>, ResponseError> {
    let uri = p.text_document_position.text_document.uri;
    let position = p.text_document_position.position;
    Ok(analyzer::completion::implementation(&snapshot.engine, &snapshot.files(), uri, position))
}

fn resolve_completion_item(
    snapshot: StateSnapshot,
    item: CompletionItem,
) -> Result<CompletionItem, ResponseError> {
    Ok(analyzer::completion::resolve::implementation(&snapshot.engine, item))
}

fn did_change(
    state: &mut State,
    p: DidChangeTextDocumentParams,
) -> ControlFlow<async_lsp::Result<()>> {
    let uri = p.text_document.uri.as_str();
    for content_change in &p.content_changes {
        let text = content_change.text.as_str();
        on_change(state, uri, text);
    }
    ControlFlow::Continue(())
}

fn on_change(state: &mut State, uri: &str, text: &str) {
    let (id, content) = {
        let mut files = state.files.write();
        let id = files.insert(uri, text);
        (id, files.content(id))
    };
    state.engine.set_content(id, content);
    let Ok((parsed, _)) = state.engine.parsed(id) else {
        return;
    };
    if let Some(name) = parsed.module_name() {
        state.engine.set_module_file(&name, id);
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

trait RequestExtension: BorrowMut<Router<State>> {
    fn request_snapshot<R: request::Request>(
        &mut self,
        f: impl Fn(StateSnapshot, R::Params) -> Result<R::Result, ResponseError> + Send + Copy + 'static,
    ) -> &mut Self
    where
        R::Params: Send + 'static,
        R::Result: Send + 'static,
    {
        self.borrow_mut().request::<R, _>(move |this, p| {
            let task = this.spawn(move |snapshot| f(snapshot, p));
            async move {
                task.await
                    .map_err(|_| ResponseError::new(ErrorCode::REQUEST_FAILED, "REQUEST_FAILED"))?
            }
        });
        self
    }
}

impl RequestExtension for Router<State> {}

pub async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let mut router: Router<State, ResponseError> = Router::new(State::new(client.clone()));

        router
            .request::<extension::CustomInitialize, _>(initialize)
            .request_snapshot::<request::GotoDefinition>(definition)
            .request_snapshot::<request::HoverRequest>(hover)
            .request_snapshot::<request::Completion>(completion)
            .request_snapshot::<request::ResolveCompletionItem>(resolve_completion_item)
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
