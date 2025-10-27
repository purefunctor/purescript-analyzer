pub mod error;
pub mod extension;

use std::{
    borrow::BorrowMut,
    env, fs,
    ops::{ControlFlow, Deref},
    path::PathBuf,
    process,
    sync::Arc,
};

use analyzer::{Files, QueryEngine, prim};
use async_lsp::{
    ClientSocket, ErrorCode, ResponseError, client_monitor::ClientProcessMonitorLayer,
    concurrency::ConcurrencyLayer, lsp_types::*, panic::CatchUnwindLayer, router::Router,
    server::LifecycleLayer,
};
use globset::{Glob, GlobSetBuilder};
use parking_lot::RwLock;
use path_absolutize::Absolutize;
use tokio::task;
use tower::ServiceBuilder;
use walkdir::WalkDir;

use crate::{
    cli,
    lsp::error::{IntoResponseError, LspError, NonFatalResult},
};

pub struct State {
    pub config: Arc<cli::Config>,
    pub client: ClientSocket,

    pub engine: QueryEngine,
    pub files: Arc<RwLock<Files>>,

    pub root: Option<PathBuf>,
}

impl State {
    fn new(config: Arc<cli::Config>, client: ClientSocket) -> State {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let files = Arc::new(RwLock::new(files));
        let root = None;

        State { config, client, engine, files, root }
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
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                }),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                references_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..ServerCapabilities::default()
            },
        })
    }
}

fn initialized(state: &mut State, _: InitializedParams) -> Result<(), LspError> {
    let _span = tracing::info_span!("initialization").entered();
    let config = Arc::clone(&state.config);
    if let Some(command) = config.source_command.as_deref() {
        initialized_manual(state, command)
    } else {
        initialized_spago(state)
    }
}

fn initialized_manual(state: &mut State, command: &str) -> Result<(), LspError> {
    let root = state.root.as_ref().ok_or(LspError::MissingRoot)?;

    tracing::info!("Using '{}'", command);

    let mut parts = command.split(" ");
    let program = parts.next().ok_or(LspError::InvalidSourceCommand)?;

    let mut command = process::Command::new(program);
    command.args(parts);

    let output = command.output()?;
    let output = str::from_utf8(&output.stdout)?;

    let mut files = vec![];
    let mut globs = GlobSetBuilder::new();

    for line in output.lines() {
        let path = root.join(line);
        if let Ok(path) = path.absolutize()
            && let Some(path) = path.to_str()
            && let Ok(glob) = Glob::new(path)
        {
            globs.add(glob);
        } else {
            files.push(path);
        }
    }

    let globs = globs.build()?;

    tracing::info!("Found {} file literals", files.len());
    tracing::info!("Found {} glob patterns", globs.len());

    let files_from_glob = WalkDir::new(root).into_iter().filter_map(move |entry| {
        let entry = entry.ok()?;
        let path = entry.path();
        if globs.matches(path).is_empty() { None } else { Some(path.to_path_buf()) }
    });

    files.extend(files_from_glob);
    load_files(state, &files)?;

    Ok(())
}

fn initialized_spago(state: &mut State) -> Result<(), LspError> {
    let root = state.root.as_ref().ok_or(LspError::MissingRoot)?;

    tracing::info!("Using 'spago.lock'");

    let files = spago::source_files(root).map_err(LspError::SpagoLock)?;
    load_files(state, &files)?;

    Ok(())
}

fn load_files(state: &mut State, files: &[PathBuf]) -> Result<(), LspError> {
    tracing::info!("Loading {} files.", files.len());

    for file in files {
        let url = url::Url::from_file_path(file).map_err(|_| {
            let file = PathBuf::clone(file);
            LspError::PathParseFail(file)
        })?;

        let uri = url.to_string();

        let text = fs::read_to_string(file)?;
        on_change(state, &uri, &text)?
    }

    tracing::info!("Loaded {} files.", files.len());

    Ok(())
}

fn definition(
    snapshot: StateSnapshot,
    p: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>, ResponseError> {
    let _span = tracing::info_span!("definition").entered();
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    analyzer::definition::implementation(&snapshot.engine, &snapshot.files(), uri, position)
        .on_non_fatal(None)
}

fn hover(snapshot: StateSnapshot, p: HoverParams) -> Result<Option<Hover>, ResponseError> {
    let _span = tracing::info_span!("hover").entered();
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    analyzer::hover::implementation(&snapshot.engine, &snapshot.files(), uri, position)
        .on_non_fatal(None)
}

fn completion(
    snapshot: StateSnapshot,
    p: CompletionParams,
) -> Result<Option<CompletionResponse>, ResponseError> {
    let _span = tracing::info_span!("completion").entered();
    let uri = p.text_document_position.text_document.uri;
    let position = p.text_document_position.position;
    analyzer::completion::implementation(&snapshot.engine, &snapshot.files(), uri, position)
        .on_non_fatal(None)
}

fn resolve_completion_item(
    snapshot: StateSnapshot,
    item: CompletionItem,
) -> Result<CompletionItem, ResponseError> {
    let _span = tracing::info_span!("resolve_completion_item").entered();
    analyzer::completion::resolve::implementation(&snapshot.engine, item)
        .or_else(|(error, item)| Err(error).on_non_fatal(item))
}

fn references(
    snapshot: StateSnapshot,
    p: ReferenceParams,
) -> Result<Option<Vec<Location>>, ResponseError> {
    let _span = tracing::info_span!("references").entered();
    let uri = p.text_document_position.text_document.uri;
    let position = p.text_document_position.position;
    analyzer::references::implementation(&snapshot.engine, &snapshot.files(), uri, position)
        .on_non_fatal(None)
}

fn did_change(state: &mut State, p: DidChangeTextDocumentParams) -> Result<(), LspError> {
    let uri = p.text_document.uri.as_str();

    for content_change in &p.content_changes {
        let text = content_change.text.as_str();
        on_change(state, uri, text)?
    }

    Ok(())
}

fn on_change(state: &mut State, uri: &str, content: &str) -> Result<(), LspError> {
    // Cancel in-flight queries so that threads holding a read lock
    // over `files` are terminated quickly, compared to having to
    // wait for expensive LSP requests to complete successfully.
    state.engine.request_cancel();

    let mut files = state.files.write();
    let id = files.insert(uri, content);

    state.engine.set_content(id, content);

    let (parsed, _) = state.engine.parsed(id)?;

    if let Some(name) = parsed.module_name() {
        state.engine.set_module_file(&name, id);
    }

    Ok(())
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

fn adapt_notification<T, E, P>(
    f: impl Fn(&mut State, P) -> Result<T, E>,
) -> impl Fn(&mut State, P) -> ControlFlow<async_lsp::Result<T>>
where
    E: IntoResponseError,
{
    move |s, p| match f(s, p) {
        Ok(_) => ControlFlow::Continue(()),
        Err(error) => {
            let error = error.into_response_error();
            let error = async_lsp::Error::from(error);
            ControlFlow::Break(Err(error))
        }
    }
}

pub async fn start(config: Arc<cli::Config>) {
    let (server, _) = async_lsp::MainLoop::new_server(move |client| {
        let mut router: Router<State, ResponseError> =
            Router::new(State::new(config, client.clone()));

        router
            .request::<extension::CustomInitialize, _>(initialize)
            .request_snapshot::<request::GotoDefinition>(definition)
            .request_snapshot::<request::HoverRequest>(hover)
            .request_snapshot::<request::Completion>(completion)
            .request_snapshot::<request::ResolveCompletionItem>(resolve_completion_item)
            .request_snapshot::<request::References>(references)
            .notification::<notification::Initialized>(adapt_notification(initialized))
            .notification::<notification::DidOpenTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidSaveTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidCloseTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeConfiguration>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeTextDocument>(adapt_notification(did_change));

        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client))
            .service(router)
    });

    #[cfg(unix)]
    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );

    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    server.run_buffered(stdin, stdout).await.unwrap();
}
