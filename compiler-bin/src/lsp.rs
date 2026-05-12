pub mod error;
pub mod event;
pub mod extension;
pub mod build;

use std::borrow::BorrowMut;
use std::ops::{ControlFlow, Deref};
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::{env, fs, mem, process};
use std::collections::{HashMap, HashSet};

use analyzer::completion::SuggestionsCache;
use analyzer::symbols::WorkspaceSymbolsCache;
use analyzer::{Files, QueryEngine, prim};
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::notification::Notification;
use async_lsp::lsp_types::request::Request;
use async_lsp::lsp_types::*;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::{ClientSocket, ResponseError};
use globset::{Glob, GlobSetBuilder};
use parking_lot::RwLock;
use path_absolutize::Absolutize;
use tokio::task;
use tower::ServiceBuilder;
use walkdir::WalkDir;

use crate::cli;
use crate::lsp::error::{AnalyzerResultExt, LspError};

const PS_BUILD: &str = "purescript.build";
const PS_CLEAN: &str = "purescript.clean";
const PS_RESET: &str = "purescript.reset";
const PS_ANALYZER_REFRESH: &str = "purescript.analyzerRefresh";

pub struct State {
    pub config: Arc<cli::Config>,
    pub client: ClientSocket,

    pub engine: QueryEngine,
    pub files: Arc<RwLock<Files>>,

    pub workspace_symbols_cache: Arc<RwLock<WorkspaceSymbolsCache>>,
    pub suggestions_cache: Arc<RwLock<SuggestionsCache>>,

    // Last published diagnostics from different sources.
    // We merge these per-URI when publishing to the client.
    pub build_diagnostics: Arc<RwLock<HashMap<Url, Vec<Diagnostic>>>>,
    pub analyzer_diagnostics: Arc<RwLock<HashMap<Url, Vec<Diagnostic>>>>,

    // URIs currently open in the client.
    pub open_uris: Arc<RwLock<HashSet<Url>>>,

    // Bumped on reset to prevent in-flight tasks from republishing stale diagnostics.
    pub diagnostics_generation: Arc<AtomicU64>,

    pub root: Option<PathBuf>,
}

impl State {
    fn new(config: Arc<cli::Config>, client: ClientSocket) -> State {
        let mut engine = QueryEngine::default();
        let mut files = Files::default();
        prim::configure(&mut engine, &mut files);

        let files = Arc::new(RwLock::new(files));

        let workspace_symbols_cache = WorkspaceSymbolsCache::default();
        let workspace_symbols_cache = Arc::new(RwLock::new(workspace_symbols_cache));

        let suggestions_cache = SuggestionsCache::default();
        let suggestions_cache = Arc::new(RwLock::new(suggestions_cache));

        let build_diagnostics = Arc::new(RwLock::new(HashMap::new()));
        let analyzer_diagnostics = Arc::new(RwLock::new(HashMap::new()));

        let diagnostics_generation = Arc::new(AtomicU64::new(0));

        let open_uris = Arc::new(RwLock::new(HashSet::new()));

        let root = None;

        State {
            config,
            client,
            engine,
            files,
            workspace_symbols_cache,
            suggestions_cache,
            build_diagnostics,
            analyzer_diagnostics,
            diagnostics_generation,
            open_uris,
            root,
        }
    }

    fn spawn<T>(&self, f: impl FnOnce(StateSnapshot) -> T + Send + 'static) -> task::JoinHandle<T>
    where
        T: Send + 'static,
    {
        let snapshot = StateSnapshot {
            client: self.client.clone(),
            engine: self.engine.snapshot(),
            files: Arc::clone(&self.files),
            workspace_symbols_cache: Arc::clone(&self.workspace_symbols_cache),
            suggestions_cache: Arc::clone(&self.suggestions_cache),
            build_diagnostics: Arc::clone(&self.build_diagnostics),
            analyzer_diagnostics: Arc::clone(&self.analyzer_diagnostics),
            diagnostics_generation: Arc::clone(&self.diagnostics_generation),
            root: self.root.clone(),
        };
        task::spawn_blocking(move || f(snapshot))
    }

    fn invalidate_workspace_symbols(&self) {
        let mut cache = self.workspace_symbols_cache.write();
        mem::take(&mut *cache);
    }

    fn invalidate_suggestions_cache(&self) {
        let mut cache = self.suggestions_cache.write();
        mem::take(&mut *cache);
    }
}

struct StateSnapshot {
    client: ClientSocket,
    engine: QueryEngine,
    files: Arc<RwLock<Files>>,
    workspace_symbols_cache: Arc<RwLock<WorkspaceSymbolsCache>>,
    suggestions_cache: Arc<RwLock<SuggestionsCache>>,
    build_diagnostics: Arc<RwLock<HashMap<Url, Vec<Diagnostic>>>>,
    analyzer_diagnostics: Arc<RwLock<HashMap<Url, Vec<Diagnostic>>>>,
    diagnostics_generation: Arc<AtomicU64>,
    root: Option<PathBuf>,
}

impl StateSnapshot {
    fn files(&self) -> impl Deref<Target = Files> + use<'_> {
        self.files.read()
    }

    fn merged_diagnostics_for_uri(&self, uri: &Url) -> Vec<Diagnostic> {
        let build = {
            let map = self.build_diagnostics.read();
            map.get(uri).cloned().unwrap_or_default()
        };
        let analyzer = {
            let map = self.analyzer_diagnostics.read();
            map.get(uri).cloned().unwrap_or_default()
        };
        merge_diagnostics(&build, &analyzer)
    }
}

fn merge_diagnostics(build: &[Diagnostic], analyzer: &[Diagnostic]) -> Vec<Diagnostic> {
    use std::collections::HashSet;
    use std::hash::{Hash, Hasher};

    #[derive(Clone, Eq)]
    struct Key(String);

    impl PartialEq for Key {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }

    impl Hash for Key {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.0.hash(state)
        }
    }

    fn key(d: &Diagnostic) -> Key {
        let sev = d
            .severity
            .map(|s| match s {
                DiagnosticSeverity::ERROR => 1u32,
                DiagnosticSeverity::WARNING => 2u32,
                DiagnosticSeverity::INFORMATION => 3u32,
                DiagnosticSeverity::HINT => 4u32,
                _ => 0u32,
            })
            .unwrap_or(0);
        let code = match &d.code {
            Some(NumberOrString::Number(n)) => format!("n:{n}"),
            Some(NumberOrString::String(s)) => format!("s:{s}"),
            None => "".to_string(),
        };
        let source = d.source.as_deref().unwrap_or("");
        let r = &d.range;
        Key(format!(
            "{}:{}:{}:{}:{}|{}|{}|{}",
            r.start.line,
            r.start.character,
            r.end.line,
            r.end.character,
            sev,
            code,
            source,
            d.message
        ))
    }

    let mut seen: HashSet<Key> = HashSet::new();
    let mut out = Vec::with_capacity(build.len() + analyzer.len());

    for d in build.iter().chain(analyzer.iter()) {
        let k = key(d);
        if seen.insert(k) {
            out.push(d.clone());
        }
    }
    out
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
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![
                        PS_BUILD.to_string(),
                        PS_CLEAN.to_string(),
                        PS_RESET.to_string(),
                        PS_ANALYZER_REFRESH.to_string(),
                    ],
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                }),
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
                workspace_symbol_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        // Clients may not send didSave unless we advertise it.
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                        ..TextDocumentSyncOptions::default()
                    },
                )),
                ..ServerCapabilities::default()
            },
        })
    }
}

fn execute_command(
    state: &mut State,
    p: ExecuteCommandParams,
) -> std::future::Ready<Result<<request::ExecuteCommand as Request>::Result, ResponseError>> {
    use std::future;

    let res = match p.command.as_str() {
        // Implemented in later tasks; for now dispatch to events.
        PS_ANALYZER_REFRESH => state
            .client
            .emit(event::AnalyzerRefresh)
            .map(|_| None)
            .map_err(|e| ResponseError::new(async_lsp::ErrorCode::REQUEST_FAILED, e.to_string())),
        PS_RESET => state
            .client
            .emit(event::Reset)
            .map(|_| None)
            .map_err(|e| ResponseError::new(async_lsp::ErrorCode::REQUEST_FAILED, e.to_string())),
        PS_CLEAN => state
            .client
            .emit(event::Clean)
            .map(|_| None)
            .map_err(|e| ResponseError::new(async_lsp::ErrorCode::REQUEST_FAILED, e.to_string())),
        PS_BUILD => state
            .client
            .emit(build::Build)
            .map(|_| None)
            .map_err(|e| ResponseError::new(async_lsp::ErrorCode::REQUEST_FAILED, e.to_string())),
        other => Err(ResponseError::new(
            async_lsp::ErrorCode::INVALID_PARAMS,
            format!("unsupported command: {other}"),
        )),
    };

    future::ready(res)
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
) -> Result<Option<GotoDefinitionResponse>, LspError> {
    let _span = tracing::info_span!("definition").entered();
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    analyzer::definition::implementation(&snapshot.engine, &snapshot.files(), uri, position)
        .on_non_fatal(None)
}

fn hover(snapshot: StateSnapshot, p: HoverParams) -> Result<Option<Hover>, LspError> {
    let _span = tracing::info_span!("hover").entered();
    let uri = p.text_document_position_params.text_document.uri;
    let position = p.text_document_position_params.position;
    analyzer::hover::implementation(&snapshot.engine, &snapshot.files(), uri, position)
        .on_non_fatal(None)
}

fn completion(
    snapshot: StateSnapshot,
    p: CompletionParams,
) -> Result<Option<CompletionResponse>, LspError> {
    let _span = tracing::info_span!("completion").entered();
    let uri = p.text_document_position.text_document.uri;
    let position = p.text_document_position.position;

    let mut cache = snapshot.suggestions_cache.write();

    analyzer::completion::implementation(
        &snapshot.engine,
        &snapshot.files(),
        &mut cache,
        uri,
        position,
    )
    .on_non_fatal(None)
}

fn resolve_completion_item(
    snapshot: StateSnapshot,
    item: CompletionItem,
) -> Result<CompletionItem, LspError> {
    let _span = tracing::info_span!("resolve_completion_item").entered();
    analyzer::completion::resolve::implementation(&snapshot.engine, item)
        .or_else(|(error, item)| Err(error).on_non_fatal(item))
}

fn references(
    snapshot: StateSnapshot,
    p: ReferenceParams,
) -> Result<Option<Vec<Location>>, LspError> {
    let _span = tracing::info_span!("references").entered();
    let uri = p.text_document_position.text_document.uri;
    let position = p.text_document_position.position;
    analyzer::references::implementation(&snapshot.engine, &snapshot.files(), uri, position)
        .on_non_fatal(None)
}

fn workspace_symbols(
    snapshot: StateSnapshot,
    p: WorkspaceSymbolParams,
) -> Result<Option<WorkspaceSymbolResponse>, LspError> {
    let _span = tracing::info_span!("workspace_symbols").entered();

    let mut cache = snapshot.workspace_symbols_cache.write();
    analyzer::symbols::workspace(&snapshot.engine, &snapshot.files(), &mut cache, &p.query)
        .on_non_fatal(None)
}

fn did_change(state: &mut State, p: DidChangeTextDocumentParams) -> Result<(), LspError> {
    let uri = p.text_document.uri.as_str();

    for content_change in &p.content_changes {
        let text = content_change.text.as_str();
        on_change(state, uri, text)?
    }

    state.invalidate_workspace_symbols();
    state.invalidate_suggestions_cache();

    if state.config.diagnostics_on_change {
        event::emit_collect_diagnostics(state, p.text_document.uri)?;
    }

    Ok(())
}

fn did_open(state: &mut State, p: DidOpenTextDocumentParams) -> Result<(), LspError> {
    let uri = p.text_document.uri.as_str();
    let text = p.text_document.text.as_str();

    {
        let mut open = state.open_uris.write();
        open.insert(p.text_document.uri.clone());
    }

    on_change(state, uri, text)?;

    state.invalidate_workspace_symbols();
    state.invalidate_suggestions_cache();

    if state.config.diagnostics_on_open {
        event::emit_collect_diagnostics(state, p.text_document.uri)?;
    }

    Ok(())
}

fn did_close(state: &mut State, p: DidCloseTextDocumentParams) -> Result<(), LspError> {
    let mut open = state.open_uris.write();
    open.remove(&p.text_document.uri);
    Ok(())
}

fn did_save(state: &mut State, p: DidSaveTextDocumentParams) -> Result<(), LspError> {
    state.invalidate_suggestions_cache();

    if state.config.diagnostics_on_save {
        event::emit_collect_diagnostics(state, p.text_document.uri)?;
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
    fn request_snapshot<R: Request>(
        &mut self,
        action: impl Fn(StateSnapshot, R::Params) -> Result<R::Result, LspError> + Send + Copy + 'static,
    ) -> &mut Self {
        self.borrow_mut().request::<R, _>(move |state, parameters| {
            let task = state.spawn(move |snapshot| action(snapshot, parameters));
            async move {
                task.await.map_err(LspError::JoinError).flatten().map_err(|error| {
                    error.emit_trace();
                    let code = error.code();
                    let message = error.message();
                    ResponseError::new(code, message)
                })
            }
        });
        self
    }

    fn notification_ext<N: Notification>(
        &mut self,
        action: impl Fn(&mut State, N::Params) -> Result<(), LspError> + Send + Copy + 'static,
    ) -> &mut Self {
        let this: &mut Router<State> = self.borrow_mut();
        this.notification::<N>(move |state, parameters| {
            let _ = action(state, parameters).inspect_err(|error| error.emit_trace());
            ControlFlow::Continue(())
        });
        self
    }
    fn event_ext<E>(
        &mut self,
        action: impl Fn(&mut State, E) -> Result<(), LspError> + Send + Copy + 'static,
    ) -> &mut Self
    where
        E: Send + 'static,
    {
        let this: &mut Router<State> = self.borrow_mut();
        this.event::<E>(move |state, event| {
            let _ = action(state, event).inspect_err(|error| error.emit_trace());
            ControlFlow::Continue(())
        });
        self
    }
}

impl RequestExtension for Router<State> {}

pub async fn start(config: Arc<cli::Config>) {
    let (server, _) = async_lsp::MainLoop::new_server(move |client| {
        let mut router: Router<State, ResponseError> =
            Router::new(State::new(config, client.clone()));

        router
            .request::<extension::CustomInitialize, _>(initialize)
            .request::<request::ExecuteCommand, _>(execute_command)
            .request_snapshot::<request::GotoDefinition>(definition)
            .request_snapshot::<request::HoverRequest>(hover)
            .request_snapshot::<request::Completion>(completion)
            .request_snapshot::<request::ResolveCompletionItem>(resolve_completion_item)
            .request_snapshot::<request::References>(references)
            .request_snapshot::<request::WorkspaceSymbolRequest>(workspace_symbols)
            .notification_ext::<notification::Initialized>(initialized)
            .notification_ext::<notification::DidOpenTextDocument>(did_open)
            .notification_ext::<notification::DidSaveTextDocument>(did_save)
            .notification_ext::<notification::DidCloseTextDocument>(did_close)
            .notification_ext::<notification::DidChangeConfiguration>(|_, _| Ok(()))
            .notification_ext::<notification::DidChangeTextDocument>(did_change)
            .notification_ext::<notification::DidChangeWatchedFiles>(|_, _| Ok(()))
            .event_ext::<event::CollectDiagnostics>(event::collect_diagnostics)
            .event_ext::<event::AnalyzerRefresh>(event::analyzer_refresh)
            .event_ext::<event::Reset>(event::reset)
            .event_ext::<event::Clean>(event::clean)
            .event_ext::<build::Build>(build::build);

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

    if let Err(error) = server.run_buffered(stdin, stdout).await {
        tracing::error!(?error, "LSP main loop exited");
        process::exit(1);
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    use async_lsp::lsp_types::{
        ClientCapabilities, ExecuteCommandParams, InitializeParams, Url, WorkspaceFolder,
    };

    fn mk_state_with(config: cli::Config) -> State {
        // ClientSocket isn't used by initialize/formatting logic in tests.
        let client = ClientSocket::new_closed();
        State::new(Arc::new(config), client)
    }

    fn mk_init_params(root: &std::path::Path) -> InitializeParams {
        InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(root).unwrap(),
                name: "workspace".to_string(),
            }]),
            work_done_progress_params: WorkDoneProgressParams::default(),
            client_info: None,
            locale: None,
        }
    }

    fn base_config() -> cli::Config {
        cli::Config {
            stdio: true,
            log_file: false,
            query_log: tracing::level_filters::LevelFilter::OFF,
            lsp_log: tracing::level_filters::LevelFilter::INFO,
            checking_log: tracing::level_filters::LevelFilter::OFF,
            source_command: None,
            diagnostics_on_open: true,
            diagnostics_on_save: true,
            diagnostics_on_change: false,
            build_tool: cli::BuildTool::Auto,
            build_arg: vec![],
        }
    }

    #[test]
    fn reset_clears_diagnostics_but_keeps_files() {
        let mut state = mk_state_with(base_config());
        on_change(&mut state, "file:///test/Main.purs", "module Main where\n").unwrap();

        // Seed both diagnostic sources.
        let uri = Url::parse("file:///test/Main.purs").unwrap();
        state
            .build_diagnostics
            .write()
            .insert(uri.clone(), vec![Diagnostic::default()]);
        state
            .analyzer_diagnostics
            .write()
            .insert(uri.clone(), vec![Diagnostic::default()]);

        event::reset(&mut state, event::Reset).unwrap();

        assert!(state.build_diagnostics.read().is_empty());
        assert!(state.analyzer_diagnostics.read().is_empty());
        assert!(state.files.read().id(uri.as_str()).is_some());
    }

    #[tokio::test]
    async fn analyzer_refresh_handles_multiple_files() {
        let mut state = mk_state_with(base_config());
        on_change(&mut state, "file:///test/A.purs", "module A where\n").unwrap();
        on_change(&mut state, "file:///test/B.purs", "module B where\n").unwrap();

        event::analyzer_refresh(&mut state, event::AnalyzerRefresh).unwrap();
    }

    #[tokio::test]
    async fn execute_command_capability_advertised() {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let mut state = mk_state_with(base_config());

        let initialize_params = mk_init_params(root);
        let res = initialize(
            &mut state,
            extension::CustomInitializeParams { initialize_params, work_done_token: None },
        )
        .await
        .unwrap();

        let provider = res.capabilities.execute_command_provider.unwrap();
        assert_eq!(
            provider.commands,
            vec![
                PS_BUILD.to_string(),
                PS_CLEAN.to_string(),
                PS_RESET.to_string(),
                PS_ANALYZER_REFRESH.to_string(),
            ]
        );
    }

    #[tokio::test]
    async fn execute_command_unknown_rejected() {
        let mut state = mk_state_with(base_config());
        let res = execute_command(
            &mut state,
            ExecuteCommandParams {
                command: "purescript.nope".to_string(),
                arguments: vec![],
                work_done_progress_params: WorkDoneProgressParams::default(),
            },
        )
        .await;

        let err = res.unwrap_err();
        assert_eq!(err.code, async_lsp::ErrorCode::INVALID_PARAMS);
    }

    #[tokio::test]
    async fn execute_command_dispatches_via_events() {
        // With a closed client socket, emitting an internal event will fail.
        // This still verifies the executeCommand handler is wired to dispatch.
        let mut state = mk_state_with(base_config());
        let res = execute_command(
            &mut state,
            ExecuteCommandParams {
                command: PS_ANALYZER_REFRESH.to_string(),
                arguments: vec![],
                work_done_progress_params: WorkDoneProgressParams::default(),
            },
        )
        .await;

        let err = res.unwrap_err();
        assert_eq!(err.code, async_lsp::ErrorCode::REQUEST_FAILED);
    }
}
