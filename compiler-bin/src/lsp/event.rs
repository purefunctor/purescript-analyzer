use analyzer::{common, locate};
use async_lsp::LanguageClient;
use async_lsp::lsp_types::*;
use diagnostics::{DiagnosticsContext, ToDiagnostics};
use files::FileId;
use itertools::Itertools;
use path_absolutize::Absolutize;
use rowan::TextSize;

use crate::lsp::error::LspError;
use crate::lsp::{State, StateSnapshot};
use std::collections::HashSet;
use std::path::{Component, Path};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

const REFRESH_PROGRESS_INTERVAL: usize = 50;

pub struct AnalyzerRefresh;

pub fn analyzer_refresh(state: &mut State, _: AnalyzerRefresh) -> Result<(), LspError> {
    // Best-effort: cancel in-flight queries so refresh work wins.
    state.engine.request_cancel();

    let mut refresh_root = state.root.as_deref();
    let (mut file_ids, mut excluded_uris) =
        refreshable_file_ids_and_excluded_uris(state, refresh_root);

    if file_ids.is_empty() && state.root.is_some() {
        // Some clients initialize the LSP with a root that does not contain the
        // loaded source file paths. Fall back to loaded file:// .purs files while
        // still honoring excluded directory names.
        refresh_root = None;
        (file_ids, excluded_uris) = refreshable_file_ids_and_excluded_uris(state, refresh_root);
    }

    {
        let analyzer = state.analyzer_diagnostics.read();
        excluded_uris.extend(
            analyzer
                .keys()
                .filter(|uri| {
                    !is_refreshable_workspace_source_uri(
                        refresh_root,
                        &state.config.analyzer_excluded_dir,
                        uri,
                    )
                })
                .cloned(),
        );
    }

    excluded_uris.sort();
    excluded_uris.dedup();

    let cleared_uris = {
        let mut analyzer = state.analyzer_diagnostics.write();
        excluded_uris.into_iter().filter(|uri| analyzer.remove(uri).is_some()).collect_vec()
    };

    for uri in cleared_uris {
        publish_uri_if_changed(state, uri)?;
    }

    prioritize_open_files(state, &mut file_ids);
    let scheduled = file_ids.len();
    let open_count = count_open_files(state, &file_ids);
    let _ = state.client.show_message(ShowMessageParams {
        typ: MessageType::INFO,
        message: format!("Analyzer refresh scheduled {scheduled} file(s), {open_count} open"),
    });
    schedule_diagnostics(state, file_ids, PublishMode::ChangedOnly, true);
    Ok(())
}

pub(crate) fn is_refreshable_workspace_source_uri(
    root: Option<&Path>,
    excluded_dirs: &[String],
    uri: &Url,
) -> bool {
    if uri.scheme() != "file" {
        return false;
    }

    let Ok(path) = uri.to_file_path() else {
        return false;
    };

    if path.extension().and_then(|extension| extension.to_str()) != Some("purs") {
        return false;
    }

    let path = path.absolutize().map(|path| path.to_path_buf()).unwrap_or(path);

    if let Some(root) = root {
        let root =
            root.absolutize().map(|root| root.to_path_buf()).unwrap_or_else(|_| root.to_path_buf());
        if !path.starts_with(root) {
            return false;
        }
    }

    !is_excluded_path(&path, excluded_dirs)
}

fn refreshable_file_ids_and_excluded_uris(
    state: &State,
    root: Option<&Path>,
) -> (Vec<FileId>, Vec<Url>) {
    let files = state.files.read();
    files
        .iter_id()
        .filter_map(|file_id| {
            let path = files.path(file_id);
            let uri = Url::parse(path.as_ref()).ok()?;
            if is_refreshable_workspace_source_uri(root, &state.config.analyzer_excluded_dir, &uri)
            {
                Some((Some(file_id), None))
            } else if uri.scheme() == "file" {
                Some((None, Some(uri)))
            } else {
                None
            }
        })
        .fold((vec![], vec![]), |(mut file_ids, mut excluded_uris), (file_id, uri)| {
            if let Some(file_id) = file_id {
                file_ids.push(file_id);
            }
            if let Some(uri) = uri {
                excluded_uris.push(uri);
            }
            (file_ids, excluded_uris)
        })
}

fn prioritize_open_files(state: &State, file_ids: &mut [FileId]) {
    let files = state.files.read();
    let open_paths = open_file_paths(state);
    file_ids.sort_by_key(|file_id| {
        let is_open = Url::parse(files.path(*file_id).as_ref())
            .ok()
            .and_then(|uri| uri.to_file_path().ok())
            .map(|path| normalized_path_key(path.as_path()))
            .is_some_and(|path| open_paths.contains(&path));
        !is_open
    });
}

fn count_open_files(state: &State, file_ids: &[FileId]) -> usize {
    let files = state.files.read();
    let open_paths = open_file_paths(state);
    file_ids
        .iter()
        .filter(|file_id| {
            Url::parse(files.path(**file_id).as_ref())
                .ok()
                .and_then(|uri| uri.to_file_path().ok())
                .map(|path| normalized_path_key(path.as_path()))
                .is_some_and(|path| open_paths.contains(&path))
        })
        .count()
}

fn open_file_paths(state: &State) -> HashSet<String> {
    state
        .open_uris
        .read()
        .iter()
        .filter_map(|uri| uri.to_file_path().ok())
        .map(|path| normalized_path_key(path.as_path()))
        .collect()
}

fn normalized_path_key(path: &Path) -> String {
    path.absolutize()
        .map(|path| path.to_string_lossy().to_string())
        .unwrap_or_else(|_| path.to_string_lossy().to_string())
}

fn publish_uri_if_changed(state: &mut State, uri: Url) -> Result<(), LspError> {
    let mut snapshot = StateSnapshot {
        client: state.client.clone(),
        config: std::sync::Arc::clone(&state.config),
        engine: state.engine.snapshot(),
        files: std::sync::Arc::clone(&state.files),
        workspace_symbols_cache: std::sync::Arc::clone(&state.workspace_symbols_cache),
        suggestions_cache: std::sync::Arc::clone(&state.suggestions_cache),
        build_diagnostics: std::sync::Arc::clone(&state.build_diagnostics),
        analyzer_diagnostics: std::sync::Arc::clone(&state.analyzer_diagnostics),
        published_diagnostics: std::sync::Arc::clone(&state.published_diagnostics),
        diagnostics_generation: std::sync::Arc::clone(&state.diagnostics_generation),
        root: state.root.clone(),
    };
    snapshot.publish_merged_diagnostics_if_changed(uri)
}

fn is_excluded_path(path: &Path, excluded_dirs: &[String]) -> bool {
    path.components().any(|component| match component {
        Component::Normal(name) => {
            name.to_str().is_some_and(|name| excluded_dirs.iter().any(|dir| dir == name))
        }
        _ => false,
    })
}

pub struct Reset;

pub fn reset(state: &mut State, _: Reset) -> Result<(), LspError> {
    state.engine.request_cancel();

    // Prevent in-flight build/diagnostics tasks from republishing stale diagnostics after reset.
    state.diagnostics_generation.fetch_add(1, Ordering::SeqCst);

    // Capture a snapshot client for publishing after we mutate state.
    let mut client = state.client.clone();

    // Clear any published diagnostics (build + analyzer).
    let mut uris_to_clear: Vec<Url> = {
        let build = state.build_diagnostics.read();
        let analyzer = state.analyzer_diagnostics.read();
        let open = state.open_uris.read();
        build
            .keys()
            .chain(analyzer.keys())
            .chain(open.iter())
            .filter(|u| u.scheme() == "file")
            .cloned()
            .collect()
    };
    uris_to_clear.sort();
    uris_to_clear.dedup();

    {
        state.build_diagnostics.write().clear();
        state.analyzer_diagnostics.write().clear();
        state.published_diagnostics.write().clear();
    }

    for uri in uris_to_clear.iter().cloned() {
        let _ = client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics: vec![],
            version: None,
        });
    }

    // Debug breadcrumb.
    let _ = client.show_message(ShowMessageParams {
        typ: MessageType::INFO,
        message: "Reset complete".to_string(),
    });

    // Fast reset: clear diagnostics + caches, keep current file contents.
    // Full workspace reload is expensive on large projects; users can opt into
    // recomputing diagnostics via purescript.analyzerRefresh or by running build.
    state.invalidate_workspace_symbols();
    state.invalidate_suggestions_cache();

    Ok(())
}

pub struct Clean;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CleanOutcome {
    Deleted,
    NotFound,
}

pub(crate) fn clean_output_dir(root: &std::path::Path) -> Result<CleanOutcome, LspError> {
    let root = root.absolutize()?.to_path_buf();
    let output = root.join("output");

    // Safety: only ever delete exactly <root>/output.
    if output.parent() != Some(root.as_path()) {
        return Err(LspError::IoError(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "refusing to delete non-workspace output directory",
        )));
    }

    match std::fs::remove_dir_all(&output) {
        Ok(()) => Ok(CleanOutcome::Deleted),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(CleanOutcome::NotFound),
        Err(e) => Err(LspError::IoError(e)),
    }
}

pub fn clean(state: &mut State, _: Clean) -> Result<(), LspError> {
    state.spawn(|mut snapshot| {
        let _span = tracing::info_span!("clean").entered();
        let root = snapshot.root.as_ref().ok_or(LspError::MissingRoot)?;

        // Clearing output artifacts should also clear any build diagnostics, otherwise
        // clients keep showing stale build-only errors after `purescript.clean`.
        let mut build_uris: Vec<Url> = {
            let map = snapshot.build_diagnostics.read();
            map.keys().filter(|u| u.scheme() == "file").cloned().collect()
        };
        build_uris.sort();
        build_uris.dedup();

        let res: Result<(), LspError> = match clean_output_dir(root) {
            Ok(CleanOutcome::Deleted) => {
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::INFO,
                    message: "Deleted output/".to_string(),
                });
                Ok(())
            }
            Ok(CleanOutcome::NotFound) => {
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::INFO,
                    message: "output/ did not exist".to_string(),
                });
                Ok(())
            }
            Err(e) => Err(e),
        };

        if let Err(e) = &res {
            let _ = snapshot.client.show_message(ShowMessageParams {
                typ: MessageType::ERROR,
                message: format!("Failed to delete output/: {e}"),
            });
        }

        if res.is_ok() {
            // Drop build diagnostics and republish merged diagnostics for affected URIs.
            snapshot.build_diagnostics.write().clear();
            for uri in build_uris {
                let _ = snapshot.publish_merged_diagnostics_if_changed(uri);
            }
        }

        res
    });

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mk_tmp_dir(name: &str) -> std::path::PathBuf {
        let mut dir = std::env::temp_dir();
        dir.push(format!("purescript-analyzer-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn clean_deletes_output_dir() {
        let root = mk_tmp_dir("clean");
        std::fs::create_dir_all(root.join("output")).unwrap();

        let out = clean_output_dir(&root).unwrap();
        assert_eq!(out, CleanOutcome::Deleted);
        assert!(!root.join("output").exists());
    }

    #[test]
    fn analyzer_refresh_only_targets_workspace_sources() {
        let root = mk_tmp_dir("refresh-scope");
        let source_uri = Url::from_file_path(root.join("src/Main.purs")).unwrap();
        let test_uri = Url::from_file_path(root.join("test/Main.purs")).unwrap();
        let dependency_uri =
            Url::from_file_path(root.join(".spago/p/prelude/src/Prelude.purs")).unwrap();
        let output_uri = Url::from_file_path(root.join("output/Main/index.js")).unwrap();
        let external_uri = Url::from_file_path(root.with_file_name("external/Main.purs")).unwrap();
        let generated_purs_uri = Url::from_file_path(root.join("output/Main.purs")).unwrap();
        let prim_uri = Url::parse("prim://Prim.purs").unwrap();
        let excluded_dirs = [".spago", "output", ".git", "node_modules"].map(String::from);

        assert!(is_refreshable_workspace_source_uri(Some(&root), &excluded_dirs, &source_uri));
        assert!(is_refreshable_workspace_source_uri(Some(&root), &excluded_dirs, &test_uri));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &excluded_dirs, &dependency_uri));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &excluded_dirs, &output_uri));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &excluded_dirs, &external_uri));
        assert!(!is_refreshable_workspace_source_uri(
            Some(&root),
            &excluded_dirs,
            &generated_purs_uri
        ));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &excluded_dirs, &prim_uri));
    }
}

pub fn emit_collect_diagnostics(state: &mut State, uri: Url) -> Result<(), LspError> {
    let files = state.files.read();
    let uri = uri.as_str();

    let file_id = files.id(uri);
    drop(files);

    if let Some(file_id) = file_id {
        schedule_diagnostics(state, vec![file_id], PublishMode::ChangedOnly, false);
    }

    Ok(())
}

pub(super) struct CollectDiagnostics(pub(super) FileId);

pub(super) fn collect_diagnostics(
    state: &mut State,
    id: CollectDiagnostics,
) -> Result<(), LspError> {
    schedule_diagnostics(state, vec![id.0], PublishMode::ChangedOnly, false);
    Ok(())
}

#[derive(Clone, Copy)]
pub(super) enum PublishMode {
    ChangedOnly,
}

fn schedule_diagnostics(
    state: &mut State,
    file_ids: Vec<FileId>,
    publish_mode: PublishMode,
    show_summary: bool,
) {
    if file_ids.is_empty() {
        return;
    }

    state.engine.request_cancel();
    let generation = state.diagnostics_generation.fetch_add(1, Ordering::SeqCst) + 1;

    // Query execution can wait on in-progress queries from other snapshots.
    // Keep full refresh sequential to avoid whole-refresh deadlocks between
    // workers checking mutually dependent modules.
    let worker_count = 1;
    let total = file_ids.len();
    let remaining = Arc::new(AtomicUsize::new(worker_count));
    let started = Arc::new(AtomicUsize::new(0));
    let checked = Arc::new(AtomicUsize::new(0));
    let diagnostic_count = Arc::new(AtomicUsize::new(0));
    let errors = Arc::new(AtomicUsize::new(0));

    for worker_index in 0..worker_count {
        let files = file_ids
            .iter()
            .copied()
            .enumerate()
            .filter_map(
                |(index, file_id)| {
                    if index % worker_count == worker_index { Some(file_id) } else { None }
                },
            )
            .collect_vec();
        let remaining = Arc::clone(&remaining);
        let started = Arc::clone(&started);
        let checked = Arc::clone(&checked);
        let diagnostic_count = Arc::clone(&diagnostic_count);
        let errors = Arc::clone(&errors);

        state.spawn(move |mut snapshot| {
            let _span = tracing::info_span!("collect_diagnostics").entered();
            if show_summary {
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::INFO,
                    message: format!(
                        "Analyzer refresh worker {}/{} started",
                        worker_index + 1,
                        worker_count
                    ),
                });
            }
            for file_id in files {
                if snapshot.diagnostics_generation.load(Ordering::SeqCst) != generation {
                    break;
                }
                let started = started.fetch_add(1, Ordering::SeqCst) + 1;
                if show_summary && (started <= 8 || started.is_multiple_of(REFRESH_PROGRESS_INTERVAL)) {
                    let uri = {
                        let files = snapshot.files.read();
                        common::file_uri(&snapshot.engine, &files, file_id).ok()
                    };
                    let detail = uri
                        .as_ref()
                        .and_then(|uri| uri.to_file_path().ok())
                        .map(|path| path.to_string_lossy().to_string())
                        .unwrap_or_else(|| format!("{file_id:?}"));
                    let _ = snapshot.client.show_message(ShowMessageParams {
                        typ: MessageType::INFO,
                        message: format!(
                            "Analyzer refresh started {started}/{total}: {detail}"
                        ),
                    });
                }
                let res = collect_diagnostics_core(
                    &mut snapshot,
                    CollectDiagnostics(file_id),
                    generation,
                    publish_mode,
                );
                match res {
                    Ok(count) => {
                        let completed = checked.fetch_add(1, Ordering::SeqCst) + 1;
                        diagnostic_count.fetch_add(count, Ordering::SeqCst);
                        if show_summary && (completed <= 8 || completed.is_multiple_of(REFRESH_PROGRESS_INTERVAL)) {
                            let _ = snapshot.client.show_message(ShowMessageParams {
                                typ: MessageType::INFO,
                                message: format!(
                                    "Analyzer refresh completed {completed}/{total} file(s), last produced {count} diagnostic(s)"
                                ),
                            });
                        }
                    }
                    Err(error) => {
                        errors.fetch_add(1, Ordering::SeqCst);
                        error.emit_trace();
                    }
                }
            }

            if show_summary && remaining.fetch_sub(1, Ordering::SeqCst) == 1 {
                let checked = checked.load(Ordering::SeqCst);
                let diagnostic_count = diagnostic_count.load(Ordering::SeqCst);
                let errors = errors.load(Ordering::SeqCst);
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::INFO,
                    message: format!(
                        "Analyzer refresh checked {checked}/{total} file(s), produced {diagnostic_count} diagnostic(s), {errors} error(s)"
                    ),
                });
            }
        });
    }
}

pub(super) fn collect_diagnostics_core(
    snapshot: &mut StateSnapshot,
    CollectDiagnostics(id): CollectDiagnostics,
    generation: u64,
    publish_mode: PublishMode,
) -> Result<usize, LspError> {
    let content = snapshot.engine.content(id);

    let (parsed, _) = snapshot.engine.parsed(id)?;
    let root = parsed.syntax_node();

    let stabilized = snapshot.engine.stabilized(id)?;
    let indexed = snapshot.engine.indexed(id)?;
    let resolved = snapshot.engine.resolved(id)?;
    let lowered = snapshot.engine.lowered(id)?;
    let checked = snapshot.engine.checked(id)?;

    let uri = {
        let files = snapshot.files.read();
        common::file_uri(&snapshot.engine, &files, id)?
    };

    // Some internal/stdlib files use non-file schemes (e.g. prim://...).
    // Emacs lsp-mode assumes diagnostics are for file:// URIs.
    if uri.scheme() != "file" {
        return Ok(0);
    }

    let context =
        DiagnosticsContext::new(&snapshot.engine, &content, &root, &stabilized, &indexed, &lowered);

    let mut all_diagnostics = vec![];

    for error in &lowered.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    for error in &resolved.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    for error in &checked.errors {
        all_diagnostics.extend(error.to_diagnostics(&context));
    }

    let to_position = |offset: u32| locate::offset_to_position(&content, TextSize::from(offset));

    let diagnostics: Vec<Diagnostic> = all_diagnostics
        .iter()
        .filter_map(|diagnostic| {
            let start = to_position(diagnostic.primary.start)?;
            let end = to_position(diagnostic.primary.end)?;
            let range = Range { start, end };

            let severity = match diagnostic.severity {
                diagnostics::Severity::Error => DiagnosticSeverity::ERROR,
                diagnostics::Severity::Warning => DiagnosticSeverity::WARNING,
            };

            let related_information = diagnostic.related.iter().filter_map(|related| {
                let start = to_position(related.span.start)?;
                let end = to_position(related.span.end)?;
                Some(DiagnosticRelatedInformation {
                    location: Location { uri: uri.clone(), range: Range { start, end } },
                    message: related.message.clone(),
                })
            });

            let related_information = related_information.collect_vec();

            Some(Diagnostic {
                range,
                severity: Some(severity),
                code: Some(NumberOrString::String(diagnostic.code.to_string())),
                code_description: None,
                source: Some(format!("analyzer/{}", diagnostic.source)),
                message: diagnostic.message.clone(),
                related_information: if related_information.is_empty() {
                    None
                } else {
                    Some(related_information)
                },
                tags: None,
                data: None,
            })
        })
        .collect();

    // Store analyzer diagnostics then publish merged diagnostics (build + analyzer).
    if snapshot.diagnostics_generation.load(Ordering::SeqCst) != generation {
        return Ok(0);
    }
    let diagnostic_count = diagnostics.len();
    {
        let mut map = snapshot.analyzer_diagnostics.write();
        if diagnostics.is_empty() {
            map.remove(&uri);
        } else {
            map.insert(uri.clone(), diagnostics);
        }
    }

    if snapshot.diagnostics_generation.load(Ordering::SeqCst) != generation {
        return Ok(0);
    }

    match publish_mode {
        PublishMode::ChangedOnly => snapshot.publish_merged_diagnostics_if_changed(uri)?,
    }

    Ok(diagnostic_count)
}

pub(crate) fn rebuild_workspace_graph(state: &mut State) -> Result<(), LspError> {
    {
        let mut wg = state.workspace_graph.write();
        wg.imports.clear();
        wg.dependants.clear();
    }

    let file_ids = {
        let files = state.files.read();
        files.iter_id().collect_vec()
    };

    for file_id in file_ids {
        update_workspace_graph_for_file(state, file_id)?;
    }

    Ok(())
}

pub(crate) fn update_workspace_graph_for_file(
    state: &mut State,
    file_id: FileId,
) -> Result<(), LspError> {
    let uri = {
        let files = state.files.read();
        Url::parse(files.path(file_id).as_ref()).ok()
    };

    let Some(uri) = uri else {
        state.workspace_graph.write().remove_file(file_id);
        return Ok(());
    };

    if !is_refreshable_workspace_source_uri(
        state.root.as_deref(),
        &state.config.analyzer_excluded_dir,
        &uri,
    ) {
        state.workspace_graph.write().remove_file(file_id);
        return Ok(());
    }

    let indexed = state.engine.indexed(file_id)?;
    let mut imports = HashSet::new();
    for import in indexed.imports.values() {
        let Some(module_name) = &import.name else {
            continue;
        };
        let Some(import_file_id) = state.engine.module_file(module_name) else {
            continue;
        };
        let import_uri = {
            let files = state.files.read();
            Url::parse(files.path(import_file_id).as_ref()).ok()
        };
        if import_uri.is_some_and(|uri| {
            is_refreshable_workspace_source_uri(
                state.root.as_deref(),
                &state.config.analyzer_excluded_dir,
                &uri,
            )
        }) {
            imports.insert(import_file_id);
        }
    }

    state.workspace_graph.write().replace_imports(file_id, imports);
    Ok(())
}

pub(crate) fn schedule_dependant_diagnostics(state: &mut State, file_id: FileId) {
    let ordered = ordered_dependant_diagnostics(state, file_id);
    schedule_diagnostics(state, ordered, PublishMode::ChangedOnly, false);
}

pub(crate) fn ordered_dependant_diagnostics(state: &State, file_id: FileId) -> Vec<FileId> {
    let direct = state.workspace_graph.read().direct_dependants(file_id);
    let transitive = state.workspace_graph.read().transitive_dependants(file_id);
    let open_uris = state.open_uris.read().clone();
    let files = state.files.read();

    let is_open =
        |id: FileId| Url::parse(files.path(id).as_ref()).is_ok_and(|uri| open_uris.contains(&uri));

    let mut ordered = vec![file_id];
    let mut seen: HashSet<FileId> = HashSet::from_iter([file_id]);

    let mut push_group = |ids: Vec<FileId>| {
        for id in ids {
            if seen.insert(id) {
                ordered.push(id);
            }
        }
    };

    let mut open_direct = direct.iter().copied().filter(|&id| is_open(id)).collect_vec();
    let mut open_transitive =
        transitive.iter().copied().filter(|id| !direct.contains(id) && is_open(*id)).collect_vec();
    let mut closed_direct = direct.iter().copied().filter(|&id| !is_open(id)).collect_vec();
    let mut closed_transitive =
        transitive.iter().copied().filter(|id| !direct.contains(id) && !is_open(*id)).collect_vec();

    open_direct.sort();
    open_transitive.sort();
    closed_direct.sort();
    closed_transitive.sort();

    push_group(open_direct);
    push_group(open_transitive);
    push_group(closed_direct);
    push_group(closed_transitive);

    ordered
}
