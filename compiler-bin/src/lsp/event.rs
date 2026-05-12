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
use std::path::{Component, Path};
use std::sync::atomic::Ordering;

pub struct AnalyzerRefresh;

pub fn analyzer_refresh(state: &mut State, _: AnalyzerRefresh) -> Result<(), LspError> {
    // Best-effort: cancel in-flight queries so refresh work wins.
    state.engine.request_cancel();

    let (file_ids, mut excluded_uris): (Vec<FileId>, Vec<Url>) = {
        let files = state.files.read();
        files
            .iter_id()
            .filter_map(|file_id| {
                let path = files.path(file_id);
                let uri = Url::parse(path.as_ref()).ok()?;
                if is_refreshable_workspace_source_uri(state.root.as_deref(), &uri) {
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
    };

    {
        let analyzer = state.analyzer_diagnostics.read();
        excluded_uris.extend(
            analyzer
                .keys()
                .filter(|uri| !is_refreshable_workspace_source_uri(state.root.as_deref(), uri))
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
        let diagnostics = {
            let build = state.build_diagnostics.read();
            build.get(&uri).cloned().unwrap_or_default()
        };
        state.client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        })?;
    }

    for file_id in file_ids {
        // Reuse existing per-file diagnostic computation.
        collect_diagnostics(state, CollectDiagnostics(file_id))?;
    }
    Ok(())
}

fn is_refreshable_workspace_source_uri(root: Option<&Path>, uri: &Url) -> bool {
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

    !path.components().any(|component| match component {
        Component::Normal(name) => {
            matches!(name.to_str(), Some(".spago" | "output" | ".git" | "node_modules"))
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

        // Drop build diagnostics and republish merged diagnostics for affected URIs.
        snapshot.build_diagnostics.write().clear();
        for uri in build_uris {
            let diagnostics = snapshot.merged_diagnostics_for_uri(&uri);
            let _ = snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            });
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

        assert!(is_refreshable_workspace_source_uri(Some(&root), &source_uri));
        assert!(is_refreshable_workspace_source_uri(Some(&root), &test_uri));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &dependency_uri));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &output_uri));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &external_uri));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &generated_purs_uri));
        assert!(!is_refreshable_workspace_source_uri(Some(&root), &prim_uri));
    }
}

pub fn emit_collect_diagnostics(state: &mut State, uri: Url) -> Result<(), LspError> {
    let files = state.files.read();
    let uri = uri.as_str();

    if let Some(file_id) = files.id(uri) {
        state.client.emit(CollectDiagnostics(file_id))?;
    }

    Ok(())
}

pub struct CollectDiagnostics(FileId);

pub fn collect_diagnostics(state: &mut State, id: CollectDiagnostics) -> Result<(), LspError> {
    state.spawn(move |snapshot| {
        let _span = tracing::info_span!("collect_diagnostics").entered();
        collect_diagnostics_core(snapshot, id).inspect_err(|error| error.emit_trace())
    });
    Ok(())
}

fn collect_diagnostics_core(
    mut snapshot: StateSnapshot,
    CollectDiagnostics(id): CollectDiagnostics,
) -> Result<(), LspError> {
    let generation = snapshot.diagnostics_generation.load(Ordering::SeqCst);

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
        return Ok(());
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
        return Ok(());
    }
    {
        let mut map = snapshot.analyzer_diagnostics.write();
        if diagnostics.is_empty() {
            map.remove(&uri);
        } else {
            map.insert(uri.clone(), diagnostics);
        }
    }

    let merged = snapshot.merged_diagnostics_for_uri(&uri);

    if snapshot.diagnostics_generation.load(Ordering::SeqCst) != generation {
        return Ok(());
    }

    snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
        uri,
        diagnostics: merged,
        version: None,
    })?;

    Ok(())
}
