use std::sync::Arc;

use analyzer::{Files, QueryEngine, common, locate, prim};
use async_lsp::LanguageClient;
use async_lsp::lsp_types::*;
use diagnostics::{DiagnosticsContext, ToDiagnostics};
use files::FileId;
use itertools::Itertools;
use parking_lot::RwLock;
use path_absolutize::Absolutize;
use rowan::TextSize;

use crate::lsp::error::LspError;
use crate::lsp::{State, StateSnapshot};

pub struct AnalyzerRefresh;

pub fn analyzer_refresh(state: &mut State, _: AnalyzerRefresh) -> Result<(), LspError> {
    // Best-effort: cancel in-flight queries so refresh work wins.
    state.engine.request_cancel();

    let file_ids: Vec<FileId> = {
        let files = state.files.read();
        files.iter_id().collect()
    };

    for file_id in file_ids {
        // Reuse existing per-file diagnostic computation.
        collect_diagnostics(state, CollectDiagnostics(file_id))?;
    }
    Ok(())
}

pub struct Reset;

pub fn reset(state: &mut State, _: Reset) -> Result<(), LspError> {
    state.engine.request_cancel();

    // Clear any published diagnostics (build + analyzer).
    let mut uris_to_clear: Vec<Url> = {
        let build = state.build_diagnostics.read();
        let analyzer = state.analyzer_diagnostics.read();
        build
            .keys()
            .chain(analyzer.keys())
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

    for uri in uris_to_clear {
        let _ = state.client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics: vec![],
            version: None,
        });
    }

    // Reset analyzer state.
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);
    state.engine = engine;
    state.files = Arc::new(RwLock::new(files));

    state.invalidate_workspace_symbols();
    state.invalidate_suggestions_cache();

    // Reload workspace sources (same mechanism as initialization).
    let config = Arc::clone(&state.config);
    if let Some(command) = config.source_command.as_deref() {
        super::initialized_manual(state, command)
    } else {
        super::initialized_spago(state)
    }
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
    {
        let mut map = snapshot.analyzer_diagnostics.write();
        if diagnostics.is_empty() {
            map.remove(&uri);
        } else {
            map.insert(uri.clone(), diagnostics);
        }
    }

    let merged = snapshot.merged_diagnostics_for_uri(&uri);
    snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
        uri,
        diagnostics: merged,
        version: None,
    })?;

    Ok(())
}
