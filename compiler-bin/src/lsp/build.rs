use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process;

use async_lsp::LanguageClient;
use async_lsp::lsp_types::*;
use globset::{Glob, GlobSetBuilder};
use path_absolutize::Absolutize;
use serde_json::Value;
use walkdir::WalkDir;

use once_cell::sync::Lazy;

use crate::cli;
use crate::lsp::error::LspError;
use crate::lsp::StateSnapshot;

pub struct Build;

pub fn build(state: &mut crate::lsp::State, _: Build) -> Result<(), LspError> {
    state.spawn(|snapshot| {
        let _span = tracing::info_span!("build").entered();
        build_core(snapshot).inspect_err(|e| e.emit_trace())
    });
    Ok(())
}

fn build_core(mut snapshot: StateSnapshot) -> Result<(), LspError> {
    let root = snapshot.root.as_ref().ok_or(LspError::MissingRoot)?;

    let known_uris: Vec<Url> = {
        let files = snapshot.files.read();
        files
            .iter_id()
            .map(|id| Url::parse(files.path(id).as_ref()))
            .collect::<Result<Vec<_>, _>>()
    }?;

    let tool = match snapshot.config.build_tool {
        cli::BuildTool::Spago => cli::BuildTool::Spago,
        cli::BuildTool::Purs => cli::BuildTool::Purs,
        cli::BuildTool::Auto => {
            let has_lock = root.join("spago.lock").exists();
            if has_lock { cli::BuildTool::Spago } else { cli::BuildTool::Purs }
        }
    };

    let output = match tool {
        cli::BuildTool::Spago => run_spago(root, &snapshot.config.build_arg)?,
        cli::BuildTool::Purs => {
            let sources = workspace_sources(root, &snapshot.config)?;
            run_purs(root, &sources, &snapshot.config.build_arg)?
        }
        cli::BuildTool::Auto => unreachable!(),
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let json_text = if stderr.contains('{') || stderr.contains('[') { stderr.as_ref() } else { stdout.as_ref() };

    let diagnostics_by_uri = parse_purs_json_errors(json_text, tool);
    let build_map = match &diagnostics_by_uri {
        Ok(map) => map,
        Err(_) => {
            // Still clear existing diagnostics even if parsing fails.
            static EMPTY: Lazy<HashMap<Url, Vec<Diagnostic>>> = Lazy::new(HashMap::new);
            &EMPTY
        }
    };

    // Replace any previously published diagnostics: always clear known files first.
    // Then publish build diagnostics for files reported by the compiler.
    let publish_plan = build_publish_plan(&known_uris, build_map);

    for (uri, diagnostics) in publish_plan {
        let _ = snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        });
    }

    match diagnostics_by_uri {
        Ok(_) => {
            if output.status.success() {
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::INFO,
                    message: "Build completed".to_string(),
                });
            } else {
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::ERROR,
                    message: "Build failed".to_string(),
                });
            }
            Ok(())
        }
        Err(err) => {
            let _ = snapshot.client.show_message(ShowMessageParams {
                typ: MessageType::ERROR,
                message: format!("Failed to parse build diagnostics: {err}"),
            });
            Err(err)
        }
    }
}

fn build_publish_plan(
    known_uris: &[Url],
    build_diagnostics: &HashMap<Url, Vec<Diagnostic>>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let mut plan = Vec::with_capacity(known_uris.len() + build_diagnostics.len());

    // First clear everything we already know about.
    for uri in known_uris {
        plan.push((uri.clone(), vec![]));
    }

    // Then publish build diagnostics.
    for (uri, diagnostics) in build_diagnostics {
        plan.push((uri.clone(), diagnostics.clone()));
    }

    plan
}

fn run_spago(root: &Path, extra_args: &[String]) -> Result<process::Output, LspError> {
    let mut cmd = process::Command::new("spago");
    cmd.current_dir(root);
    cmd.arg("build");
    cmd.arg("--purs-args");
    cmd.arg("--json-errors");
    cmd.args(extra_args);
    Ok(cmd.output()?)
}

fn run_purs(
    root: &Path,
    sources: &[PathBuf],
    extra_args: &[String],
) -> Result<process::Output, LspError> {
    let mut cmd = process::Command::new("purs");
    cmd.current_dir(root);
    cmd.arg("compile");
    cmd.arg("--json-errors");
    cmd.args(extra_args);
    cmd.args(sources);
    Ok(cmd.output()?)
}

fn workspace_sources(root: &Path, config: &cli::Config) -> Result<Vec<PathBuf>, LspError> {
    if let Some(command) = config.source_command.as_deref() {
        sources_from_command(root, command)
    } else {
        Ok(spago::source_files(root).map_err(LspError::SpagoLock)?)
    }
}

fn sources_from_command(root: &Path, command: &str) -> Result<Vec<PathBuf>, LspError> {
    let mut parts = command.split(' ');
    let program = parts.next().ok_or(LspError::InvalidSourceCommand)?;

    let mut cmd = process::Command::new(program);
    cmd.current_dir(root);
    cmd.args(parts);

    let output = cmd.output()?;
    let output = std::str::from_utf8(&output.stdout)?;

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

    let files_from_glob = WalkDir::new(root).into_iter().filter_map(move |entry| {
        let entry = entry.ok()?;
        let path = entry.path();
        if globs.matches(path).is_empty() { None } else { Some(path.to_path_buf()) }
    });

    files.extend(files_from_glob);
    Ok(files)
}

fn parse_purs_json_errors(
    text: &str,
    tool: cli::BuildTool,
) -> Result<HashMap<Url, Vec<Diagnostic>>, LspError> {
    let mut map: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

    // Try whole-buffer parse first.
    if let Ok(value) = serde_json::from_str::<Value>(text.trim()) {
        extend_from_value(&mut map, value, tool)?;
        return Ok(map);
    }

    // Fall back to line-by-line JSON values (tolerate mixed output).
    for line in text.lines() {
        let line = line.trim();
        if !(line.starts_with('{') || line.starts_with('[')) {
            continue;
        }
        if let Ok(value) = serde_json::from_str::<Value>(line) {
            extend_from_value(&mut map, value, tool)?;
        }
    }

    Ok(map)
}

fn extend_from_value(
    map: &mut HashMap<Url, Vec<Diagnostic>>,
    value: Value,
    tool: cli::BuildTool,
) -> Result<(), LspError> {
    let errors: Vec<Value> = match value {
        Value::Array(arr) => arr,
        Value::Object(obj) => obj
            .get("errors")
            .and_then(|v| v.as_array())
            .cloned()
            .unwrap_or_default(),
        _ => vec![],
    };

    for err in errors {
        let Some(filename) = err.get("filename").and_then(|v| v.as_str()) else { continue };
        let uri = if filename.starts_with("file://") {
            Url::parse(filename)?
        } else {
            Url::from_file_path(filename).map_err(|_| {
                LspError::PathParseFail(PathBuf::from(filename))
            })?
        };

        let diagnostic = error_to_diagnostic(&err, tool);
        map.entry(uri).or_default().push(diagnostic);
    }

    Ok(())
}

fn error_to_diagnostic(err: &Value, tool: cli::BuildTool) -> Diagnostic {
    let message = extract_message(err);
    let range = extract_range(err).unwrap_or(Range {
        start: Position::new(0, 0),
        end: Position::new(0, 0),
    });

    let source = match tool {
        cli::BuildTool::Spago => "build/spago",
        cli::BuildTool::Purs => "build/purs",
        cli::BuildTool::Auto => "build",
    };

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some(source.to_string()),
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}

fn extract_message(err: &Value) -> String {
    if let Some(s) = err.get("message").and_then(|v| v.as_str()) {
        return s.to_string();
    }

    // Some compiler versions emit structured messages.
    if let Some(arr) = err.get("message").and_then(|v| v.as_array()) {
        let mut out = String::new();
        for part in arr {
            let s = part
                .get("text")
                .and_then(|v| v.as_str())
                .or_else(|| part.as_str())
                .unwrap_or("");
            out.push_str(s);
        }
        if !out.is_empty() {
            return out;
        }
    }

    "Build error".to_string()
}

fn extract_range(err: &Value) -> Option<Range> {
    let pos = err.get("position")?;
    let obj = pos.as_object()?;

    let start_line = obj.get("startLine")?.as_u64()?;
    let start_col = obj.get("startColumn")?.as_u64()?;
    let end_line = obj.get("endLine")?.as_u64()?;
    let end_col = obj.get("endColumn")?.as_u64()?;

    // purs positions are 1-based.
    let start = Position::new((start_line.saturating_sub(1)) as u32, (start_col.saturating_sub(1)) as u32);
    let end = Position::new((end_line.saturating_sub(1)) as u32, (end_col.saturating_sub(1)) as u32);
    Some(Range { start, end })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_accepts_errors_object() {
        let input = r#"{"errors":[{"filename":"file:///test/Main.purs","position":{"startLine":1,"startColumn":1,"endLine":1,"endColumn":4},"message":"nope"}]}"#;
        let map = parse_purs_json_errors(input, cli::BuildTool::Purs).unwrap();
        let (uri, diags) = map.into_iter().next().unwrap();
        assert_eq!(uri.as_str(), "file:///test/Main.purs");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "nope");
        assert_eq!(diags[0].range.start, Position::new(0, 0));
    }

    #[test]
    fn publish_plan_clears_known_files_first() {
        let a = Url::parse("file:///test/A.purs").unwrap();
        let b = Url::parse("file:///test/B.purs").unwrap();

        let mut build_map: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
        build_map.insert(
            b.clone(),
            vec![Diagnostic {
                range: Range { start: Position::new(0, 0), end: Position::new(0, 0) },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("build/purs".to_string()),
                message: "nope".to_string(),
                related_information: None,
                tags: None,
                data: None,
            }],
        );

        let plan = build_publish_plan(&[a.clone(), b.clone()], &build_map);

        assert_eq!(plan[0].0, a);
        assert!(plan[0].1.is_empty());
        assert_eq!(plan[1].0, b);
        assert!(plan[1].1.is_empty());

        assert_eq!(plan[2].0, b);
        assert_eq!(plan[2].1.len(), 1);
    }
}
