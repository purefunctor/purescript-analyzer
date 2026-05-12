use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process;

use async_lsp::LanguageClient;
use async_lsp::lsp_types::*;
use globset::{Glob, GlobSetBuilder};
use path_absolutize::Absolutize;
use serde_json::Value;
use walkdir::WalkDir;

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
    // Clone to avoid holding an immutable borrow of snapshot across client calls.
    let root = snapshot.root.clone().ok_or(LspError::MissingRoot)?;

    // Only clear diagnostics we previously published for builds.
    // Publishing one empty-diagnostics notification per known file can freeze clients.
    let previously_published: Vec<Url> = {
        let set = snapshot.build_diagnostics_uris.read();
        set.iter().cloned().collect()
    };

    let tool = match snapshot.config.build_tool {
        cli::BuildTool::Spago => cli::BuildTool::Spago,
        cli::BuildTool::Purs => cli::BuildTool::Purs,
        cli::BuildTool::Auto => {
            let has_lock = root.join("spago.lock").exists();
            if has_lock { cli::BuildTool::Spago } else { cli::BuildTool::Purs }
        }
    };

    let output = match tool {
        cli::BuildTool::Spago => run_spago(&root, &snapshot.config.build_arg)?,
        cli::BuildTool::Purs => {
            let sources = workspace_sources(&root, &snapshot.config)?;
            run_purs(&root, &sources, &snapshot.config.build_arg)?
        }
        cli::BuildTool::Auto => unreachable!(),
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Spago/purs output varies by version: JSON may appear on stdout or stderr,
    // and may be interleaved with other text. Parse both.
    let combined = format!("{stderr}\n{stdout}");
    let diagnostics_by_uri = parse_purs_json_errors(&combined, tool);
    // Use an owned map so we don't hold immutable borrows across client calls.
    let build_map: HashMap<Url, Vec<Diagnostic>> = match diagnostics_by_uri {
        Ok(map) => map,
        Err(_) => HashMap::new(),
    };

    // Clear previous build diagnostics, then publish current build diagnostics.
    for uri in previously_published {
        let _ = snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics: vec![],
            version: None,
        });
    }

    for (uri, diagnostics) in &build_map {
            let _ = snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostics.clone(),
                version: None,
            });
    }

    // Update the set of URIs we consider "build diagnostics".
    {
        let mut set = snapshot.build_diagnostics_uris.write();
        set.clear();
        set.extend(build_map.keys().cloned());
    }

    // We already materialized build_map; treat parse failures as "no build diagnostics".
    // Still provide Build completed/failed based on exit status.
    {
            if output.status.success() {
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::INFO,
                    message: "Build completed".to_string(),
                });
            } else {
                if build_map.is_empty() {
                    // Build failed but we didn't get any parseable JSON errors.
                    // Surface the tool output for debugging.
                    let mut msg = String::new();
                    msg.push_str("Build failed (no JSON diagnostics parsed).\n");
                    if !stderr.trim().is_empty() {
                        msg.push_str("stderr:\n");
                        msg.push_str(stderr.trim());
                        msg.push('\n');
                    }
                    if !stdout.trim().is_empty() {
                        msg.push_str("stdout:\n");
                        msg.push_str(stdout.trim());
                        msg.push('\n');
                    }
                    // Avoid sending extremely large messages.
                    const LIMIT: usize = 8000;
                    if msg.len() > LIMIT {
                        msg.truncate(LIMIT);
                        msg.push_str("\n…(truncated)…");
                    }

                    let _ = snapshot.client.show_message(ShowMessageParams {
                        typ: MessageType::ERROR,
                        message: msg,
                    });
                }
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::ERROR,
                    message: "Build failed".to_string(),
                });
            }
            Ok(())
    }
}

fn run_spago(root: &Path, extra_args: &[String]) -> Result<process::Output, LspError> {
    let mut cmd = process::Command::new("spago");
    cmd.current_dir(root);
    cmd.arg("build");
    // Spago has its own --json-errors flag; it must not be forwarded to purs.
    cmd.arg("--json-errors");

    // Our config's build args are purs args; forward via --purs-args.
    if !extra_args.is_empty() {
        cmd.arg("--purs-args");
        cmd.arg(extra_args.join(" "));
    }
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

    // Note: build publishing behavior is tested indirectly via LSP integration/unit tests.
}
