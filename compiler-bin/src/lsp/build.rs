use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{self, Stdio};
use std::time::Duration;

use async_lsp::LanguageClient;
use async_lsp::lsp_types::*;
use globset::{Glob, GlobSetBuilder};
use path_absolutize::Absolutize;
use serde_json::Value;
use tokio::io::AsyncReadExt;
use tokio::process as tokio_process;
use tokio::time;
use walkdir::WalkDir;

use crate::cli;
use crate::lsp::StateSnapshot;
use crate::lsp::error::LspError;

pub struct Build;

const BUILD_COMMAND_TIMEOUT: Duration = Duration::from_secs(60 * 5);

pub fn build(state: &mut crate::lsp::State, _: Build) -> Result<(), LspError> {
    state.spawn(|snapshot| {
        let _span = tracing::info_span!("build").entered();
        build_core(snapshot).inspect_err(|e| e.emit_trace())
    });
    Ok(())
}

fn build_core(mut snapshot: StateSnapshot) -> Result<(), LspError> {
    let generation = snapshot.diagnostics_generation.load(std::sync::atomic::Ordering::SeqCst);
    // Clone to avoid holding an immutable borrow of snapshot across client calls.
    let root = snapshot.root.clone().ok_or(LspError::MissingRoot)?;

    // Copy out config we need so we don't keep an immutable borrow of `snapshot`
    // alive while publishing diagnostics.
    let build_tool_cfg = snapshot.config.build_tool;
    let build_arg = snapshot.config.build_arg.clone();
    let source_command = snapshot.config.source_command.clone();

    // Only update diagnostics for URIs we previously published build diagnostics for,
    // plus any URIs reported by the current build. This avoids flooding clients.
    let previously_published: Vec<Url>;
    {
        let map = snapshot.build_diagnostics.read();
        previously_published = map.keys().cloned().collect();
    }

    let tool = match build_tool_cfg {
        cli::BuildTool::Spago => cli::BuildTool::Spago,
        cli::BuildTool::Purs => cli::BuildTool::Purs,
        cli::BuildTool::Auto => {
            let has_lock = root.join("spago.lock").exists();
            if has_lock { cli::BuildTool::Spago } else { cli::BuildTool::Purs }
        }
    };

    let output = match tool {
        cli::BuildTool::Spago => {
            tokio::runtime::Handle::current().block_on(run_spago(&root, &build_arg))?
        }
        cli::BuildTool::Purs => {
            let sources = workspace_sources(&root, source_command.as_deref())?;
            tokio::runtime::Handle::current().block_on(run_purs(&root, &sources, &build_arg))?
        }
        cli::BuildTool::Auto => unreachable!(),
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Spago/purs output varies by version: JSON may appear on stdout or stderr,
    // and may be interleaved with other text. Parse both.
    let combined = format!("{stderr}\n{stdout}");
    let diagnostics_by_uri = parse_purs_json_errors(&combined, tool, &root);

    // Use an owned map so we don't hold immutable borrows across client calls.
    // If parsing fails, still proceed with build success/failure messaging.
    let build_map: HashMap<Url, Vec<Diagnostic>> = match diagnostics_by_uri {
        Ok(map) => map,
        Err(err) => {
            let _ = snapshot.client.show_message(ShowMessageParams {
                typ: MessageType::WARNING,
                message: format!("Failed to parse build diagnostics JSON: {err}"),
            });
            HashMap::new()
        }
    };

    // If a reset happened while building, don't publish stale diagnostics.
    if snapshot.diagnostics_generation.load(std::sync::atomic::Ordering::SeqCst) != generation {
        return Ok(());
    }

    // Update stored build diagnostics (file:// only).
    let mut affected: Vec<Url> = previously_published;
    {
        let mut map = snapshot.build_diagnostics.write();
        map.clear();
        for (uri, diags) in &build_map {
            if uri.scheme() != "file" {
                continue;
            }
            map.insert(uri.clone(), diags.clone());
        }
    }

    // Publish merged diagnostics for any URI touched by the previous or current build.
    affected.extend(build_map.keys().filter(|u| u.scheme() == "file").cloned());
    affected.sort();
    affected.dedup();

    for uri in affected {
        if snapshot.diagnostics_generation.load(std::sync::atomic::Ordering::SeqCst) != generation {
            return Ok(());
        }
        let diagnostics = snapshot.merged_diagnostics_for_uri(&uri);
        let _ = snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        });
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

                let _ = snapshot
                    .client
                    .show_message(ShowMessageParams { typ: MessageType::ERROR, message: msg });
            } else {
                let _ = snapshot.client.show_message(ShowMessageParams {
                    typ: MessageType::ERROR,
                    message: "Build failed".to_string(),
                });
            }
        }
        Ok(())
    }
}

async fn run_spago(root: &Path, extra_args: &[String]) -> Result<process::Output, LspError> {
    let mut cmd = tokio_process::Command::new("spago");
    cmd.current_dir(root);
    cmd.arg("build");
    // Spago has its own --json-errors flag; it must not be forwarded to purs.
    cmd.arg("--json-errors");

    // Our config's build args are purs args; forward via --purs-args.
    if !extra_args.is_empty() {
        cmd.arg("--purs-args");
        cmd.arg(extra_args.join(" "));
    }
    output_with_timeout(cmd, "spago build").await
}

async fn run_purs(
    root: &Path,
    sources: &[PathBuf],
    extra_args: &[String],
) -> Result<process::Output, LspError> {
    let mut cmd = tokio_process::Command::new("purs");
    cmd.current_dir(root);
    cmd.arg("compile");
    cmd.arg("--json-errors");
    cmd.args(extra_args);
    cmd.args(sources);
    output_with_timeout(cmd, "purs compile").await
}

async fn output_with_timeout(
    mut cmd: tokio_process::Command,
    command_name: &'static str,
) -> Result<process::Output, LspError> {
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());
    let mut child = cmd.spawn()?;
    let mut stdout = child.stdout.take().ok_or(LspError::MissingProcessPipe("stdout"))?;
    let mut stderr = child.stderr.take().ok_or(LspError::MissingProcessPipe("stderr"))?;
    let stdout_task = tokio::spawn(async move {
        let mut buf = vec![];
        stdout.read_to_end(&mut buf).await.map(|_| buf)
    });
    let stderr_task = tokio::spawn(async move {
        let mut buf = vec![];
        stderr.read_to_end(&mut buf).await.map(|_| buf)
    });

    let status = match time::timeout(BUILD_COMMAND_TIMEOUT, child.wait()).await {
        Ok(status) => status?,
        Err(_) => {
            let _ = child.start_kill();
            let _ = child.wait().await;
            return Err(LspError::BuildTimeout {
                command: command_name,
                timeout: BUILD_COMMAND_TIMEOUT,
            });
        }
    };

    Ok(process::Output { status, stdout: stdout_task.await??, stderr: stderr_task.await?? })
}

fn workspace_sources(root: &Path, source_command: Option<&str>) -> Result<Vec<PathBuf>, LspError> {
    if let Some(command) = source_command {
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
    root: &Path,
) -> Result<HashMap<Url, Vec<Diagnostic>>, LspError> {
    let mut map: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

    // Try whole-buffer parse first.
    if let Ok(value) = serde_json::from_str::<Value>(text.trim()) {
        extend_from_value(&mut map, value, tool, root)?;
        return Ok(map);
    }

    // Fall back to line-by-line JSON values (tolerate mixed output).
    for line in text.lines() {
        let line = line.trim();
        if !(line.starts_with('{') || line.starts_with('[')) {
            continue;
        }
        if let Ok(value) = serde_json::from_str::<Value>(line) {
            extend_from_value(&mut map, value, tool, root)?;
        }
    }

    Ok(map)
}

fn extend_from_value(
    map: &mut HashMap<Url, Vec<Diagnostic>>,
    value: Value,
    tool: cli::BuildTool,
    root: &Path,
) -> Result<(), LspError> {
    let (errors, warnings): (Vec<Value>, Vec<Value>) = match value {
        Value::Array(arr) => (arr, vec![]),
        Value::Object(obj) => {
            let errors = obj.get("errors").and_then(|v| v.as_array()).cloned().unwrap_or_default();
            let warnings =
                obj.get("warnings").and_then(|v| v.as_array()).cloned().unwrap_or_default();
            (errors, warnings)
        }
        _ => (vec![], vec![]),
    };

    extend_diagnostics(map, errors, DiagnosticSeverity::ERROR, tool, root)?;
    extend_diagnostics(map, warnings, DiagnosticSeverity::WARNING, tool, root)?;

    Ok(())
}

fn extend_diagnostics(
    map: &mut HashMap<Url, Vec<Diagnostic>>,
    diagnostics: Vec<Value>,
    severity: DiagnosticSeverity,
    tool: cli::BuildTool,
    root: &Path,
) -> Result<(), LspError> {
    for err in diagnostics {
        let Some(filename) = err.get("filename").and_then(|v| v.as_str()) else { continue };
        let uri = if filename.starts_with("file://") {
            Url::parse(filename)?
        } else {
            // Spago emits relative paths like "src/Foo.purs". Make them absolute.
            let path = Path::new(filename);
            let abs = if path.is_absolute() { PathBuf::from(path) } else { root.join(path) };
            Url::from_file_path(&abs).map_err(|_| LspError::PathParseFail(abs))?
        };

        let diagnostic = error_to_diagnostic(&err, severity, tool);
        map.entry(uri).or_default().push(diagnostic);
    }

    Ok(())
}

fn error_to_diagnostic(
    err: &Value,
    severity: DiagnosticSeverity,
    tool: cli::BuildTool,
) -> Diagnostic {
    let message = extract_message(err);
    let range = extract_range(err)
        .unwrap_or(Range { start: Position::new(0, 0), end: Position::new(0, 0) });

    let source = match tool {
        cli::BuildTool::Spago => "build/spago",
        cli::BuildTool::Purs => "build/purs",
        cli::BuildTool::Auto => "build",
    };

    Diagnostic {
        range,
        severity: Some(severity),
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
            let s =
                part.get("text").and_then(|v| v.as_str()).or_else(|| part.as_str()).unwrap_or("");
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
    let start =
        Position::new((start_line.saturating_sub(1)) as u32, (start_col.saturating_sub(1)) as u32);
    let end =
        Position::new((end_line.saturating_sub(1)) as u32, (end_col.saturating_sub(1)) as u32);
    Some(Range { start, end })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_accepts_errors_object() {
        let input = r#"{"errors":[{"filename":"file:///test/Main.purs","position":{"startLine":1,"startColumn":1,"endLine":1,"endColumn":4},"message":"nope"}],"warnings":[{"filename":"file:///test/Main.purs","position":{"startLine":2,"startColumn":1,"endLine":2,"endColumn":4},"message":"careful"}]}"#;
        let map = parse_purs_json_errors(input, cli::BuildTool::Purs, Path::new("/")).unwrap();
        let (uri, diags) = map.into_iter().next().unwrap();
        assert_eq!(uri.as_str(), "file:///test/Main.purs");
        assert_eq!(diags.len(), 2);
        assert_eq!(diags[0].message, "nope");
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diags[0].range.start, Position::new(0, 0));
        assert_eq!(diags[1].message, "careful");
        assert_eq!(diags[1].severity, Some(DiagnosticSeverity::WARNING));
    }

    // Note: build publishing behavior is tested indirectly via LSP integration/unit tests.
}
