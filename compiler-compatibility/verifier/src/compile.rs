use std::collections::HashMap;
use std::fs;

use building::{QueryEngine, QueryError, prim};
use checking::ExternalQueries as _;
use diagnostics::{
    Diagnostic, DiagnosticsContext, Severity, Span, ToDiagnostics, format_rustc_with_path,
};
use files::{FileId, Files};
use url::Url;

use crate::error::VerifierError;
use crate::report::{CompilerDiagnostic, SpanReport, VerifierIssue};
use crate::sources::SourceFile;

#[derive(Debug, Default)]
pub struct CompileReport {
    pub diagnostics: Vec<CompilerDiagnostic>,
    pub verifier_errors: Vec<VerifierIssue>,
}

#[derive(Debug, Clone)]
struct FileMeta {
    package: String,
    version: String,
    path: String,
    content: String,
}

pub fn compile_sources(source_files: &[SourceFile]) -> Result<CompileReport, VerifierError> {
    let mut engine = QueryEngine::default();
    let mut files = Files::default();
    prim::configure(&mut engine, &mut files);

    let mut report = CompileReport::default();
    let mut file_ids = Vec::new();
    let mut meta = HashMap::new();

    for source in source_files {
        let content = fs::read_to_string(&source.path)?;
        let absolute_path = fs::canonicalize(&source.path)?;
        let uri = Url::from_file_path(&absolute_path)
            .map_err(|_| VerifierError::FileUrl(absolute_path.clone()))?
            .to_string();
        let file_id = files.insert(uri, content.clone());
        engine.set_content(file_id, content.clone());
        file_ids.push(file_id);
        meta.insert(
            file_id,
            FileMeta {
                package: source.package.clone(),
                version: source.version.clone(),
                path: source.path.to_string_lossy().into_owned(),
                content,
            },
        );
    }

    register_modules(&engine, &file_ids, &meta, &mut report);

    for &file_id in &file_ids {
        collect_file(
            &engine,
            file_id,
            meta.get(&file_id).expect("file metadata exists"),
            &mut report,
        );
    }

    Ok(report)
}

fn register_modules(
    engine: &QueryEngine,
    file_ids: &[FileId],
    meta: &HashMap<FileId, FileMeta>,
    report: &mut CompileReport,
) {
    let mut modules: HashMap<String, FileId> = HashMap::new();

    for &file_id in file_ids {
        let file_meta = meta.get(&file_id).expect("file metadata exists");
        match engine.parsed(file_id) {
            Ok((parsed, errors)) => {
                report.diagnostics.extend(parse_diagnostics(file_meta, &errors));

                if let Some(module_name) = parsed.module_name() {
                    let module_name = module_name.to_string();
                    if let Some(existing) = modules.get(&module_name) {
                        let first = meta.get(existing).expect("file metadata exists");
                        report.verifier_errors.push(VerifierIssue::duplicate_module(
                            &module_name,
                            &first.path,
                            &file_meta.path,
                        ));
                    } else {
                        engine.set_module_file(&module_name, file_id);
                        modules.insert(module_name, file_id);
                    }
                }
            }
            Err(error) => push_query_error(report, file_meta, "parse", error),
        }
    }
}

fn collect_file(
    engine: &QueryEngine,
    file_id: FileId,
    meta: &FileMeta,
    report: &mut CompileReport,
) {
    let Ok((parsed, _)) = engine.parsed(file_id) else {
        return;
    };
    let root = parsed.syntax_node();

    if let Err(error) = engine.stabilized(file_id) {
        push_query_error(report, meta, "stabilizing", error);
        return;
    }

    match engine.indexed(file_id) {
        Ok(indexed) => {
            for error in &indexed.errors {
                report.diagnostics.push(debug_diagnostic(meta, "indexing", "IndexingError", error));
            }
        }
        Err(error) => {
            push_query_error(report, meta, "indexing", error);
            return;
        }
    }

    match engine.resolved(file_id) {
        Ok(resolved) => {
            with_diagnostics_context(engine, file_id, &root, meta, |ctx| {
                for error in &resolved.errors {
                    report
                        .diagnostics
                        .extend(convert_diagnostics(meta, error.to_diagnostics(&ctx)));
                }
            });
        }
        Err(error) => {
            push_query_error(report, meta, "resolving", error);
        }
    }

    match engine.lowered(file_id) {
        Ok(lowered) => {
            with_diagnostics_context(engine, file_id, &root, meta, |ctx| {
                for error in &lowered.errors {
                    report
                        .diagnostics
                        .extend(convert_diagnostics(meta, error.to_diagnostics(&ctx)));
                }
            });
        }
        Err(error) => push_query_error(report, meta, "lowering", error),
    }

    match engine.grouped(file_id) {
        Ok(grouped) => {
            with_diagnostics_context(engine, file_id, &root, meta, |ctx| {
                for error in &grouped.cycle_errors {
                    report
                        .diagnostics
                        .extend(convert_diagnostics(meta, error.to_diagnostics(&ctx)));
                }
            });
        }
        Err(error) => push_query_error(report, meta, "grouping", error),
    }

    if let Err(error) = engine.bracketed(file_id) {
        push_query_error(report, meta, "bracketing", error);
    }

    if let Err(error) = engine.sectioned(file_id) {
        push_query_error(report, meta, "sectioning", error);
    }

    match engine.checked(file_id) {
        Ok(checked) => {
            with_diagnostics_context(engine, file_id, &root, meta, |ctx| {
                let lookup = |id| engine.lookup_smol_str(id);
                let ctx = ctx.with_checking_lookup(&lookup);
                for error in &checked.errors {
                    report
                        .diagnostics
                        .extend(convert_diagnostics(meta, error.to_diagnostics(&ctx)));
                }
            });
        }
        Err(error) => push_query_error(report, meta, "checking", error),
    }
}

fn with_diagnostics_context(
    engine: &QueryEngine,
    file_id: FileId,
    root: &syntax::SyntaxNode,
    meta: &FileMeta,
    f: impl FnOnce(DiagnosticsContext<'_>),
) {
    let Ok(stabilized) = engine.stabilized(file_id) else {
        return;
    };
    let Ok(indexed) = engine.indexed(file_id) else {
        return;
    };
    let Ok(lowered) = engine.lowered(file_id) else {
        return;
    };
    f(DiagnosticsContext::new(&meta.content, root, &stabilized, &indexed, &lowered));
}

fn parse_diagnostics(meta: &FileMeta, errors: &[parsing::ParseError]) -> Vec<CompilerDiagnostic> {
    errors
        .iter()
        .map(|error| {
            let start = error.offset as u32;
            let end = start.saturating_add(1);
            let diagnostic = Diagnostic::error(
                "ParseError",
                error.message.to_string(),
                Span::new(start, end),
                "parse",
            );
            compiler_diagnostic(meta, "parse", diagnostic)
        })
        .collect()
}

fn debug_diagnostic(
    meta: &FileMeta,
    stage: &'static str,
    code: &'static str,
    error: &impl std::fmt::Debug,
) -> CompilerDiagnostic {
    let diagnostic = Diagnostic::error(
        code,
        format!("{error:?}"),
        Span::new(0, meta.content.len() as u32),
        stage,
    );
    compiler_diagnostic(meta, stage, diagnostic)
}

fn convert_diagnostics(meta: &FileMeta, diagnostics: Vec<Diagnostic>) -> Vec<CompilerDiagnostic> {
    diagnostics
        .into_iter()
        .map(|diagnostic| compiler_diagnostic(meta, diagnostic.source, diagnostic))
        .collect()
}

fn compiler_diagnostic(meta: &FileMeta, stage: &str, diagnostic: Diagnostic) -> CompilerDiagnostic {
    let severity = match diagnostic.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
    };
    let human =
        format_rustc_with_path(std::slice::from_ref(&diagnostic), &meta.content, &meta.path);

    CompilerDiagnostic {
        package: meta.package.clone(),
        version: meta.version.clone(),
        file: meta.path.clone(),
        stage: stage.to_string(),
        severity: severity.to_string(),
        code: diagnostic.code.to_string(),
        message: diagnostic.message,
        span: SpanReport { start: diagnostic.primary.start, end: diagnostic.primary.end },
        human,
    }
}

fn push_query_error(report: &mut CompileReport, meta: &FileMeta, stage: &str, error: QueryError) {
    report.verifier_errors.push(VerifierIssue::query_error(
        &meta.package,
        &meta.path,
        stage,
        format!("{error:?}"),
    ));
}

#[cfg(test)]
mod tests {
    use std::fs;

    use tempfile::tempdir;

    use crate::sources::SourceFile;

    use super::compile_sources;

    #[test]
    fn smoke_compiles_two_modules() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("src")).unwrap();
        let lib = dir.path().join("src/Lib.purs");
        let main = dir.path().join("src/Main.purs");
        fs::write(&lib, "module Lib where\n\nx :: Int\nx = 1\n").unwrap();
        fs::write(&main, "module Main where\n\nimport Lib\n\ny :: Int\ny = x\n").unwrap();

        let sources = [source("fixture", "1.0.0", &lib), source("fixture", "1.0.0", &main)];
        let report = compile_sources(&sources).unwrap();

        assert!(report.verifier_errors.is_empty(), "{:#?}", report.verifier_errors);
        assert!(report.diagnostics.is_empty(), "{:#?}", report.diagnostics);
    }

    #[test]
    fn duplicate_module_reports_verifier_issue() {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("src")).unwrap();
        let a = dir.path().join("src/A.purs");
        let b = dir.path().join("src/B.purs");
        fs::write(&a, "module Main where\n").unwrap();
        fs::write(&b, "module Main where\n").unwrap();

        let report =
            compile_sources(&[source("fixture", "1.0.0", &a), source("fixture", "1.0.0", &b)])
                .unwrap();

        assert!(report.verifier_errors.iter().any(|issue| issue.kind == "DuplicateModule"));
    }

    fn source(package: &str, version: &str, path: &std::path::Path) -> SourceFile {
        SourceFile {
            package: package.to_string(),
            version: version.to_string(),
            path: path.to_path_buf(),
            relative_path: path.file_name().unwrap().into(),
        }
    }
}
