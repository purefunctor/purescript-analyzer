use analyzer::{common, locate};
use async_lsp::LanguageClient;
use async_lsp::lsp_types::*;
use files::FileId;
use syntax::SyntaxNode;

use crate::lsp::error::LspError;
use crate::lsp::{State, StateSnapshot};

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
        let _span = tracing::info_span!("collect-diagnostics-task").entered();
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
    let lowered = snapshot.engine.lowered(id)?;

    let mut diagnostics = vec![];
    diagnostics.extend(lowered_diagnostics(&content, &root, &stabilized, &lowered));

    let uri = {
        let files = snapshot.files.read();
        common::file_uri(&snapshot.engine, &files, id)?
    };

    snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    })?;

    Ok(())
}

fn lowered_diagnostics<'context>(
    content: &'context str,
    root: &'context SyntaxNode,
    stabilized: &'context stabilizing::StabilizedModule,
    lowered: &'context lowering::LoweredModule,
) -> impl Iterator<Item = Diagnostic> + 'context {
    lowered.errors.iter().filter_map(|error| lowered_error(content, root, stabilized, error))
}

fn lowered_error<'context>(
    content: &'context str,
    root: &'context SyntaxNode,
    stabilized: &'context stabilizing::StabilizedModule,
    error: &lowering::LoweringError,
) -> Option<Diagnostic> {
    match error {
        lowering::LoweringError::NotInScope(not_in_scope) => {
            let (ptr, name) = match not_in_scope {
                lowering::NotInScope::ExprConstructor { id } => {
                    (stabilized.ast_ptr(*id)?.syntax_node_ptr(), None)
                }
                lowering::NotInScope::ExprVariable { id } => {
                    (stabilized.ast_ptr(*id)?.syntax_node_ptr(), None)
                }
                lowering::NotInScope::ExprOperatorName { id } => {
                    (stabilized.ast_ptr(*id)?.syntax_node_ptr(), None)
                }
                lowering::NotInScope::TypeConstructor { id } => {
                    (stabilized.ast_ptr(*id)?.syntax_node_ptr(), None)
                }
                lowering::NotInScope::TypeVariable { id } => {
                    (stabilized.ast_ptr(*id)?.syntax_node_ptr(), None)
                }
                lowering::NotInScope::TypeOperatorName { id } => {
                    (stabilized.ast_ptr(*id)?.syntax_node_ptr(), None)
                }
                lowering::NotInScope::DoFn { kind, id } => (
                    stabilized.ast_ptr(*id)?.syntax_node_ptr(),
                    match kind {
                        lowering::DoFn::Bind => Some("bind"),
                        lowering::DoFn::Discard => Some("discard"),
                    },
                ),
                lowering::NotInScope::AdoFn { kind, id } => (
                    stabilized.ast_ptr(*id)?.syntax_node_ptr(),
                    match kind {
                        lowering::AdoFn::Map => Some("map"),
                        lowering::AdoFn::Apply => Some("apply"),
                    },
                ),
                lowering::NotInScope::TermOperator { id } => {
                    (stabilized.ast_ptr(*id)?.syntax_node_ptr(), None)
                }
                lowering::NotInScope::TypeOperator { id } => {
                    (stabilized.ast_ptr(*id)?.syntax_node_ptr(), None)
                }
            };

            let message = if let Some(name) = name {
                format!("'{name}' is not in scope")
            } else {
                let range = ptr.to_node(&root).text_range();
                let name = content[range].trim();
                format!("'{name}' is not in scope")
            };

            let range = locate::syntax_range(&content, &root, &ptr)?;

            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("NotInScope".to_string())),
                code_description: None,
                source: Some("analyzer/lowering".to_string()),
                message: message.to_string(),
                related_information: None,
                tags: None,
                data: None,
            })
        }
    }
}
