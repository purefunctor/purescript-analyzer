use analyzer::{common, locate};
use async_lsp::LanguageClient;
use async_lsp::lsp_types::*;
use files::FileId;
use indexing::{IndexedModule, TypeItemKind};
use lowering::{LoweringError, RecursiveSynonym};
use resolving::{ResolvedModule, ResolvingError};
use rowan::ast::AstNode;
use stabilizing::StabilizedModule;
use syntax::SyntaxNode;

use crate::lsp::error::LspError;
use crate::lsp::{State, StateSnapshot};

struct DiagnosticsContext<'a> {
    uri: &'a Url,
    content: &'a str,
    root: &'a SyntaxNode,
    stabilized: &'a StabilizedModule,
    indexed: &'a IndexedModule,
    resolved: &'a ResolvedModule,
    lowered: &'a lowering::LoweredModule,
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
    let indexed = snapshot.engine.indexed(id)?;
    let resolved = snapshot.engine.resolved(id)?;
    let lowered = snapshot.engine.lowered(id)?;

    let uri = {
        let files = snapshot.files.read();
        common::file_uri(&snapshot.engine, &files, id)?
    };

    let context = DiagnosticsContext {
        uri: &uri,
        content: &content,
        root: &root,
        stabilized: &stabilized,
        indexed: &indexed,
        resolved: &resolved,
        lowered: &lowered,
    };

    let mut diagnostics = vec![];
    diagnostics.extend(resolved_diagnostics(&context));
    diagnostics.extend(lowered_diagnostics(&context));

    snapshot.client.publish_diagnostics(PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    })?;

    Ok(())
}

fn lowered_diagnostics<'a>(
    context: &'a DiagnosticsContext<'a>,
) -> impl Iterator<Item = Diagnostic> + 'a {
    context.lowered.errors.iter().filter_map(|error| lowered_error(context, error))
}

fn lowered_error(context: &DiagnosticsContext<'_>, error: &LoweringError) -> Option<Diagnostic> {
    match error {
        LoweringError::NotInScope(not_in_scope) => {
            let (ptr, name) = match not_in_scope {
                lowering::NotInScope::ExprConstructor { id } => {
                    (context.stabilized.syntax_ptr(*id)?, None)
                }
                lowering::NotInScope::ExprVariable { id } => {
                    (context.stabilized.syntax_ptr(*id)?, None)
                }
                lowering::NotInScope::ExprOperatorName { id } => {
                    (context.stabilized.syntax_ptr(*id)?, None)
                }
                lowering::NotInScope::TypeConstructor { id } => {
                    (context.stabilized.syntax_ptr(*id)?, None)
                }
                lowering::NotInScope::TypeVariable { id } => {
                    (context.stabilized.syntax_ptr(*id)?, None)
                }
                lowering::NotInScope::TypeOperatorName { id } => {
                    (context.stabilized.syntax_ptr(*id)?, None)
                }
                lowering::NotInScope::DoFn { kind, id } => (
                    context.stabilized.syntax_ptr(*id)?,
                    match kind {
                        lowering::DoFn::Bind => Some("bind"),
                        lowering::DoFn::Discard => Some("discard"),
                    },
                ),
                lowering::NotInScope::AdoFn { kind, id } => (
                    context.stabilized.syntax_ptr(*id)?,
                    match kind {
                        lowering::AdoFn::Map => Some("map"),
                        lowering::AdoFn::Apply => Some("apply"),
                    },
                ),
                lowering::NotInScope::TermOperator { id } => {
                    (context.stabilized.syntax_ptr(*id)?, None)
                }
                lowering::NotInScope::TypeOperator { id } => {
                    (context.stabilized.syntax_ptr(*id)?, None)
                }
            };

            let message = if let Some(name) = name {
                format!("'{name}' is not in scope")
            } else {
                let range = ptr.to_node(context.root).text_range();
                let name = context.content[range].trim();
                format!("'{name}' is not in scope")
            };

            let range = locate::syntax_range(context.content, context.root, &ptr)?;

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

        LoweringError::RecursiveSynonym(RecursiveSynonym { group }) => {
            let equations = group.iter().filter_map(|id| {
                if let TypeItemKind::Synonym { equation, .. } = context.indexed.items[*id].kind {
                    equation
                } else {
                    None
                }
            });

            let locations = equations.filter_map(|equation| {
                let syntax_ptr = context.stabilized.syntax_ptr(equation)?;
                locate::syntax_range(context.content, context.root, &syntax_ptr)
            });

            let locations: Vec<_> = locations.collect();
            let [range, associated @ ..] = &locations[..] else { return None };

            let related_information = associated.iter().map(|&range| {
                let uri = context.uri.clone();
                let location = Location { uri, range };
                DiagnosticRelatedInformation {
                    location,
                    message: "Includes this type synonym".to_string(),
                }
            });

            let related_information = related_information.collect();

            Some(Diagnostic {
                range: *range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("RecursiveSynonym".to_string())),
                code_description: None,
                source: Some("analyzer/lowering".to_string()),
                message: "Invalid type synonym cycle".to_string(),
                related_information: Some(related_information),
                tags: None,
                data: None,
            })
        }
    }
}

fn resolved_diagnostics<'a>(
    context: &'a DiagnosticsContext<'a>,
) -> impl Iterator<Item = Diagnostic> + 'a {
    context.resolved.errors.iter().filter_map(|error| resolved_error(context, error))
}

fn resolved_error(context: &DiagnosticsContext<'_>, error: &ResolvingError) -> Option<Diagnostic> {
    let source = Some("analyzer/resolving".to_string());
    match error {
        ResolvingError::TermImportConflict { .. } => None,

        ResolvingError::TypeImportConflict { .. } => None,

        ResolvingError::TermExportConflict { .. } => None,

        ResolvingError::TypeExportConflict { .. } => None,

        ResolvingError::ExistingTerm { .. } => None,

        ResolvingError::ExistingType { .. } => None,

        ResolvingError::InvalidImportStatement { id } => {
            let ptr = context.stabilized.ast_ptr(*id)?;

            let message = {
                let cst = ptr.to_node(context.root);

                let name = cst.module_name().map(|cst| {
                    let range = cst.syntax().text_range();
                    context.content[range].trim()
                });

                let name = name.unwrap_or("<ParseError>");
                format!("Cannot import module '{name}'")
            };

            let ptr = ptr.syntax_node_ptr();
            let range = locate::syntax_range(context.content, context.root, &ptr)?;

            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("InvalidImportStatement".to_string())),
                code_description: None,
                source,
                message,
                related_information: None,
                tags: None,
                data: None,
            })
        }

        ResolvingError::InvalidImportItem { id } => {
            let ptr = context.stabilized.syntax_ptr(*id)?;

            let message = {
                let range = ptr.to_node(context.root).text_range();
                let name = context.content[range].trim();
                format!("Cannot import item '{name}'")
            };

            let range = locate::syntax_range(context.content, context.root, &ptr)?;

            Some(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("InvalidImportItem".to_string())),
                code_description: None,
                source,
                message,
                related_information: None,
                tags: None,
                data: None,
            })
        }
    }
}
