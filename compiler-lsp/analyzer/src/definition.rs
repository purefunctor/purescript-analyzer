use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
use indexing::{ImportItemId, TermItemId, TypeItemId};
use lowering::{
    BinderId, BinderKind, ExpressionId, ExpressionKind, ImplicitTypeVariable,
    TermVariableResolution, TypeId, TypeKind, TypeVariableResolution,
};
use rowan::ast::{AstNode, AstPtr};
use smol_str::ToSmolStr;
use syntax::cst;

use crate::{AnalyzerError, common, locate};

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Result<Option<GotoDefinitionResponse>, AnalyzerError> {
    let current_file = {
        let uri = uri.as_str();
        files.id(uri).ok_or(AnalyzerError::NonFatal)?
    };

    let located = locate::locate(engine, current_file, position)?;

    match located {
        locate::Located::ModuleName(module_name) => {
            definition_module_name(engine, files, current_file, module_name)
        }
        locate::Located::ImportItem(import_id) => {
            definition_import(engine, files, current_file, import_id)
        }
        locate::Located::Binder(binder_id) => {
            definition_binder(engine, files, current_file, binder_id)
        }
        locate::Located::Expression(expression_id) => {
            definition_expression(engine, files, uri, current_file, expression_id)
        }
        locate::Located::Type(type_id) => {
            definition_type(engine, files, uri, current_file, type_id)
        }
        locate::Located::TermOperator(operator_id) => {
            let lowered = engine.lowered(current_file)?;
            let (f_id, t_id) =
                lowered.info.get_term_operator(operator_id).ok_or(AnalyzerError::NonFatal)?;
            definition_file_term(engine, files, f_id, t_id)
        }
        locate::Located::TypeOperator(operator_id) => {
            let lowered = engine.lowered(current_file)?;
            let (f_id, t_id) =
                lowered.info.get_type_operator(operator_id).ok_or(AnalyzerError::NonFatal)?;
            definition_file_type(engine, files, f_id, t_id)
        }
        locate::Located::TermItem(term_id) => {
            definition_file_term(engine, files, current_file, term_id)
        }
        locate::Located::TypeItem(type_id) => {
            definition_file_type(engine, files, current_file, type_id)
        }
        locate::Located::Nothing => Ok(None),
    }
}

fn definition_module_name(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    module_name: AstPtr<cst::ModuleName>,
) -> Result<Option<GotoDefinitionResponse>, AnalyzerError> {
    let (parsed, _) = engine.parsed(current_file)?;

    let root = parsed.syntax_node();
    let module_name = module_name.try_to_node(&root).ok_or(AnalyzerError::NonFatal)?;

    let module_name = module_name.syntax().text().to_smolstr();
    let module_id = engine.module_file(&module_name).ok_or(AnalyzerError::NonFatal)?;

    let content = engine.content(module_id);

    let (parsed, _) = engine.parsed(module_id)?;
    let root = parsed.syntax_node();

    let range = root.text_range();

    let range_start =
        locate::offset_to_position(&content, range.start()).ok_or(AnalyzerError::NonFatal)?;
    let range_end =
        locate::offset_to_position(&content, range.end()).ok_or(AnalyzerError::NonFatal)?;

    let uri = common::file_uri(engine, files, module_id)?;
    let range = Range { start: range_start, end: range_end };

    Ok(Some(GotoDefinitionResponse::Scalar(Location { uri, range })))
}

fn definition_import(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    import_id: ImportItemId,
) -> Result<Option<GotoDefinitionResponse>, AnalyzerError> {
    let (parsed, _) = engine.parsed(current_file)?;
    let stabilized = engine.stabilized(current_file)?;

    let root = parsed.syntax_node();
    let ptr = stabilized.ast_ptr(import_id).ok_or(AnalyzerError::NonFatal)?;
    let node = ptr.try_to_node(&root).ok_or(AnalyzerError::NonFatal)?;

    let statement = node
        .syntax()
        .ancestors()
        .find_map(cst::ImportStatement::cast)
        .ok_or(AnalyzerError::NonFatal)?;
    let module_name = statement.module_name().ok_or(AnalyzerError::NonFatal)?.syntax().to_smolstr();

    let import_resolved = {
        let import_id = engine.module_file(&module_name).ok_or(AnalyzerError::NonFatal)?;
        engine.resolved(import_id)?
    };

    let goto_term = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) =
            import_resolved.exports.lookup_term(name).ok_or(AnalyzerError::NonFatal)?;
        definition_file_term(engine, files, f_id, t_id)
    };

    let goto_type = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) =
            import_resolved.exports.lookup_type(name).ok_or(AnalyzerError::NonFatal)?;
        definition_file_type(engine, files, f_id, t_id)
    };

    match node {
        cst::ImportItem::ImportValue(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            goto_term(engine, files, name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            goto_type(engine, files, name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            goto_type(engine, files, name)
        }
        cst::ImportItem::ImportOperator(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            goto_term(engine, files, name)
        }
        cst::ImportItem::ImportTypeOperator(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            goto_type(engine, files, name)
        }
    }
}

fn definition_binder(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    binder_id: BinderId,
) -> Result<Option<GotoDefinitionResponse>, AnalyzerError> {
    let lowered = engine.lowered(current_file)?;
    let kind = lowered.info.get_binder_kind(binder_id).ok_or(AnalyzerError::NonFatal)?;
    match kind {
        BinderKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            definition_file_term(engine, files, *f_id, *t_id)
        }
        _ => Ok(None),
    }
}

fn definition_expression(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    current_file: FileId,
    expression_id: ExpressionId,
) -> Result<Option<GotoDefinitionResponse>, AnalyzerError> {
    let content = engine.content(current_file);
    let (parsed, _) = engine.parsed(current_file)?;

    let stabilized = engine.stabilized(current_file)?;
    let lowered = engine.lowered(current_file)?;

    let kind = lowered.info.get_expression_kind(expression_id).ok_or(AnalyzerError::NonFatal)?;

    match kind {
        ExpressionKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            definition_file_term(engine, files, *f_id, *t_id)
        }
        ExpressionKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            match resolution {
                TermVariableResolution::Binder(id) => {
                    let root = parsed.syntax_node();
                    let ptr = stabilized.syntax_ptr(*id).ok_or(AnalyzerError::NonFatal)?;
                    let range = locate::syntax_range(&content, &root, &ptr)
                        .ok_or(AnalyzerError::NonFatal)?;
                    Ok(Some(GotoDefinitionResponse::Scalar(Location { uri, range })))
                }
                TermVariableResolution::Let(binding) => {
                    let root = parsed.syntax_node();

                    let signature = binding
                        .signature
                        .and_then(|id| {
                            let ptr = stabilized.syntax_ptr(id)?;
                            locate::syntax_range(&content, &root, &ptr)
                        })
                        .into_iter();

                    let equations = binding.equations.iter().filter_map(|&id| {
                        let ptr = stabilized.syntax_ptr(id)?;
                        locate::syntax_range(&content, &root, &ptr)
                    });

                    let range = signature
                        .chain(equations)
                        .reduce(|start, end| Range { start: start.start, end: end.end })
                        .ok_or(AnalyzerError::NonFatal)?;

                    Ok(Some(GotoDefinitionResponse::Scalar(Location { uri, range })))
                }
                TermVariableResolution::Reference(f_id, t_id) => {
                    definition_file_term(engine, files, *f_id, *t_id)
                }
            }
        }
        ExpressionKind::OperatorName { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            definition_file_term(engine, files, *f_id, *t_id)
        }
        _ => Ok(None),
    }
}

fn definition_type(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    current_file: FileId,
    type_id: TypeId,
) -> Result<Option<GotoDefinitionResponse>, AnalyzerError> {
    let content = engine.content(current_file);
    let (parsed, _) = engine.parsed(current_file)?;
    let stabilized = engine.stabilized(current_file)?;
    let lowered = engine.lowered(current_file)?;

    let kind = lowered.info.get_type_kind(type_id).ok_or(AnalyzerError::NonFatal)?;
    match kind {
        TypeKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            definition_file_type(engine, files, *f_id, *t_id)
        }
        TypeKind::Operator { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            definition_file_type(engine, files, *f_id, *t_id)
        }
        TypeKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            match resolution {
                TypeVariableResolution::Forall(binding) => {
                    let root = parsed.syntax_node();
                    let ptr = stabilized
                        .ast_ptr(*binding)
                        .ok_or(AnalyzerError::NonFatal)?
                        .syntax_node_ptr();
                    let range = locate::syntax_range(&content, &root, &ptr)
                        .ok_or(AnalyzerError::NonFatal)?;
                    Ok(Some(GotoDefinitionResponse::Scalar(Location { uri, range })))
                }
                TypeVariableResolution::Implicit(ImplicitTypeVariable { .. }) => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

fn definition_file_term(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    term_id: TermItemId,
) -> Result<Option<GotoDefinitionResponse>, AnalyzerError> {
    let uri = common::file_uri(engine, files, file_id)?;
    let location = common::file_term_location(engine, uri, file_id, term_id)?;
    Ok(Some(GotoDefinitionResponse::Scalar(location)))
}

fn definition_file_type(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    type_id: TypeItemId,
) -> Result<Option<GotoDefinitionResponse>, AnalyzerError> {
    let uri = common::file_uri(engine, files, file_id)?;
    let location = common::file_type_location(engine, uri, file_id, type_id)?;
    Ok(Some(GotoDefinitionResponse::Scalar(location)))
}
