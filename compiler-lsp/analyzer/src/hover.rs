use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
use indexing::{ImportItemId, TermItemId, TypeItemId};
use itertools::Itertools;
use lowering::{
    BinderId, BinderKind, ExpressionId, ExpressionKind, TermVariableResolution, TypeId, TypeKind,
};
use rowan::TextRange;
use rowan::ast::{AstNode, AstPtr};
use smol_str::ToSmolStr;
use stabilizing::StabilizedModule;
use syntax::{SyntaxNode, cst};

use crate::extract::{self, AnnotationSyntaxRange};
use crate::{AnalyzerError, locate};

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Result<Option<Hover>, AnalyzerError> {
    let current_file = {
        let uri = uri.as_str();
        files.id(uri).ok_or(AnalyzerError::NonFatal)?
    };

    let located = locate::locate(engine, current_file, position)?;

    match located {
        locate::Located::ModuleName(module_name) => {
            hover_module_name(engine, current_file, module_name)
        }
        locate::Located::ImportItem(import_id) => hover_import(engine, current_file, import_id),
        locate::Located::Binder(binder_id) => hover_binder(engine, current_file, binder_id),
        locate::Located::Expression(expression_id) => {
            hover_expression(engine, current_file, expression_id)
        }
        locate::Located::Type(type_id) => hover_type(engine, current_file, type_id),
        locate::Located::TermOperator(operator_id) => {
            let lowered = engine.lowered(current_file)?;
            let (f_id, t_id) =
                lowered.info.get_term_operator(operator_id).ok_or(AnalyzerError::NonFatal)?;
            hover_file_term(engine, f_id, t_id)
        }
        locate::Located::TypeOperator(operator_id) => {
            let lowered = engine.lowered(current_file)?;
            let (f_id, t_id) =
                lowered.info.get_type_operator(operator_id).ok_or(AnalyzerError::NonFatal)?;
            hover_file_type(engine, f_id, t_id)
        }
        locate::Located::TermItem(term_id) => hover_file_term(engine, current_file, term_id),
        locate::Located::TypeItem(type_id) => hover_file_type(engine, current_file, type_id),
        locate::Located::Nothing => Ok(None),
    }
}

fn hover_module_name(
    engine: &QueryEngine,
    current_file: FileId,
    module_name: AstPtr<cst::ModuleName>,
) -> Result<Option<Hover>, AnalyzerError> {
    let (parsed, _) = engine.parsed(current_file)?;

    let root = parsed.syntax_node();
    let module_name = module_name.try_to_node(&root).ok_or(AnalyzerError::NonFatal)?;

    let module_name = module_name.syntax().text().to_smolstr();
    let module_id = engine.module_file(&module_name).ok_or(AnalyzerError::NonFatal)?;

    let (root, range) = AnnotationSyntaxRange::of_file(engine, module_id)?;

    let annotation = range.annotation.and_then(|range| render_annotation(&root, range));
    let syntax = range.syntax.and_then(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().collect_vec();
    let contents = HoverContents::Array(array);
    let range = None;

    Ok(Some(Hover { contents, range }))
}

fn hover_import(
    engine: &QueryEngine,
    current_file: FileId,
    import_id: ImportItemId,
) -> Result<Option<Hover>, AnalyzerError> {
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
    let module_name =
        statement.module_name().ok_or(AnalyzerError::NonFatal)?.syntax().text().to_smolstr();

    let import_id = engine.module_file(&module_name).ok_or(AnalyzerError::NonFatal)?;
    let import_resolved = engine.resolved(import_id)?;

    let hover_term_import = |engine: &QueryEngine, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) =
            import_resolved.exports.lookup_term(name).ok_or(AnalyzerError::NonFatal)?;
        hover_file_term(engine, f_id, t_id)
    };

    let hover_type_import = |engine: &QueryEngine, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) =
            import_resolved.exports.lookup_type(name).ok_or(AnalyzerError::NonFatal)?;
        hover_file_type(engine, f_id, t_id)
    };

    match node {
        cst::ImportItem::ImportValue(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            hover_term_import(engine, name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            hover_type_import(engine, name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            hover_type_import(engine, name)
        }
        cst::ImportItem::ImportOperator(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            hover_term_import(engine, name)
        }
        cst::ImportItem::ImportTypeOperator(cst) => {
            let token = cst.name_token().ok_or(AnalyzerError::NonFatal)?;
            let name = token.text();
            hover_type_import(engine, name)
        }
    }
}

fn hover_binder(
    engine: &QueryEngine,
    current_file: FileId,
    binder_id: BinderId,
) -> Result<Option<Hover>, AnalyzerError> {
    let lowered = engine.lowered(current_file)?;
    let kind = lowered.info.get_binder_kind(binder_id).ok_or(AnalyzerError::NonFatal)?;
    match kind {
        BinderKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            hover_file_term(engine, *f_id, *t_id)
        }
        _ => Ok(None),
    }
}

fn hover_expression(
    engine: &QueryEngine,
    current_file: FileId,
    expression_id: ExpressionId,
) -> Result<Option<Hover>, AnalyzerError> {
    let lowered = engine.lowered(current_file)?;
    let stabilized = engine.stabilized(current_file)?;

    let kind = lowered.info.get_expression_kind(expression_id).ok_or(AnalyzerError::NonFatal)?;

    match kind {
        ExpressionKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            hover_file_term(engine, *f_id, *t_id)
        }
        ExpressionKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            match resolution {
                TermVariableResolution::Binder(_) => Ok(None),
                TermVariableResolution::Let(let_binding_id) => {
                    let (parsed, _) = engine.parsed(current_file)?;
                    let root = parsed.syntax_node();
                    let let_binding = lowered.info.get_let_binding_group(*let_binding_id);
                    hover_let(&root, &stabilized, let_binding)
                }
                TermVariableResolution::RecordPun(_) => Ok(None),
                TermVariableResolution::Reference(f_id, t_id) => {
                    hover_file_term(engine, *f_id, *t_id)
                }
            }
        }
        ExpressionKind::OperatorName { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            hover_file_term(engine, *f_id, *t_id)
        }
        _ => Ok(None),
    }
}

fn hover_let(
    root: &SyntaxNode,
    stabilized: &StabilizedModule,
    let_binding: &lowering::LetBindingNameGroup,
) -> Result<Option<Hover>, AnalyzerError> {
    let signature = let_binding.signature.and_then(|id| {
        let ptr = stabilized.syntax_ptr(id)?;
        Some(AnnotationSyntaxRange::from_ptr(root, &ptr))
    });

    if let Some(range) = signature {
        let annotation = range.annotation.and_then(|range| render_annotation(root, range));
        let syntax = range.syntax.and_then(|range| render_syntax(root, range));

        let array = [syntax, annotation].into_iter().flatten();
        let separator = MarkedString::String("---".to_string());
        let array = Itertools::intersperse(array, separator).collect();

        let contents = HoverContents::Array(array);
        let range = None;

        Ok(Some(Hover { contents, range }))
    } else {
        let id = let_binding.equations.first().copied().ok_or(AnalyzerError::NonFatal)?;

        let ptr = stabilized.ast_ptr(id).ok_or(AnalyzerError::NonFatal)?;
        let node = ptr.try_to_node(root).ok_or(AnalyzerError::NonFatal)?;

        let token = node.name_token().ok_or(AnalyzerError::NonFatal)?;
        let text = token.text();

        let array = vec![
            MarkedString::LanguageString(LanguageString {
                language: "purescript".to_string(),
                value: format!("{text} :: _"),
            }),
            MarkedString::String("---".to_string()),
            MarkedString::String(format!("`{text}` is a `let`-bound name.")),
            MarkedString::String("---".to_string()),
            MarkedString::String("note: type information is currently not available".to_string()),
        ];

        let contents = HoverContents::Array(array);
        let range = None;

        Ok(Some(Hover { contents, range }))
    }
}

fn hover_type(
    engine: &QueryEngine,
    current_file: FileId,
    type_id: TypeId,
) -> Result<Option<Hover>, AnalyzerError> {
    let lowered = engine.lowered(current_file)?;
    let kind = lowered.info.get_type_kind(type_id).ok_or(AnalyzerError::NonFatal)?;
    match kind {
        TypeKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref().ok_or(AnalyzerError::NonFatal)?;
            hover_file_type(engine, *f_id, *t_id)
        }
        _ => Ok(None),
    }
}

fn hover_file_term(
    engine: &QueryEngine,
    file_id: FileId,
    term_id: TermItemId,
) -> Result<Option<Hover>, AnalyzerError> {
    let (root, range) = AnnotationSyntaxRange::of_file_term(engine, file_id, term_id)?;

    let annotation = range.annotation.and_then(|range| render_annotation(&root, range));
    let syntax = range.syntax.and_then(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten();
    let separator = MarkedString::String("---".to_string());
    let array = Itertools::intersperse(array, separator).collect();

    let contents = HoverContents::Array(array);
    let range = None;

    Ok(Some(Hover { contents, range }))
}

fn hover_file_type(
    engine: &QueryEngine,
    file_id: FileId,
    type_id: TypeItemId,
) -> Result<Option<Hover>, AnalyzerError> {
    let (root, range) = AnnotationSyntaxRange::of_file_type(engine, file_id, type_id)?;

    let annotation = range.annotation.and_then(|range| render_annotation(&root, range));
    let syntax = range.syntax.and_then(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten();
    let separator = MarkedString::String("---".to_string());
    let array = Itertools::intersperse(array, separator).collect();

    let contents = HoverContents::Array(array);
    let range = None;

    Ok(Some(Hover { contents, range }))
}

fn render_annotation(root: &SyntaxNode, range: TextRange) -> Option<MarkedString> {
    let cleaned = extract::extract_annotation(root, range);
    if cleaned.is_empty() { None } else { Some(MarkedString::String(cleaned)) }
}

fn render_syntax(root: &SyntaxNode, range: TextRange) -> Option<MarkedString> {
    let value = extract::extract_syntax(root, range);
    let string = LanguageString { language: "purescript".to_string(), value };
    Some(MarkedString::LanguageString(string))
}
