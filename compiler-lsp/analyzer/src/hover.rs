use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
use indexing::{ImportItemId, TermItemId, TypeItemId};
use itertools::Itertools;
use lowering::{
    BinderId, BinderKind, ExpressionId, ExpressionKind, FullLoweredModule, LetBound,
    TermVariableResolution, TypeId, TypeKind,
};
use rowan::{
    TextRange,
    ast::{AstNode, AstPtr},
};
use smol_str::ToSmolStr;
use syntax::{SyntaxNode, cst};

use crate::{
    extract::{self, AnnotationSyntaxRange},
    locate,
};

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Option<Hover> {
    let current_file = {
        let uri = uri.as_str();
        files.id(uri)?
    };

    let located = locate::locate(engine, current_file, position).ok()?;

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
            let lowered = engine.lowered(current_file).ok()?;
            let (f_id, t_id) = lowered.intermediate.index_term_operator(operator_id)?;
            hover_file_term(engine, *f_id, *t_id)
        }
        locate::Located::TypeOperator(operator_id) => {
            let lowered = engine.lowered(current_file).ok()?;
            let (f_id, t_id) = lowered.intermediate.index_type_operator(operator_id)?;
            hover_file_type(engine, *f_id, *t_id)
        }
        locate::Located::Nothing => None,
    }
}

fn hover_module_name(
    engine: &QueryEngine,
    current_file: FileId,
    module_name: AstPtr<cst::ModuleName>,
) -> Option<Hover> {
    let (parsed, _) = engine.parsed(current_file).ok()?;

    let root = parsed.syntax_node();
    let module_name = module_name.try_to_node(&root)?;

    let module_name = module_name.syntax().text().to_smolstr();
    let module_id = engine.module_file(&module_name)?;

    let (root, range) = AnnotationSyntaxRange::of_file(engine, module_id)?;

    let annotation = range.annotation.and_then(|range| render_annotation(&root, range));
    let syntax = range.syntax.and_then(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().collect_vec();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

fn hover_import(
    engine: &QueryEngine,
    current_file: FileId,
    import_id: ImportItemId,
) -> Option<Hover> {
    let (parsed, _) = engine.parsed(current_file).ok()?;
    let indexed = engine.indexed(current_file).ok()?;

    let root = parsed.syntax_node();
    let node = indexed.source[import_id].try_to_node(&root)?;

    let statement = node.syntax().ancestors().find_map(cst::ImportStatement::cast)?;
    let module_name = statement.module_name()?.syntax().text().to_smolstr();

    let import_id = engine.module_file(&module_name)?;
    let import_resolved = engine.resolved(import_id).ok()?;

    let hover_term_import = |engine: &QueryEngine, name: &str| {
        let (f_id, t_id) = import_resolved.exports.lookup_term(name)?;
        hover_file_term(engine, f_id, t_id)
    };

    let hover_type_import = |engine: &QueryEngine, name: &str| {
        let (f_id, t_id) = import_resolved.exports.lookup_type(name)?;
        hover_file_type(engine, f_id, t_id)
    };

    match node {
        cst::ImportItem::ImportValue(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_term_import(engine, name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_type_import(engine, name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_type_import(engine, name)
        }
        cst::ImportItem::ImportOperator(_) => None,
        cst::ImportItem::ImportTypeOperator(_) => None,
    }
}

fn hover_binder(engine: &QueryEngine, current_file: FileId, binder_id: BinderId) -> Option<Hover> {
    let lowered = engine.lowered(current_file).ok()?;
    let kind = lowered.intermediate.index_binder_kind(binder_id)?;
    match kind {
        BinderKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            hover_file_term(engine, *f_id, *t_id)
        }
        _ => None,
    }
}

fn hover_expression(
    engine: &QueryEngine,
    current_file: FileId,
    expression_id: ExpressionId,
) -> Option<Hover> {
    let lowered = engine.lowered(current_file).ok()?;
    let kind = lowered.intermediate.index_expression_kind(expression_id)?;
    match kind {
        ExpressionKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            hover_file_term(engine, *f_id, *t_id)
        }
        ExpressionKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermVariableResolution::Binder(_) => None,
                TermVariableResolution::Let(let_binding) => {
                    let (parsed, _) = engine.parsed(current_file).ok()?;
                    let root = parsed.syntax_node();
                    hover_let(&root, &lowered, let_binding)
                }
                TermVariableResolution::Reference(f_id, t_id) => {
                    hover_file_term(engine, *f_id, *t_id)
                }
            }
        }
        ExpressionKind::OperatorName { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            hover_file_term(engine, *f_id, *t_id)
        }
        _ => None,
    }
}

fn hover_let(
    root: &SyntaxNode,
    lowered: &FullLoweredModule,
    let_binding: &LetBound,
) -> Option<Hover> {
    let signature = let_binding.signature.map(|id| {
        let ptr = lowered.source[id].syntax_node_ptr();
        AnnotationSyntaxRange::from_ptr(root, &ptr)
    });

    if let Some(range) = signature {
        let annotation = range.annotation.and_then(|range| render_annotation(root, range));
        let syntax = range.syntax.and_then(|range| render_syntax(root, range));

        let array = [syntax, annotation].into_iter().flatten();
        let separator = MarkedString::String("---".to_string());
        let array = Itertools::intersperse(array, separator).collect();

        let contents = HoverContents::Array(array);
        let range = None;

        Some(Hover { contents, range })
    } else {
        let id = let_binding.equations.first().copied()?;

        let ptr = &lowered.source[id];
        let node = ptr.try_to_node(root)?;

        let token = node.name_token()?;
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

        Some(Hover { contents, range })
    }
}

fn hover_type(engine: &QueryEngine, current_file: FileId, type_id: TypeId) -> Option<Hover> {
    let lowered = engine.lowered(current_file).ok()?;
    let kind = lowered.intermediate.index_type_kind(type_id)?;
    match kind {
        TypeKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            hover_file_type(engine, *f_id, *t_id)
        }
        _ => None,
    }
}

fn hover_file_term(engine: &QueryEngine, file_id: FileId, term_id: TermItemId) -> Option<Hover> {
    let (root, range) = AnnotationSyntaxRange::of_file_term(engine, file_id, term_id)?;

    let annotation = range.annotation.and_then(|range| render_annotation(&root, range));
    let syntax = range.syntax.and_then(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten();
    let separator = MarkedString::String("---".to_string());
    let array = Itertools::intersperse(array, separator).collect();

    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

fn hover_file_type(engine: &QueryEngine, file_id: FileId, type_id: TypeItemId) -> Option<Hover> {
    let (root, range) = AnnotationSyntaxRange::of_file_type(engine, file_id, type_id)?;

    let annotation = range.annotation.and_then(|range| render_annotation(&root, range));
    let syntax = range.syntax.and_then(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten();
    let separator = MarkedString::String("---".to_string());
    let array = Itertools::intersperse(array, separator).collect();

    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
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
