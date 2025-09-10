use async_lsp::lsp_types::*;
use building::{QueryEngine, prim};
use files::{FileId, Files};
use indexing::{ImportItemId, TermItemId, TypeItemId};
use lowering::{
    BinderId, BinderKind, ExpressionId, ExpressionKind, ImplicitTypeVariable,
    TermVariableResolution, TypeId, TypeKind, TypeVariableResolution,
};
use rowan::ast::{AstNode, AstPtr};
use smol_str::ToSmolStr;
use syntax::cst;

use crate::locate;

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let current_file = {
        let uri = uri.as_str();
        files.id(uri)?
    };

    let located = locate::locate(engine, current_file, position);

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
            let lowered = engine.lowered(current_file).ok()?;
            let (f_id, t_id) = lowered.intermediate.index_term_operator(operator_id)?;
            definition_file_term(engine, files, *f_id, *t_id)
        }
        locate::Located::TypeOperator(operator_id) => {
            let lowered = engine.lowered(current_file).ok()?;
            let (f_id, t_id) = lowered.intermediate.index_type_operator(operator_id)?;
            definition_file_type(engine, files, *f_id, *t_id)
        }
        locate::Located::Nothing => None,
    }
}

fn definition_module_name(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    module_name: AstPtr<cst::ModuleName>,
) -> Option<GotoDefinitionResponse> {
    let (parsed, _) = engine.parsed(current_file).ok()?;

    let root = parsed.syntax_node();
    let module_name = module_name.try_to_node(&root)?;

    let module_name = module_name.syntax().text().to_smolstr();
    let module_id = engine.module_file(&module_name)?;

    let path = files.path(module_id);
    let content = engine.content(module_id);

    let (parsed, _) = engine.parsed(module_id).ok()?;
    let root = parsed.syntax_node();

    let range = root.text_range();
    let start = locate::offset_to_position(&content, range.start());
    let end = locate::offset_to_position(&content, range.end());

    let uri = Url::parse(&path).ok()?;
    let uri = prim::handle_generated(uri, &content)?;

    let range = Range { start, end };

    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
}

fn definition_import(
    engine: &QueryEngine,
    files: &Files,
    current_file: FileId,
    import_id: ImportItemId,
) -> Option<GotoDefinitionResponse> {
    let (parsed, _) = engine.parsed(current_file).ok()?;
    let indexed = engine.indexed(current_file).ok()?;

    let root = parsed.syntax_node();
    let ptr = &indexed.source[import_id];
    let node = ptr.try_to_node(&root)?;

    let statement = node.syntax().ancestors().find_map(cst::ImportStatement::cast)?;
    let module_name = statement.module_name()?.syntax().to_smolstr();

    let import_resolved = {
        let import_id = engine.module_file(&module_name)?;
        engine.resolved(import_id).ok()?
    };

    let goto_term = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) = import_resolved.exports.lookup_term(name)?;

        let uri = {
            let path = files.path(f_id);
            let content = files.content(f_id);

            let uri = Url::parse(&path).ok()?;
            prim::handle_generated(uri, &content)?
        };

        let content = engine.content(f_id);
        let (parsed, _) = engine.parsed(f_id).ok()?;
        let indexed = engine.indexed(f_id).ok()?;

        let root = parsed.syntax_node();
        let ptrs = indexed.term_item_ptr(t_id);
        let range = ptrs
            .into_iter()
            .filter_map(|ptr| locate::syntax_range(&content, &root, &ptr))
            .reduce(|start, end| Range { start: start.start, end: end.end })?;

        Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
    };

    let goto_type = |engine: &QueryEngine, files: &Files, name: &str| {
        let name = name.trim_start_matches("(").trim_end_matches(")");
        let (f_id, t_id) = import_resolved.exports.lookup_type(name)?;

        let uri = {
            let path = files.path(f_id);
            let content = files.content(f_id);
            let uri = Url::parse(&path).ok()?;
            prim::handle_generated(uri, &content)?
        };

        let content = engine.content(f_id);
        let (parsed, _) = engine.parsed(f_id).ok()?;
        let indexed = engine.indexed(f_id).ok()?;

        let root = parsed.syntax_node();
        let ptrs = indexed.type_item_ptr(t_id);
        let range = ptrs
            .into_iter()
            .filter_map(|ptr| locate::syntax_range(&content, &root, &ptr))
            .reduce(|start, end| Range { start: start.start, end: end.end })?;

        Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
    };

    match node {
        cst::ImportItem::ImportValue(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            goto_term(engine, files, name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            goto_type(engine, files, name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            goto_type(engine, files, name)
        }
        cst::ImportItem::ImportOperator(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            goto_term(engine, files, name)
        }
        cst::ImportItem::ImportTypeOperator(cst) => {
            let token = cst.name_token()?;
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
) -> Option<GotoDefinitionResponse> {
    let lowered = engine.lowered(current_file).ok()?;
    let kind = lowered.intermediate.index_binder_kind(binder_id)?;
    match kind {
        BinderKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            definition_file_term(engine, files, *f_id, *t_id)
        }
        _ => None,
    }
}

fn definition_expression(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    current_file: FileId,
    expression_id: ExpressionId,
) -> Option<GotoDefinitionResponse> {
    let content = engine.content(current_file);
    let (parsed, _) = engine.parsed(current_file).ok()?;
    let lowered = engine.lowered(current_file).ok()?;

    let kind = lowered.intermediate.index_expression_kind(expression_id)?;
    match kind {
        ExpressionKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            definition_file_term(engine, files, *f_id, *t_id)
        }
        ExpressionKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermVariableResolution::Binder(binder) => {
                    let root = parsed.syntax_node();
                    let ptr = lowered.source[*binder].syntax_node_ptr();
                    let range = locate::syntax_range(&content, &root, &ptr)?;
                    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
                }
                TermVariableResolution::Let(binding) => {
                    let root = parsed.syntax_node();

                    let signature = binding
                        .signature
                        .and_then(|id| {
                            let ptr = lowered.source[id].syntax_node_ptr();
                            locate::syntax_range(&content, &root, &ptr)
                        })
                        .into_iter();

                    let equations = binding.equations.iter().filter_map(|&id| {
                        let ptr = lowered.source[id].syntax_node_ptr();
                        locate::syntax_range(&content, &root, &ptr)
                    });

                    let range = signature
                        .chain(equations)
                        .reduce(|start, end| Range { start: start.start, end: end.end })?;

                    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
                }
                TermVariableResolution::Reference(f_id, t_id) => {
                    definition_file_term(engine, files, *f_id, *t_id)
                }
            }
        }
        ExpressionKind::OperatorName { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            definition_file_term(engine, files, *f_id, *t_id)
        }
        _ => None,
    }
}

fn definition_type(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    current_file: FileId,
    type_id: TypeId,
) -> Option<GotoDefinitionResponse> {
    let content = engine.content(current_file);
    let (parsed, _) = engine.parsed(current_file).ok()?;
    let lowered = engine.lowered(current_file).ok()?;

    let kind = lowered.intermediate.index_type_kind(type_id)?;
    match kind {
        TypeKind::Constructor { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            definition_file_type(engine, files, *f_id, *t_id)
        }
        TypeKind::Operator { resolution, .. } => {
            let (f_id, t_id) = resolution.as_ref()?;
            definition_file_type(engine, files, *f_id, *t_id)
        }
        TypeKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TypeVariableResolution::Forall(binding) => {
                    let root = parsed.syntax_node();
                    let ptr = lowered.source[*binding].syntax_node_ptr();
                    let range = locate::syntax_range(&content, &root, &ptr)?;
                    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
                }
                TypeVariableResolution::Implicit(ImplicitTypeVariable { .. }) => None,
            }
        }
        _ => None,
    }
}

fn definition_file_term(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    term_id: TermItemId,
) -> Option<GotoDefinitionResponse> {
    let uri = {
        let path = files.path(file_id);
        let content = files.content(file_id);
        let uri = Url::parse(&path).ok()?;
        prim::handle_generated(uri, &content)?
    };

    let content = engine.content(file_id);
    let (parsed, _) = engine.parsed(file_id).ok()?;
    let indexed = engine.indexed(file_id).ok()?;

    // TODO: Once we implement textDocument/typeDefinition, we
    // should probably also add a term_item_type_ptr function.
    let root = parsed.syntax_node();
    let ptrs = indexed.term_item_ptr(term_id);
    let range = ptrs
        .into_iter()
        .filter_map(|ptr| locate::syntax_range(&content, &root, &ptr))
        .reduce(|start, end| Range { start: start.start, end: end.end })?;

    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
}

fn definition_file_type(
    engine: &QueryEngine,
    files: &Files,
    file_id: FileId,
    type_id: TypeItemId,
) -> Option<GotoDefinitionResponse> {
    let uri = {
        let path = files.path(file_id);
        let content = files.content(file_id);
        let uri = Url::parse(&path).ok()?;
        prim::handle_generated(uri, &content)?
    };

    let content = engine.content(file_id);
    let (parsed, _) = engine.parsed(file_id).ok()?;
    let indexed = engine.indexed(file_id).ok()?;

    let root = parsed.syntax_node();
    let ptrs = indexed.type_item_ptr(type_id);
    let range = ptrs
        .into_iter()
        .filter_map(|ptr| locate::syntax_range(&content, &root, &ptr))
        .reduce(|start, end| Range { start: start.start, end: end.end })?;

    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
}
