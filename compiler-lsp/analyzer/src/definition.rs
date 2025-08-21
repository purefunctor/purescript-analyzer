use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
use indexing::ImportItemId;
use lowering::{
    BinderId, BinderKind, DeferredResolutionId, Domain, ExpressionId, ExpressionKind,
    FullLoweredModule, TermResolution, TypeId, TypeVariableResolution,
};
use resolving::FullResolvedModule;
use rowan::ast::{AstNode, AstPtr};
use smol_str::{SmolStr, SmolStrBuilder};
use syntax::cst;

use crate::locate;

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let f_id = {
        let uri = uri.as_str();
        files.id(uri)?
    };

    let thing = locate::locate(engine, f_id, position);
    match thing {
        locate::Located::ModuleName(cst) => definition_module_name(engine, files, f_id, cst),
        locate::Located::ImportItem(i_id) => definition_import(engine, files, f_id, i_id),
        locate::Located::Binder(b_id) => definition_binder(engine, files, f_id, b_id),
        locate::Located::Expression(e_id) => definition_expression(engine, files, uri, f_id, e_id),
        locate::Located::Type(t_id) => definition_type(engine, files, uri, f_id, t_id),
        locate::Located::OperatorInChain(domain, text) => {
            definition_nominal(engine, files, f_id, domain, text)
        }
        locate::Located::Nothing => None,
    }
}

fn definition_module_name(
    engine: &QueryEngine,
    files: &Files,
    f_id: FileId,
    cst: AstPtr<cst::ModuleName>,
) -> Option<GotoDefinitionResponse> {
    let (parsed, _) = engine.parsed(f_id).ok()?;

    let root = parsed.syntax_node();
    let module = cst.try_to_node(&root)?;
    let module = {
        let mut buffer = SmolStrBuilder::default();

        if let Some(token) = module.qualifier().and_then(|cst| cst.text()) {
            buffer.push_str(token.text());
        }

        let token = module.name_token()?;
        buffer.push_str(token.text());

        buffer.finish()
    };

    let (uri, range) = {
        let id = engine.module_file(&module)?;
        let path = files.path(id);
        let content = engine.content(id);

        let (parsed, _) = engine.parsed(id).ok()?;
        let root = parsed.syntax_node();

        let range = root.text_range();
        let start = locate::offset_to_position(&content, range.start());
        let end = locate::offset_to_position(&content, range.end());

        let uri = Url::parse(&path).ok()?;
        let uri = prim::handle_generated(uri, &content)?;

        let range = Range { start, end };

        (uri, range)
    };

    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
}

fn definition_import(
    engine: &QueryEngine,
    files: &Files,
    f_id: FileId,
    i_id: ImportItemId,
) -> Option<GotoDefinitionResponse> {
    let (parsed, indexed) = {
        let (parsed, _) = engine.parsed(f_id).ok()?;
        let indexed = engine.indexed(f_id).ok()?;
        (parsed, indexed)
    };

    let root = parsed.syntax_node();
    let ptr = &indexed.source[i_id];
    let node = ptr.try_to_node(&root)?;

    let statement = node.syntax().ancestors().find_map(cst::ImportStatement::cast)?;
    let module = statement.module_name()?;

    let module = {
        let mut buffer = SmolStrBuilder::default();

        if let Some(token) = module.qualifier().and_then(|cst| cst.text()) {
            buffer.push_str(token.text());
        }

        let token = module.name_token()?;
        buffer.push_str(token.text());

        buffer.finish()
    };

    let import_resolved = {
        let import_id = engine.module_file(&module)?;
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

        let (content, parsed, indexed) = {
            let content = engine.content(f_id);
            let (parsed, _) = engine.parsed(f_id).ok()?;
            let indexed = engine.indexed(f_id).ok()?;
            (content, parsed, indexed)
        };

        let root = parsed.syntax_node();
        let ptrs = indexed.term_item_ptr(t_id);
        let range = ptrs
            .into_iter()
            .filter_map(|ptr| locate::range_without_annotation(&content, &ptr, &root))
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

        let (content, parsed, indexed) = {
            let content = engine.content(f_id);
            let (parsed, _) = engine.parsed(f_id).ok()?;
            let indexed = engine.indexed(f_id).ok()?;
            (content, parsed, indexed)
        };

        let root = parsed.syntax_node();
        let ptrs = indexed.type_item_ptr(t_id);
        let range = ptrs
            .into_iter()
            .filter_map(|ptr| locate::range_without_annotation(&content, &ptr, &root))
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
    f_id: FileId,
    b_id: BinderId,
) -> Option<GotoDefinitionResponse> {
    let (resolved, lowered) = {
        let resolved = engine.resolved(f_id).ok()?;
        let lowered = engine.lowered(f_id).ok()?;
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_binder_kind(b_id)?;
    match kind {
        BinderKind::Constructor { resolution, .. } => {
            definition_deferred(engine, files, &resolved, &lowered, *resolution)
        }
        _ => None,
    }
}

fn definition_expression(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    f_id: FileId,
    e_id: ExpressionId,
) -> Option<GotoDefinitionResponse> {
    let (content, parsed, resolved, lowered) = {
        let content = engine.content(f_id);
        let (parsed, _) = engine.parsed(f_id).ok()?;
        let resolved = engine.resolved(f_id).ok()?;
        let lowered = engine.lowered(f_id).ok()?;
        (content, parsed, resolved, lowered)
    };

    let kind = lowered.intermediate.index_expression_kind(e_id)?;
    match kind {
        ExpressionKind::Constructor { resolution, .. } => {
            definition_deferred(engine, files, &resolved, &lowered, *resolution)
        }
        ExpressionKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermResolution::Deferred(id) => {
                    definition_deferred(engine, files, &resolved, &lowered, *id)
                }
                TermResolution::Binder(binder) => {
                    let root = parsed.syntax_node();
                    let ptr = &lowered.source[*binder].syntax_node_ptr();
                    let range = locate::range_without_annotation(&content, ptr, &root)?;
                    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
                }
                TermResolution::Let(binding) => {
                    let root = parsed.syntax_node();

                    let signature = binding
                        .signature
                        .and_then(|id| {
                            let ptr = lowered.source[id].syntax_node_ptr();
                            locate::range_without_annotation(&content, &ptr, &root)
                        })
                        .into_iter();

                    let equations = binding.equations.iter().filter_map(|&id| {
                        let ptr = lowered.source[id].syntax_node_ptr();
                        locate::range_without_annotation(&content, &ptr, &root)
                    });

                    let range = signature
                        .chain(equations)
                        .reduce(|start, end| Range { start: start.start, end: end.end })?;

                    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
                }
            }
        }
        ExpressionKind::OperatorName { resolution, .. } => {
            definition_deferred(engine, files, &resolved, &lowered, *resolution)
        }
        _ => None,
    }
}

fn definition_type(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    f_id: FileId,
    t_id: TypeId,
) -> Option<GotoDefinitionResponse> {
    let (content, parsed, resolved, lowered) = {
        let content = engine.content(f_id);
        let (parsed, _) = engine.parsed(f_id).ok()?;
        let resolved = engine.resolved(f_id).ok()?;
        let lowered = engine.lowered(f_id).ok()?;
        (content, parsed, resolved, lowered)
    };

    let kind = lowered.intermediate.index_type_kind(t_id)?;
    match kind {
        lowering::TypeKind::Constructor { resolution, .. } => {
            definition_deferred(engine, files, &resolved, &lowered, *resolution)
        }
        lowering::TypeKind::Operator { resolution, .. } => {
            definition_deferred(engine, files, &resolved, &lowered, *resolution)
        }
        lowering::TypeKind::Variable { resolution, .. } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TypeVariableResolution::Forall(binding) => {
                    let root = parsed.syntax_node();
                    let ptr = &lowered.source[*binding].syntax_node_ptr();
                    let range = locate::range_without_annotation(&content, ptr, &root)?;
                    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
                }
                TypeVariableResolution::Implicit { .. } => None,
            }
        }
        _ => None,
    }
}

fn definition_deferred(
    engine: &QueryEngine,
    files: &Files,
    resolved: &FullResolvedModule,
    lowered: &FullLoweredModule,
    id: DeferredResolutionId,
) -> Option<GotoDefinitionResponse> {
    let prim = {
        let id = engine.prim_id();
        engine.resolved(id).ok()?
    };

    let deferred = &lowered.graph[id];
    let prefix = deferred.qualifier.as_deref();
    let name = deferred.name.as_deref()?;

    match deferred.domain {
        Domain::Term => {
            let (f_id, t_id) = resolved.lookup_term(&prim, prefix, name)?;

            let uri = {
                let path = files.path(f_id);
                let content = files.content(f_id);
                let uri = Url::parse(&path).ok()?;
                prim::handle_generated(uri, &content)?
            };

            let (content, parsed, indexed) = {
                let content = engine.content(f_id);
                let (parsed, _) = engine.parsed(f_id).ok()?;
                let indexed = engine.indexed(f_id).ok()?;
                (content, parsed, indexed)
            };

            // TODO: Once we implement textDocument/typeDefinition, we
            // should probably also add a term_item_type_ptr function.
            let root = parsed.syntax_node();
            let ptrs = indexed.term_item_ptr(t_id);
            let range = ptrs
                .into_iter()
                .filter_map(|ptr| locate::range_without_annotation(&content, &ptr, &root))
                .reduce(|start, end| Range { start: start.start, end: end.end })?;

            Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
        }
        Domain::Type => {
            let (f_id, t_id) = resolved.lookup_type(&prim, prefix, name)?;

            let uri = {
                let path = files.path(f_id);
                let content = files.content(f_id);
                let uri = Url::parse(&path).ok()?;
                prim::handle_generated(uri, &content)?
            };

            let (content, parsed, indexed) = {
                let content = engine.content(f_id);
                let (parsed, _) = engine.parsed(f_id).ok()?;
                let indexed = engine.indexed(f_id).ok()?;
                (content, parsed, indexed)
            };

            let root = parsed.syntax_node();
            let ptrs = indexed.type_item_ptr(t_id);
            let range = ptrs
                .into_iter()
                .filter_map(|ptr| locate::range_without_annotation(&content, &ptr, &root))
                .reduce(|start, end| Range { start: start.start, end: end.end })?;

            Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
        }
    }
}

/// Convenience function that converts name-only references into
/// [`DeferredResolutionId`] to be used with [`definition_deferred`].
///
/// This is particularly useful for things like operators which
/// we don't currently track during [`lowering`].
fn definition_nominal(
    engine: &QueryEngine,
    files: &Files,
    f_id: FileId,
    domain: Domain,
    text: SmolStr,
) -> Option<GotoDefinitionResponse> {
    let resolved = engine.resolved(f_id).ok()?;
    let lowered = engine.lowered(f_id).ok()?;

    let id = lowered.graph.deferred().find_map(|(id, deferred)| {
        if deferred.domain == domain && deferred.name.as_ref() == Some(&text) {
            Some(id)
        } else {
            None
        }
    })?;

    definition_deferred(engine, files, &resolved, &lowered, id)
}
