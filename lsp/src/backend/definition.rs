use files::FileId;
use indexing::ImportItemId;
use lowering::{
    BinderId, BinderKind, DeferredResolutionId, ExpressionId, ExpressionKind, FullLoweredModule,
    ResolutionDomain, TermResolution, TypeId, TypeVariableResolution,
};
use resolving::FullResolvedModule;
use rowan::ast::AstNode;
use smol_str::SmolStrBuilder;
use syntax::cst;
use tower_lsp::lsp_types::*;

use super::{Backend, locate};

pub(super) async fn definition(
    backend: &Backend,
    uri: Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let f_id = {
        let files = backend.files.lock().unwrap();
        let uri = uri.as_str();
        files.id(uri)?
    };

    let thing = locate::locate(backend, f_id, position);
    match thing {
        locate::Located::ImportItem(i_id) => definition_import(backend, f_id, i_id).await,
        locate::Located::Binder(b_id) => definition_binder(backend, f_id, b_id).await,
        locate::Located::Expression(e_id) => definition_expression(backend, uri, f_id, e_id).await,
        locate::Located::Type(t_id) => definition_type(backend, uri, f_id, t_id).await,
        locate::Located::Nothing => None,
    }
}

async fn definition_import(
    backend: &Backend,
    f_id: FileId,
    i_id: ImportItemId,
) -> Option<GotoDefinitionResponse> {
    let (parsed, indexed) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let (parsed, _) = runtime.parsed(f_id);
        let indexed = runtime.indexed(f_id);
        (parsed, indexed)
    };

    let root = parsed.syntax_node();
    let ptr = &indexed.source[i_id];
    let node = ptr.to_node(&root);

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

    let import_id = {
        let mut runtime = backend.runtime.lock().unwrap();
        runtime.module_file(&module)?
    };

    let import_resolved = {
        let mut runtime = backend.runtime.lock().unwrap();
        
        runtime.resolved(import_id)
    };

    let goto_term = |name: &str| {
        let (f_id, t_id) = import_resolved.exports.lookup_term(name)?;

        let uri = {
            let files = backend.files.lock().unwrap();
            let uri = files.path(f_id);
            Url::parse(&uri).ok()?
        };

        let (content, parsed, indexed) = {
            let mut runtime = backend.runtime.lock().unwrap();
            let content = runtime.content(f_id);
            let (parsed, _) = runtime.parsed(f_id);
            let indexed = runtime.indexed(f_id);
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

    let goto_type = |name: &str| {
        let (f_id, t_id) = import_resolved.exports.lookup_type(name)?;

        let uri = {
            let files = backend.files.lock().unwrap();
            let uri = files.path(f_id);
            Url::parse(&uri).ok()?
        };

        let (content, parsed, indexed) = {
            let mut runtime = backend.runtime.lock().unwrap();
            let content = runtime.content(f_id);
            let (parsed, _) = runtime.parsed(f_id);
            let indexed = runtime.indexed(f_id);
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
            goto_term(name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            goto_type(name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            goto_type(name)
        }
        cst::ImportItem::ImportOperator(_) => None,
        cst::ImportItem::ImportTypeOperator(_) => None,
    }
}

async fn definition_binder(
    backend: &Backend,
    f_id: FileId,
    b_id: BinderId,
) -> Option<GotoDefinitionResponse> {
    let (resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_binder_kind(b_id)?;
    match kind {
        BinderKind::Constructor { resolution, .. } => {
            definition_deferred(backend, &resolved, &lowered, *resolution).await
        }
        _ => None,
    }
}

async fn definition_expression(
    backend: &Backend,
    uri: Url,
    f_id: FileId,
    e_id: ExpressionId,
) -> Option<GotoDefinitionResponse> {
    let (content, parsed, resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let content = runtime.content(f_id);
        let (parsed, _) = runtime.parsed(f_id);
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (content, parsed, resolved, lowered)
    };

    let kind = lowered.intermediate.index_expression_kind(e_id)?;
    match kind {
        ExpressionKind::Constructor { resolution } => {
            definition_deferred(backend, &resolved, &lowered, *resolution).await
        }
        ExpressionKind::Variable { resolution } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermResolution::Deferred(id) => {
                    definition_deferred(backend, &resolved, &lowered, *id).await
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
        ExpressionKind::OperatorName { resolution } => {
            definition_deferred(backend, &resolved, &lowered, *resolution).await
        }
        _ => None,
    }
}

async fn definition_type(
    backend: &Backend,
    uri: Url,
    f_id: FileId,
    t_id: TypeId,
) -> Option<GotoDefinitionResponse> {
    let (content, parsed, resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let content = runtime.content(f_id);
        let (parsed, _) = runtime.parsed(f_id);
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (content, parsed, resolved, lowered)
    };

    let kind = lowered.intermediate.index_type_kind(t_id)?;
    match kind {
        lowering::TypeKind::Constructor { resolution } => {
            definition_deferred(backend, &resolved, &lowered, *resolution).await
        }
        lowering::TypeKind::Operator { resolution } => {
            definition_deferred(backend, &resolved, &lowered, *resolution).await
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

async fn definition_deferred(
    backend: &Backend,
    resolved: &FullResolvedModule,
    lowered: &FullLoweredModule,
    id: DeferredResolutionId,
) -> Option<GotoDefinitionResponse> {
    let deferred = &lowered.graph[id];
    let prefix = deferred.qualifier.as_deref();
    let name = deferred.name.as_deref()?;
    match deferred.domain {
        ResolutionDomain::Term => {
            let (f_id, t_id) = resolved.lookup_term(prefix, name)?;

            let uri = {
                let files = backend.files.lock().unwrap();
                let path = files.path(f_id);
                Url::parse(&path).ok()?
            };

            let (content, parsed, indexed) = {
                let mut runtime = backend.runtime.lock().unwrap();
                let content = runtime.content(f_id);
                let (parsed, _) = runtime.parsed(f_id);
                let indexed = runtime.indexed(f_id);
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
        ResolutionDomain::Type => {
            let (f_id, t_id) = resolved.lookup_type(prefix, name)?;

            let uri = {
                let files = backend.files.lock().unwrap();
                let path = files.path(f_id);
                Url::parse(&path).ok()?
            };

            let (content, parsed, indexed) = {
                let mut runtime = backend.runtime.lock().unwrap();
                let content = runtime.content(f_id);
                let (parsed, _) = runtime.parsed(f_id);
                let indexed = runtime.indexed(f_id);
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
