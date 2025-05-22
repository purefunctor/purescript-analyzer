use files::FileId;
use indexing::{TermItemKind, TypeItemKind};
use lowering::{
    BinderKind, DeferredResolutionId, ExpressionId, ExpressionKind, FullLoweredModule,
    ResolutionDomain, TermResolution, TypeVariableResolution,
};
use parsing::ParsedModule;
use resolving::FullResolvedModule;
use rowan::{SyntaxText, TextRange, ast::AstPtr};
use syntax::{SyntaxKind, SyntaxNode, SyntaxNodePtr, cst};
use tower_lsp::lsp_types::*;

use crate::locate;

use super::Backend;

pub(super) async fn definition(
    backend: &Backend,
    uri: Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let (id, content) = {
        let files = backend.files.lock().unwrap();
        let uri = uri.as_str();
        let id = files.id(uri)?;
        let content = files.content(id);
        (id, content)
    };

    let parsed = {
        let mut runtime = backend.runtime.lock().unwrap();
        let (parsed, _) = runtime.parsed(id);
        parsed
    };

    let thing = locate::thing_at_position(&content, &parsed, position);

    match thing {
        locate::Thing::Annotation(_) => None,
        locate::Thing::Binder(ptr) => definition_binder(backend, id, ptr).await,
        locate::Thing::Expression(ptr) => {
            definition_expression(backend, uri, id, &content, parsed, ptr).await
        }
        locate::Thing::Type(ptr) => definition_type(backend, uri, id, &content, parsed, ptr).await,
        locate::Thing::Nothing => None,
    }
}

async fn definition_binder(
    backend: &Backend,
    id: FileId,
    ptr: AstPtr<cst::Binder>,
) -> Option<GotoDefinitionResponse> {
    let (resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let resolved = runtime.resolved(id);
        let lowered = runtime.lowered(id);
        (resolved, lowered)
    };

    let id = lowered.source.lookup_bd(&ptr)?;
    let kind = lowered.intermediate.index_binder_kind(id)?;

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
    id: FileId,
    content: &str,
    parsed: ParsedModule,
    ptr: AstPtr<cst::Expression>,
) -> Option<GotoDefinitionResponse> {
    let (resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let resolved = runtime.resolved(id);
        let lowered = runtime.lowered(id);
        (resolved, lowered)
    };

    let id = lowered.source.lookup_ex(&ptr)?;
    let kind = lowered.intermediate.index_expression_kind(id)?;

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
                    let range = range_without_annotation(content, ptr, &root)?;
                    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
                }
                TermResolution::Let(binding) => {
                    let root = parsed.syntax_node();

                    let signature = binding
                        .signature
                        .and_then(|id| {
                            let ptr = lowered.source[id].syntax_node_ptr();
                            range_without_annotation(content, &ptr, &root)
                        })
                        .into_iter();

                    let equations = binding.equations.iter().filter_map(|&id| {
                        let ptr = lowered.source[id].syntax_node_ptr();
                        range_without_annotation(content, &ptr, &root)
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
    id: FileId,
    content: &str,
    parsed: ParsedModule,
    ptr: AstPtr<cst::Type>,
) -> Option<GotoDefinitionResponse> {
    let (resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let resolved = runtime.resolved(id);
        let lowered = runtime.lowered(id);
        (resolved, lowered)
    };

    let id = lowered.source.lookup_ty(&ptr)?;
    let kind = lowered.intermediate.index_type_kind(id)?;

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
                    let range = range_without_annotation(content, ptr, &root)?;
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
                .filter_map(|ptr| range_without_annotation(&content, &ptr, &root))
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
                .filter_map(|ptr| range_without_annotation(&content, &ptr, &root))
                .reduce(|start, end| Range { start: start.start, end: end.end })?;

            Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
        }
    }
}

fn range_without_annotation(
    content: &str,
    ptr: &SyntaxNodePtr,
    root: &SyntaxNode,
) -> Option<Range> {
    let range = text_range_after_annotation(ptr, root)?;

    let start = locate::offset_to_position(content, range.start());
    let end = locate::offset_to_position(content, range.end());

    Some(Range { start, end })
}

fn text_range_after_annotation(ptr: &SyntaxNodePtr, root: &SyntaxNode) -> Option<TextRange> {
    let node = ptr.to_node(root);
    let mut children = node.children_with_tokens().peekable();

    if let Some(child) = children.peek() {
        if matches!(child.kind(), SyntaxKind::Annotation) {
            children.next();
        }
    }

    let start = children.next()?.text_range().start();
    let end = children.last().map_or(start, |child| child.text_range().end());

    Some(TextRange::new(start, end))
}

fn annotation_text(ptr: &SyntaxNodePtr, root: &SyntaxNode) -> Option<SyntaxText> {
    let node = ptr.to_node(root);
    let node = node.first_child_by_kind(&|kind| matches!(kind, SyntaxKind::Annotation))?;
    Some(node.text())
}

// TODO: Consider implementing a generic visitor pattern for Thing, or alternatively consider
// extracting more Thing-like enums to make traversals themselves generic and dispatchable.
pub(super) async fn hover(backend: &Backend, uri: Url, position: Position) -> Option<Hover> {
    let f_id = {
        let files = backend.files.lock().unwrap();
        let uri = uri.as_str();
        files.id(uri)?
    };

    let located = locate::locate(backend, f_id, position);

    match located {
        locate::Located::Binder(_) => None,
        locate::Located::Expression(e_id) => hover_expression(backend, f_id, e_id).await,
        locate::Located::Type(_) => None,
        locate::Located::Nothing => None,
    }
}

async fn hover_expression(backend: &Backend, f_id: FileId, e_id: ExpressionId) -> Option<Hover> {
    let (resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_expression_kind(e_id)?;
    match kind {
        ExpressionKind::Constructor { resolution } => {
            hover_deferred(backend, &resolved, &lowered, *resolution).await
        }
        ExpressionKind::Variable { resolution } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermResolution::Deferred(id) => {
                    hover_deferred(backend, &resolved, &lowered, *id).await
                }
                TermResolution::Binder(_) => None,
                TermResolution::Let(_) => None,
            }
        }
        ExpressionKind::OperatorName { resolution } => {
            hover_deferred(backend, &resolved, &lowered, *resolution).await
        }
        _ => None,
    }
}

async fn hover_deferred(
    backend: &Backend,
    resolved: &FullResolvedModule,
    lowered: &FullLoweredModule,
    id: DeferredResolutionId,
) -> Option<Hover> {
    let deferred = &lowered.graph[id];
    let prefix = deferred.qualifier.as_deref();
    let name = deferred.name.as_deref()?;

    match deferred.domain {
        ResolutionDomain::Term => {
            let (f_id, t_id) = resolved.lookup_term(prefix, name)?;

            let (parsed, indexed) = {
                let mut runtime = backend.runtime.lock().unwrap();
                let (parsed, _) = runtime.parsed(f_id);
                let indexed = runtime.indexed(f_id);
                (parsed, indexed)
            };

            let root = parsed.syntax_node();
            let item = &indexed.items[t_id];

            match &item.kind {
                TermItemKind::ClassMember { .. } => None,
                TermItemKind::Constructor { .. } => None,
                TermItemKind::Derive { .. } => None,
                TermItemKind::Foreign { .. } => None,
                TermItemKind::Instance { .. } => None,
                TermItemKind::Operator { .. } => None,
                TermItemKind::Value { signature, .. } => {
                    let s = signature.and_then(|id| {
                        let ptr = &indexed.source[id].syntax_node_ptr();
                        let range = text_range_after_annotation(ptr, &root)?;
                        let value = root.text().slice(range).to_string();
                        Some(MarkedString::LanguageString(LanguageString {
                            language: "purescript".to_string(),
                            value,
                        }))
                    });
                    let a = signature.and_then(|id| {
                        let ptr = &indexed.source[id].syntax_node_ptr();
                        let text = annotation_text(ptr, &root)?;
                        let text = text.to_string();
                        let markdown = text
                            .trim()
                            .lines()
                            .take_while(|line| line.starts_with("-- |"))
                            .map(|line| line.trim_start_matches("-- |").trim().to_string())
                            .reduce(|a, b| format!("\n{}\n{}", a, b).to_string())?;
                        Some(MarkedString::String(format!("---\n{}", markdown)))
                    });
                    let array: Vec<_> = [s, a].into_iter().flatten().collect();
                    Some(Hover { contents: HoverContents::Array(array), range: None })
                }
            }
        }
        ResolutionDomain::Type => {
            let (f_id, t_id) = resolved.lookup_type(prefix, name)?;

            let _uri = {
                let files = backend.files.lock().unwrap();
                let path = files.path(f_id);
                Url::parse(&path).ok()?
            };

            let (_content, _parsed, indexed) = {
                let mut runtime = backend.runtime.lock().unwrap();
                let content = runtime.content(f_id);
                let (parsed, _) = runtime.parsed(f_id);
                let indexed = runtime.indexed(f_id);
                (content, parsed, indexed)
            };

            let item = &indexed.items[t_id];

            match &item.kind {
                TypeItemKind::Data { .. } => None,
                TypeItemKind::Newtype { .. } => None,
                TypeItemKind::Synonym { .. } => None,
                TypeItemKind::Class { .. } => None,
                TypeItemKind::Foreign { .. } => None,
                TypeItemKind::Operator { .. } => None,
            }
        }
    }
}
