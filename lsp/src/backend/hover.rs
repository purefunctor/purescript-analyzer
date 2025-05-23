use std::ops::Index;

use files::FileId;
use indexing::{FullIndexedModule, IndexingSource, TermItemKind, TypeItemKind};
use itertools::Itertools;
use la_arena::Idx;
use lowering::{
    BinderId, BinderKind, DeferredResolutionId, ExpressionId, ExpressionKind, FullLoweredModule,
    ResolutionDomain, TermResolution, TypeId, TypeKind,
};
use resolving::FullResolvedModule;
use rowan::{
    TextRange,
    ast::{AstNode, AstPtr},
};
use syntax::SyntaxNode;
use tower_lsp::lsp_types::*;

use super::{Backend, locate};

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
        locate::Located::ImportItem(_) => None,
        locate::Located::Binder(b_id) => hover_binder(backend, f_id, b_id).await,
        locate::Located::Expression(e_id) => hover_expression(backend, f_id, e_id).await,
        locate::Located::Type(t_id) => hover_type(backend, f_id, t_id).await,
        locate::Located::Nothing => None,
    }
}

async fn hover_binder(backend: &Backend, f_id: FileId, b_id: BinderId) -> Option<Hover> {
    let (resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_binder_kind(b_id)?;
    match kind {
        BinderKind::Constructor { resolution, .. } => {
            hover_deferred(backend, &resolved, &lowered, *resolution).await
        }
        _ => None,
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

async fn hover_type(backend: &Backend, f_id: FileId, t_id: TypeId) -> Option<Hover> {
    let (resolved, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_type_kind(t_id)?;
    match kind {
        TypeKind::Constructor { resolution } => {
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
                TermItemKind::ClassMember { id } => {
                    let ptr = indexed.source[*id].syntax_node_ptr();
                    let (annotation, syntax) = locate::annotation_syntax_range(&root, ptr);

                    let annotation = annotation.map(|range| render_annotation(&root, range));
                    let syntax = syntax.map(|range| render_syntax(&root, range));
                    let separator =
                        annotation.as_ref().map(|_| MarkedString::String("---".to_string()));
                    let array = [syntax, separator, annotation].into_iter().flatten().collect_vec();
                    let contents = HoverContents::Array(array);
                    let range = None;

                    Some(Hover { contents, range })
                }
                TermItemKind::Constructor { id } => {
                    let ptr = indexed.source[*id].syntax_node_ptr();
                    let (annotation, syntax) = locate::annotation_syntax_range(&root, ptr);

                    let annotation = annotation.map(|range| render_annotation(&root, range));
                    let syntax = syntax.map(|range| render_syntax(&root, range));
                    let separator =
                        annotation.as_ref().map(|_| MarkedString::String("---".to_string()));
                    let array = [syntax, separator, annotation].into_iter().flatten().collect_vec();
                    let contents = HoverContents::Array(array);
                    let range = None;

                    Some(Hover { contents, range })
                }
                TermItemKind::Derive { .. } => None,
                TermItemKind::Foreign { id } => {
                    let ptr = indexed.source[*id].syntax_node_ptr();
                    let (annotation, syntax) = locate::annotation_syntax_range(&root, ptr);

                    let annotation = annotation.map(|range| render_annotation(&root, range));
                    let syntax = syntax.map(|range| render_syntax(&root, range));
                    let separator =
                        annotation.as_ref().map(|_| MarkedString::String("---".to_string()));
                    let array = [syntax, separator, annotation].into_iter().flatten().collect_vec();
                    let contents = HoverContents::Array(array);
                    let range = None;

                    Some(Hover { contents, range })
                }
                TermItemKind::Instance { .. } => None,
                TermItemKind::Operator { .. } => None,
                TermItemKind::Value { signature, equations } => {
                    let signature = signature.map(|id| {
                        let ptr = indexed.source[id].syntax_node_ptr();
                        locate::annotation_syntax_range(&root, ptr)
                    });

                    let equation = || {
                        let id = equations.first()?;
                        let ptr = indexed.source[*id].syntax_node_ptr();
                        let (annotation, _) = locate::annotation_syntax_range(&root, ptr);
                        Some((annotation, None))
                    };

                    let (annotation, syntax) = signature.or_else(equation)?;

                    let annotation = annotation.map(|range| render_annotation(&root, range));

                    let separator =
                        annotation.as_ref().map(|_| MarkedString::String("---".to_string()));

                    let syntax = syntax.map(|range| render_syntax(&root, range));

                    let array = [syntax, separator, annotation].into_iter().flatten().collect_vec();
                    let contents = HoverContents::Array(array);
                    let range = None;

                    Some(Hover { contents, range })
                }
            }
        }
        ResolutionDomain::Type => {
            let (f_id, t_id) = resolved.lookup_type(prefix, name)?;

            let (parsed, indexed) = {
                let mut runtime = backend.runtime.lock().unwrap();
                let (parsed, _) = runtime.parsed(f_id);
                let indexed = runtime.indexed(f_id);
                (parsed, indexed)
            };

            let root = parsed.syntax_node();
            let item = &indexed.items[t_id];
            let (annotation, syntax) = match &item.kind {
                TypeItemKind::Data { signature, equation, .. } => {
                    annotation_syntax(&indexed, &root, signature, equation)?
                }
                TypeItemKind::Newtype { signature, equation, .. } => {
                    annotation_syntax(&indexed, &root, signature, equation)?
                }
                TypeItemKind::Synonym { signature, equation, .. } => {
                    annotation_syntax(&indexed, &root, signature, equation)?
                }
                TypeItemKind::Class { signature, declaration, .. } => {
                    annotation_syntax(&indexed, &root, signature, declaration)?
                }
                TypeItemKind::Foreign { id } => {
                    annotation_syntax(&indexed, &root, &Some(*id), &Some(*id))?
                }
                TypeItemKind::Operator { id } => {
                    annotation_syntax(&indexed, &root, &Some(*id), &Some(*id))?
                }
            };

            let annotation = annotation.map(|range| render_annotation(&root, range));
            let separator = annotation.as_ref().map(|_| MarkedString::String("---".to_string()));
            let syntax = syntax.map(|range| render_syntax(&root, range));

            let array = [syntax, separator, annotation].into_iter().flatten().collect_vec();
            let contents = HoverContents::Array(array);
            let range = None;

            Some(Hover { contents, range })
        }
    }
}

fn annotation_syntax<S, E>(
    indexed: &FullIndexedModule,
    root: &SyntaxNode,
    signature: &Option<Idx<AstPtr<S>>>,
    equation: &Option<Idx<AstPtr<E>>>,
) -> Option<(Option<TextRange>, Option<TextRange>)>
where
    S: AstNode<Language = syntax::PureScript>,
    E: AstNode<Language = syntax::PureScript>,
    IndexingSource: Index<Idx<AstPtr<S>>, Output = AstPtr<S>>,
    IndexingSource: Index<Idx<AstPtr<E>>, Output = AstPtr<E>>,
{
    let signature = signature.map(|id| {
        let ptr = indexed.source[id].syntax_node_ptr();
        locate::annotation_syntax_range(&root, ptr)
    });
    let equation = || {
        let id = equation.as_ref()?;
        let ptr = indexed.source[*id].syntax_node_ptr();
        let (annotation, _) = locate::annotation_syntax_range(&root, ptr);
        Some((annotation, None))
    };
    Some(signature.or_else(equation)?)
}

fn render_annotation(root: &SyntaxNode, range: TextRange) -> MarkedString {
    let source = root.text().slice(range).to_string();
    let source = source.trim();
    let cleaned = source.lines().map(|line| line.trim_start_matches("-- |").trim()).join("\n");
    MarkedString::String(cleaned)
}

fn render_syntax(root: &SyntaxNode, range: TextRange) -> MarkedString {
    let source = root.text().slice(range).to_string();
    let value = source.trim().to_string();
    MarkedString::LanguageString(LanguageString { language: "purescript".to_string(), value })
}
