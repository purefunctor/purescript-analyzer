use files::FileId;
use indexing::{TermItemKind, TypeItemKind};
use itertools::Itertools;
use lowering::{
    DeferredResolutionId, ExpressionId, ExpressionKind, FullLoweredModule, ResolutionDomain,
    TermResolution,
};
use resolving::FullResolvedModule;
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

                    let annotation = annotation.map(|range| {
                        let source = root.text().slice(range).to_string();
                        let source = source.trim();
                        let cleaned = source
                            .lines()
                            .map(|line| line.trim_start_matches("-- |").trim())
                            .join("\n");
                        MarkedString::String(cleaned)
                    });

                    let separator =
                        annotation.as_ref().map(|_| MarkedString::String("---".to_string()));

                    let syntax = syntax.map(|range| {
                        let source = root.text().slice(range).to_string();
                        let value = source.trim().to_string();
                        MarkedString::LanguageString(LanguageString {
                            language: "purescript".to_string(),
                            value,
                        })
                    });

                    let array = [syntax, separator, annotation].into_iter().flatten().collect_vec();
                    let contents = HoverContents::Array(array);
                    let range = None;

                    Some(Hover { contents, range })
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
