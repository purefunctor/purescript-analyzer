use files::FileId;
use indexing::{TermItemKind, TypeItemKind};
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
                TermItemKind::Value { signature, .. } => {
                    let s = signature.and_then(|id| {
                        let ptr = &indexed.source[id].syntax_node_ptr();
                        let range = locate::text_range_after_annotation(ptr, &root)?;
                        let value = root.text().slice(range).to_string();
                        Some(MarkedString::LanguageString(LanguageString {
                            language: "purescript".to_string(),
                            value,
                        }))
                    });
                    let a = signature.and_then(|id| {
                        let ptr = &indexed.source[id].syntax_node_ptr();
                        let text = locate::annotation_text(ptr, &root)?;
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
