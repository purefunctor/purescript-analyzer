use std::ops::Index;

use files::FileId;
use indexing::{
    FullIndexedModule, ImportItemId, IndexingSource, TermItemId, TermItemKind, TypeItemKind,
};
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
use smol_str::SmolStrBuilder;
use syntax::{SyntaxNode, cst};
use tower_lsp::lsp_types::*;

use super::{Backend, locate};

pub(super) async fn hover(backend: &Backend, uri: Url, position: Position) -> Option<Hover> {
    let f_id = {
        let files = backend.files.lock().unwrap();
        let uri = uri.as_str();
        files.id(uri)?
    };

    let located = locate::locate(backend, f_id, position);

    match located {
        locate::Located::ImportItem(i_id) => hover_import(backend, f_id, i_id).await,
        locate::Located::Binder(b_id) => hover_binder(backend, f_id, b_id).await,
        locate::Located::Expression(e_id) => hover_expression(backend, f_id, e_id).await,
        locate::Located::Type(t_id) => hover_type(backend, f_id, t_id).await,
        locate::Located::Nothing => None,
    }
}

async fn hover_import(backend: &Backend, f_id: FileId, i_id: ImportItemId) -> Option<Hover> {
    let (parsed, indexed) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let (parsed, _) = runtime.parsed(f_id);
        let indexed = runtime.indexed(f_id);
        (parsed, indexed)
    };

    let node = {
        let root = parsed.syntax_node();
        let ptr = &indexed.source[i_id];
        ptr.to_node(&root)
    };

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
        let mut runtime = backend.runtime.lock().unwrap();
        let import_id = runtime.module_file(&module)?;
        runtime.resolved(import_id)
    };

    let hover_term_import = |name: &str| {
        let (f_id, t_id) = import_resolved.exports.lookup_term(name)?;
        hover_file_term(backend, f_id, t_id)
    };

    let hover_type_import = |name: &str| {
        let (f_id, t_id) = import_resolved.exports.lookup_type(name)?;
        hover_file_type(backend, f_id, t_id)
    };

    match node {
        cst::ImportItem::ImportValue(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_term_import(name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_type_import(name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_type_import(name)
        }
        cst::ImportItem::ImportOperator(_) => None,
        cst::ImportItem::ImportTypeOperator(_) => None,
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
            hover_file_term(backend, f_id, t_id)
        }
        ResolutionDomain::Type => {
            let (f_id, t_id) = resolved.lookup_type(prefix, name)?;
            hover_file_type(backend, f_id, t_id)
        }
    }
}

fn hover_file_term(backend: &Backend, f_id: FileId, t_id: TermItemId) -> Option<Hover> {
    let (parsed, indexed) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let (parsed, _) = runtime.parsed(f_id);
        let indexed = runtime.indexed(f_id);
        (parsed, indexed)
    };

    let root = parsed.syntax_node();
    let item = &indexed.items[t_id];

    let (annotation, syntax) = match &item.kind {
        TermItemKind::ClassMember { id } => {
            annotation_syntax(&indexed, &root, &Some(*id), &Some(*id))?
        }
        TermItemKind::Constructor { id } => {
            annotation_syntax(&indexed, &root, &Some(*id), &Some(*id))?
        }
        TermItemKind::Derive { .. } => return None,
        TermItemKind::Foreign { id } => annotation_syntax(&indexed, &root, &Some(*id), &Some(*id))?,
        TermItemKind::Instance { .. } => return None,
        TermItemKind::Operator { .. } => return None,
        TermItemKind::Value { signature, equations } => {
            let equation = equations.first().copied();
            annotation_syntax(&indexed, &root, signature, &equation)?
        }
    };

    let annotation = annotation.map(|range| render_annotation(&root, range));
    let syntax = syntax.map(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().flatten().collect();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

fn hover_file_type(
    backend: &Backend,
    f_id: Idx<files::File>,
    t_id: Idx<indexing::TypeItem>,
) -> Option<Hover> {
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
        TypeItemKind::Foreign { id } => annotation_syntax(&indexed, &root, &Some(*id), &Some(*id))?,
        TypeItemKind::Operator { id } => {
            annotation_syntax(&indexed, &root, &Some(*id), &Some(*id))?
        }
    };

    let annotation = annotation.map(|range| render_annotation(&root, range));
    let syntax = syntax.map(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().flatten().collect();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
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
        locate::annotation_syntax_range(root, ptr)
    });
    let equation = || {
        let id = equation.as_ref()?;
        let ptr = indexed.source[*id].syntax_node_ptr();
        let (annotation, _) = locate::annotation_syntax_range(root, ptr);
        Some((annotation, None))
    };
    signature.or_else(equation)
}

fn render_annotation(root: &SyntaxNode, range: TextRange) -> Vec<MarkedString> {
    let source = root.text().slice(range).to_string();
    let source = source.trim();
    let cleaned = source.lines().map(|line| line.trim_start_matches("-- |").trim()).join("\n");
    if cleaned.is_empty() {
        vec![]
    } else {
        vec![MarkedString::String("---".to_string()), MarkedString::String(cleaned)]
    }
}

fn render_syntax(root: &SyntaxNode, range: TextRange) -> Vec<MarkedString> {
    let source = root.text().slice(range).to_string();
    let value = source.trim().to_string();
    vec![MarkedString::LanguageString(LanguageString { language: "purescript".to_string(), value })]
}
