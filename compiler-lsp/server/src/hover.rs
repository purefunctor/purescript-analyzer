use std::ops::Index;

use async_lsp::lsp_types::*;
use files::FileId;
use indexing::{
    FullIndexedModule, ImportItemId, IndexingSource, TermItemId, TermItemKind, TypeItemId,
    TypeItemKind,
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
use syntax::{SyntaxKind, SyntaxNode, cst};

use crate::{Compiler, locate};

pub(super) fn implementation(
    compiler: &mut Compiler,
    uri: Url,
    position: Position,
) -> Option<Hover> {
    let f_id = {
        let uri = uri.as_str();
        compiler.files.id(uri)?
    };

    let located = locate::locate(compiler, f_id, position);

    match located {
        locate::Located::ModuleName(cst) => hover_module_name(compiler, f_id, cst),
        locate::Located::ImportItem(i_id) => hover_import(compiler, f_id, i_id),
        locate::Located::Binder(b_id) => hover_binder(compiler, f_id, b_id),
        locate::Located::Expression(e_id) => hover_expression(compiler, f_id, e_id),
        locate::Located::Type(t_id) => hover_type(compiler, f_id, t_id),
        locate::Located::OperatorInChain(_, _) => None,
        locate::Located::Nothing => None,
    }
}

fn hover_module_name(
    compiler: &mut Compiler,
    f_id: FileId,
    cst: AstPtr<cst::ModuleName>,
) -> Option<Hover> {
    let (parsed, _) = compiler.runtime.parsed(f_id);

    let root = parsed.syntax_node();
    let module = cst.to_node(&root);
    let module = {
        let mut buffer = SmolStrBuilder::default();

        if let Some(token) = module.qualifier().and_then(|cst| cst.text()) {
            buffer.push_str(token.text());
        }

        let token = module.name_token()?;
        buffer.push_str(token.text());

        buffer.finish()
    };

    let module_id = compiler.runtime.module_file(&module)?;

    let (root, annotation, syntax) = annotation_syntax_file(compiler, module_id)?;

    let annotation = annotation.map(|range| render_annotation(&root, range));
    let syntax = syntax.map(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().flatten().collect_vec();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

pub(crate) fn annotation_syntax_file(
    compiler: &mut Compiler,
    f_id: FileId,
) -> Option<(SyntaxNode, Option<TextRange>, Option<TextRange>)> {
    let (parsed, _) = compiler.runtime.parsed(f_id);

    let root = parsed.syntax_node();
    let cst = parsed.cst().header()?;
    let node = cst.syntax();

    let annotation = node
        .first_child_by_kind(&|kind| matches!(kind, SyntaxKind::Annotation))
        .map(|annotation| annotation.text_range());

    let syntax = || {
        let module_token =
            node.first_child_or_token_by_kind(&|kind| matches!(kind, SyntaxKind::MODULE))?;
        let where_token =
            node.first_child_or_token_by_kind(&|kind| matches!(kind, SyntaxKind::WHERE))?;
        let start = module_token.text_range().start();
        let end = where_token.text_range().end();
        Some(TextRange::new(start, end))
    };

    let syntax = syntax();

    Some((root, annotation, syntax))
}

fn hover_import(compiler: &mut Compiler, f_id: FileId, i_id: ImportItemId) -> Option<Hover> {
    let (parsed, indexed) = {
        let runtime = &mut compiler.runtime;
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
        let runtime = &mut compiler.runtime;
        let import_id = runtime.module_file(&module)?;
        runtime.resolved(import_id)
    };

    let hover_term_import = |compiler: &mut Compiler, name: &str| {
        let (f_id, t_id) = import_resolved.exports.lookup_term(name)?;
        hover_file_term(compiler, f_id, t_id)
    };

    let hover_type_import = |compiler: &mut Compiler, name: &str| {
        let (f_id, t_id) = import_resolved.exports.lookup_type(name)?;
        hover_file_type(compiler, f_id, t_id)
    };

    match node {
        cst::ImportItem::ImportValue(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_term_import(compiler, name)
        }
        cst::ImportItem::ImportClass(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_type_import(compiler, name)
        }
        cst::ImportItem::ImportType(cst) => {
            let token = cst.name_token()?;
            let name = token.text();
            hover_type_import(compiler, name)
        }
        cst::ImportItem::ImportOperator(_) => None,
        cst::ImportItem::ImportTypeOperator(_) => None,
    }
}

fn hover_binder(compiler: &mut Compiler, f_id: FileId, b_id: BinderId) -> Option<Hover> {
    let (resolved, lowered) = {
        let runtime = &mut compiler.runtime;
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_binder_kind(b_id)?;
    match kind {
        BinderKind::Constructor { resolution, .. } => {
            hover_deferred(compiler, &resolved, &lowered, *resolution)
        }
        _ => None,
    }
}

fn hover_expression(compiler: &mut Compiler, f_id: FileId, e_id: ExpressionId) -> Option<Hover> {
    let (resolved, lowered) = {
        let runtime = &mut compiler.runtime;
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_expression_kind(e_id)?;
    match kind {
        ExpressionKind::Constructor { resolution } => {
            hover_deferred(compiler, &resolved, &lowered, *resolution)
        }
        ExpressionKind::Variable { resolution } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermResolution::Deferred(id) => hover_deferred(compiler, &resolved, &lowered, *id),
                TermResolution::Binder(_) => None,
                TermResolution::Let(_) => None,
            }
        }
        ExpressionKind::OperatorName { resolution } => {
            hover_deferred(compiler, &resolved, &lowered, *resolution)
        }
        _ => None,
    }
}

fn hover_type(compiler: &mut Compiler, f_id: FileId, t_id: TypeId) -> Option<Hover> {
    let (resolved, lowered) = {
        let runtime = &mut compiler.runtime;
        let resolved = runtime.resolved(f_id);
        let lowered = runtime.lowered(f_id);
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_type_kind(t_id)?;
    match kind {
        TypeKind::Constructor { resolution } => {
            hover_deferred(compiler, &resolved, &lowered, *resolution)
        }
        _ => None,
    }
}

fn hover_deferred(
    compiler: &mut Compiler,
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
            hover_file_term(compiler, f_id, t_id)
        }
        ResolutionDomain::Type => {
            let (f_id, t_id) = resolved.lookup_type(prefix, name)?;
            hover_file_type(compiler, f_id, t_id)
        }
    }
}

fn hover_file_term(compiler: &mut Compiler, f_id: FileId, t_id: TermItemId) -> Option<Hover> {
    let (root, annotation, syntax) = annotation_syntax_file_term(compiler, f_id, t_id)?;

    let annotation = annotation.map(|range| render_annotation(&root, range));
    let syntax = syntax.map(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().flatten().collect();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

pub(super) fn annotation_syntax_file_term(
    compiler: &mut Compiler,
    f_id: FileId,
    t_id: TermItemId,
) -> Option<(SyntaxNode, Option<TextRange>, Option<TextRange>)> {
    let (parsed, _) = compiler.runtime.parsed(f_id);
    let indexed = compiler.runtime.indexed(f_id);

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

    Some((root, annotation, syntax))
}

fn hover_file_type(
    compiler: &mut Compiler,
    f_id: Idx<files::File>,
    t_id: Idx<indexing::TypeItem>,
) -> Option<Hover> {
    let (root, annotation, syntax) = annotation_syntax_file_type(compiler, f_id, t_id)?;

    let annotation = annotation.map(|range| render_annotation(&root, range));
    let syntax = syntax.map(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().flatten().collect();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

pub(crate) fn annotation_syntax_file_type(
    compiler: &mut Compiler,
    f_id: FileId,
    t_id: TypeItemId,
) -> Option<(SyntaxNode, Option<TextRange>, Option<TextRange>)> {
    let (parsed, _) = compiler.runtime.parsed(f_id);
    let indexed = compiler.runtime.indexed(f_id);

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

    Some((root, annotation, syntax))
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
    let cleaned = render_annotation_string(root, range);
    if cleaned.is_empty() {
        vec![]
    } else {
        vec![MarkedString::String("---".to_string()), MarkedString::String(cleaned)]
    }
}

pub(crate) fn render_annotation_string(root: &SyntaxNode, range: TextRange) -> String {
    let source = root.text().slice(range).to_string();
    let source = source.trim();
    source.lines().map(|line| line.trim_start_matches("-- |").trim()).join("\n")
}

fn render_syntax(root: &SyntaxNode, range: TextRange) -> Vec<MarkedString> {
    let value = render_syntax_string(root, range);
    vec![MarkedString::LanguageString(LanguageString { language: "purescript".to_string(), value })]
}

pub(crate) fn render_syntax_string(root: &SyntaxNode, range: TextRange) -> String {
    let source = root.text().slice(range).to_string();
    source.trim().to_string()
}
