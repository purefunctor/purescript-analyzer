use std::ops::Index;

use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::{FileId, Files};
use indexing::{
    FullIndexedModule, ImportItemId, IndexingSource, TermItemId, TermItemKind, TypeItemId,
    TypeItemKind,
};
use itertools::Itertools;
use la_arena::Idx;
use lowering::{
    BinderId, BinderKind, Domain, ExpressionId, ExpressionKind, FullLoweredModule, LetBound,
    Nominal, QualifiedNameId, TermVariableResolution, TypeId, TypeKind,
};
use resolving::FullResolvedModule;
use rowan::{
    TextRange,
    ast::{AstNode, AstPtr},
};
use smol_str::SmolStrBuilder;
use syntax::{SyntaxKind, SyntaxNode, cst};

use crate::locate;

pub fn implementation(
    engine: &QueryEngine,
    files: &Files,
    uri: Url,
    position: Position,
) -> Option<Hover> {
    let f_id = {
        let uri = uri.as_str();
        files.id(uri)?
    };

    let located = locate::locate(engine, f_id, position);

    match located {
        locate::Located::ModuleName(cst) => hover_module_name(engine, f_id, cst),
        locate::Located::ImportItem(i_id) => hover_import(engine, f_id, i_id),
        locate::Located::Binder(b_id) => hover_binder(engine, f_id, b_id),
        locate::Located::Expression(e_id) => hover_expression(engine, f_id, e_id),
        locate::Located::Type(t_id) => hover_type(engine, f_id, t_id),
        locate::Located::Operator(q_id) => {
            let resolved = engine.resolved(f_id).ok()?;
            let lowered = engine.lowered(f_id).ok()?;
            hover_qualified_name(engine, &resolved, &lowered, q_id)
        }
        locate::Located::Nothing => None,
    }
}

fn hover_module_name(
    engine: &QueryEngine,
    f_id: FileId,
    cst: AstPtr<cst::ModuleName>,
) -> Option<Hover> {
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

    let module_id = engine.module_file(&module)?;

    let (root, annotation, syntax) = annotation_syntax_file(engine, module_id)?;

    let annotation = annotation.map(|range| render_annotation(&root, range));
    let syntax = syntax.map(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().flatten().collect_vec();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

pub(crate) fn annotation_syntax_file(
    engine: &QueryEngine,
    f_id: FileId,
) -> Option<(SyntaxNode, Option<TextRange>, Option<TextRange>)> {
    let (parsed, _) = engine.parsed(f_id).ok()?;

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

fn hover_import(engine: &QueryEngine, f_id: FileId, i_id: ImportItemId) -> Option<Hover> {
    let (parsed, indexed) = {
        let (parsed, _) = engine.parsed(f_id).ok()?;
        let indexed = engine.indexed(f_id).ok()?;
        (parsed, indexed)
    };

    let node = {
        let root = parsed.syntax_node();
        let ptr = &indexed.source[i_id];
        ptr.try_to_node(&root)?
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
        let import_id = engine.module_file(&module)?;
        engine.resolved(import_id).ok()?
    };

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

fn hover_binder(engine: &QueryEngine, f_id: FileId, b_id: BinderId) -> Option<Hover> {
    let (resolved, lowered) = {
        let resolved = engine.resolved(f_id).ok()?;
        let lowered = engine.lowered(f_id).ok()?;
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_binder_kind(b_id)?;
    match kind {
        BinderKind::Constructor { id: Some(id), .. } => {
            hover_qualified_name(engine, &resolved, &lowered, *id)
        }
        _ => None,
    }
}

fn hover_expression(engine: &QueryEngine, f_id: FileId, e_id: ExpressionId) -> Option<Hover> {
    let (resolved, lowered) = {
        let resolved = engine.resolved(f_id).ok()?;
        let lowered = engine.lowered(f_id).ok()?;
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_expression_kind(e_id)?;
    match kind {
        ExpressionKind::Constructor { id: Some(id), .. } => {
            hover_qualified_name(engine, &resolved, &lowered, *id)
        }
        ExpressionKind::Variable { id: Some(id), resolution } => {
            let resolution = resolution.as_ref()?;
            match resolution {
                TermVariableResolution::Global => {
                    hover_qualified_name(engine, &resolved, &lowered, *id)
                }
                TermVariableResolution::Binder(_) => None,
                TermVariableResolution::Let(let_binding) => {
                    let (parsed, _) = engine.parsed(f_id).ok()?;
                    let root = parsed.syntax_node();
                    hover_let(&root, &lowered, let_binding)
                }
                TermVariableResolution::Nominal(Nominal { qualifier, name }) => {
                    let qualifier = qualifier.as_deref();
                    let name = name.as_str();
                    hover_nominal(engine, &resolved, Domain::Term, qualifier, name)
                }
            }
        }
        ExpressionKind::OperatorName { id: Some(id), .. } => {
            hover_qualified_name(engine, &resolved, &lowered, *id)
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
        locate::annotation_syntax_range(root, ptr)
    });

    if let Some((annotation, syntax)) = signature {
        let annotation = annotation.map(|range| render_annotation(root, range));
        let syntax = syntax.map(|range| render_syntax(root, range));

        let array = [syntax, annotation].into_iter().flatten().flatten().collect();
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

fn hover_type(engine: &QueryEngine, f_id: FileId, t_id: TypeId) -> Option<Hover> {
    let (resolved, lowered) = {
        let resolved = engine.resolved(f_id).ok()?;
        let lowered = engine.lowered(f_id).ok()?;
        (resolved, lowered)
    };

    let kind = lowered.intermediate.index_type_kind(t_id)?;
    match kind {
        TypeKind::Constructor { id: Some(id), .. } => {
            hover_qualified_name(engine, &resolved, &lowered, *id)
        }
        _ => None,
    }
}

fn hover_qualified_name(
    engine: &QueryEngine,
    resolved: &FullResolvedModule,
    lowered: &FullLoweredModule,
    id: QualifiedNameId,
) -> Option<Hover> {
    let ir = lowered.intermediate.index_qualified_name(id)?;
    let domain = ir.domain;
    let qualifier = ir.qualifier.as_deref();
    let name = ir.name.as_str();
    hover_nominal(engine, resolved, domain, qualifier, name)
}

fn hover_nominal(
    engine: &QueryEngine,
    resolved: &FullResolvedModule,
    domain: Domain,
    qualifier: Option<&str>,
    name: &str,
) -> Option<Hover> {
    let prim = {
        let id = engine.prim_id();
        engine.resolved(id).ok()?
    };
    match domain {
        Domain::Term => {
            let (f_id, t_id) = resolved.lookup_term(&prim, qualifier, name)?;
            hover_file_term(engine, f_id, t_id)
        }
        Domain::Type => {
            let (f_id, t_id) = resolved.lookup_type(&prim, qualifier, name)?;
            hover_file_type(engine, f_id, t_id)
        }
    }
}

fn hover_file_term(engine: &QueryEngine, f_id: FileId, t_id: TermItemId) -> Option<Hover> {
    let (root, annotation, syntax) = annotation_syntax_file_term(engine, f_id, t_id)?;

    let annotation = annotation.map(|range| render_annotation(&root, range));
    let syntax = syntax.map(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().flatten().collect();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

pub(super) fn annotation_syntax_file_term(
    engine: &QueryEngine,
    f_id: FileId,
    t_id: TermItemId,
) -> Option<(SyntaxNode, Option<TextRange>, Option<TextRange>)> {
    let (parsed, _) = engine.parsed(f_id).ok()?;
    let indexed = engine.indexed(f_id).ok()?;

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

fn hover_file_type(engine: &QueryEngine, f_id: FileId, t_id: TypeItemId) -> Option<Hover> {
    let (root, annotation, syntax) = annotation_syntax_file_type(engine, f_id, t_id)?;

    let annotation = annotation.map(|range| render_annotation(&root, range));
    let syntax = syntax.map(|range| render_syntax(&root, range));

    let array = [syntax, annotation].into_iter().flatten().flatten().collect();
    let contents = HoverContents::Array(array);
    let range = None;

    Some(Hover { contents, range })
}

pub(crate) fn annotation_syntax_file_type(
    engine: &QueryEngine,
    f_id: FileId,
    t_id: TypeItemId,
) -> Option<(SyntaxNode, Option<TextRange>, Option<TextRange>)> {
    let (parsed, _) = engine.parsed(f_id).ok()?;
    let indexed = engine.indexed(f_id).ok()?;

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
