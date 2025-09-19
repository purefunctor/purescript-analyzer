use std::ops;

use building::QueryEngine;
use files::FileId;
use indexing::{
    FullIndexedModule, IndexingSource, TermItemId, TermItemKind, TypeItemId, TypeItemKind,
};
use la_arena::Idx;
use rowan::{
    TextRange,
    ast::{AstNode, AstPtr},
};
use syntax::{SyntaxKind, SyntaxNode, SyntaxNodePtr};

use crate::AnalyzerError;

#[derive(Debug, Default)]
pub struct AnnotationSyntaxRange {
    pub annotation: Option<TextRange>,
    pub syntax: Option<TextRange>,
}

impl AnnotationSyntaxRange {
    pub fn from_ptr(root: &SyntaxNode, ptr: &SyntaxNodePtr) -> AnnotationSyntaxRange {
        ptr.try_to_node(root).map(|node| Self::from_node(&node)).unwrap_or_default()
    }

    pub fn from_node(node: &SyntaxNode) -> AnnotationSyntaxRange {
        let mut children = node.children_with_tokens().peekable();

        let annotation = children.next_if(|child| {
            let kind = child.kind();
            matches!(kind, SyntaxKind::Annotation)
        });

        let start = children.peek().map(|child| child.text_range());
        let end = children.last().map(|child| child.text_range());

        let annotation = annotation.map(|child| child.text_range());
        let syntax = start.zip(end).map(|(start, end)| start.cover(end));

        AnnotationSyntaxRange { annotation, syntax }
    }
}

pub fn extract_annotation(root: &SyntaxNode, range: TextRange) -> String {
    let text = root.text().slice(range);

    let mut annotation = String::default();

    text.for_each_chunk(|chunk| {
        let lines = chunk.lines().filter_map(|line| {
            let trimmed = line.trim_start_matches("-- |");
            if line != trimmed { Some(trimmed.trim_matches(' ')) } else { None }
        });

        let mut lines = lines.peekable();
        if let Some(line) = lines.next() {
            annotation.push_str(line);
        }

        lines.for_each(|line| {
            annotation.push('\n');
            annotation.push_str(line);
        });
    });

    annotation
}

pub fn extract_syntax(root: &SyntaxNode, range: TextRange) -> String {
    root.text().slice(range).to_string()
}

impl AnnotationSyntaxRange {
    pub fn of_file(
        engine: &QueryEngine,
        file_id: FileId,
    ) -> Result<(SyntaxNode, AnnotationSyntaxRange), AnalyzerError> {
        let (parsed, _) = engine.parsed(file_id)?;

        let root = parsed.syntax_node();

        let header = parsed.cst().header().ok_or(AnalyzerError::NonFatal)?;
        let header = header.syntax();

        let annotation = header
            .first_child_by_kind(&|kind| matches!(kind, SyntaxKind::Annotation))
            .map(|annotation| annotation.text_range());

        let syntax = {
            let module_token = header
                .first_child_or_token_by_kind(&|kind| matches!(kind, SyntaxKind::MODULE))
                .ok_or(AnalyzerError::NonFatal)?;
            let where_token = header
                .first_child_or_token_by_kind(&|kind| matches!(kind, SyntaxKind::WHERE))
                .ok_or(AnalyzerError::NonFatal)?;

            let start = module_token.text_range().start();
            let end = where_token.text_range().end();

            Some(TextRange::new(start, end))
        };

        Ok((root, AnnotationSyntaxRange { annotation, syntax }))
    }

    pub fn of_file_term(
        engine: &QueryEngine,
        file_id: FileId,
        term_id: TermItemId,
    ) -> Result<(SyntaxNode, AnnotationSyntaxRange), AnalyzerError> {
        let (parsed, _) = engine.parsed(file_id)?;
        let indexed = engine.indexed(file_id)?;

        let root = parsed.syntax_node();
        let item = &indexed.items[term_id];

        let range = match &item.kind {
            TermItemKind::ClassMember { id } => {
                signature_equation_range(&indexed, &root, &Some(*id), &Some(*id))
            }
            TermItemKind::Constructor { id } => {
                signature_equation_range(&indexed, &root, &Some(*id), &Some(*id))
            }
            TermItemKind::Derive { id } => {
                signature_equation_range(&indexed, &root, &Some(*id), &Some(*id))
            }
            TermItemKind::Foreign { id } => {
                signature_equation_range(&indexed, &root, &Some(*id), &Some(*id))
            }
            TermItemKind::Instance { id } => {
                signature_equation_range(&indexed, &root, &Some(*id), &Some(*id))
            }
            TermItemKind::Operator { id } => {
                signature_equation_range(&indexed, &root, &Some(*id), &Some(*id))
            }
            TermItemKind::Value { signature, equations } => {
                let equation = equations.first().copied();
                signature_equation_range(&indexed, &root, signature, &equation)
            }
        };

        Ok((root, range.ok_or(AnalyzerError::NonFatal)?))
    }

    pub fn of_file_type(
        engine: &QueryEngine,
        file_id: FileId,
        type_id: TypeItemId,
    ) -> Result<(SyntaxNode, AnnotationSyntaxRange), AnalyzerError> {
        let (parsed, _) = engine.parsed(file_id)?;
        let indexed = engine.indexed(file_id)?;

        let root = parsed.syntax_node();
        let item = &indexed.items[type_id];

        let range = match &item.kind {
            TypeItemKind::Data { signature, equation, .. } => {
                signature_equation_range(&indexed, &root, signature, equation)
            }
            TypeItemKind::Newtype { signature, equation, .. } => {
                signature_equation_range(&indexed, &root, signature, equation)
            }
            TypeItemKind::Synonym { signature, equation, .. } => {
                signature_equation_range(&indexed, &root, signature, equation)
            }
            TypeItemKind::Class { signature, declaration, .. } => {
                signature_equation_range(&indexed, &root, signature, declaration)
            }
            TypeItemKind::Foreign { id } => {
                signature_equation_range(&indexed, &root, &Some(*id), &Some(*id))
            }
            TypeItemKind::Operator { id } => {
                signature_equation_range(&indexed, &root, &Some(*id), &Some(*id))
            }
        };

        Ok((root, range.ok_or(AnalyzerError::NonFatal)?))
    }
}

fn signature_equation_range<S, E>(
    indexed: &FullIndexedModule,
    root: &SyntaxNode,
    signature: &Option<Idx<AstPtr<S>>>,
    equation: &Option<Idx<AstPtr<E>>>,
) -> Option<AnnotationSyntaxRange>
where
    S: AstNode<Language = syntax::PureScript>,
    E: AstNode<Language = syntax::PureScript>,
    IndexingSource: ops::Index<Idx<AstPtr<S>>, Output = AstPtr<S>>,
    IndexingSource: ops::Index<Idx<AstPtr<E>>, Output = AstPtr<E>>,
{
    let signature = signature.map(|id| {
        let ptr = indexed.source[id].syntax_node_ptr();
        AnnotationSyntaxRange::from_ptr(root, &ptr)
    });

    let equation = || {
        let id = equation.as_ref()?;
        let ptr = indexed.source[*id].syntax_node_ptr();
        let range = AnnotationSyntaxRange::from_ptr(root, &ptr);
        Some(AnnotationSyntaxRange { syntax: None, ..range })
    };

    signature.or_else(equation)
}
