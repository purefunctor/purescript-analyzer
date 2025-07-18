//! Abstractions for identifying syntax at a given location.

use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::FileId;
use indexing::{FullIndexedModule, ImportItemId};
use line_index::{LineCol, LineIndex};
use lowering::{BinderId, ExpressionId, FullLoweredModule, ResolutionDomain, TypeId};
use rowan::{
    TextRange, TextSize, TokenAtOffset,
    ast::{AstNode, AstPtr},
};
use smol_str::SmolStr;
use syntax::{SyntaxKind, SyntaxNode, SyntaxNodePtr, SyntaxToken, cst};

pub fn position_to_offset(content: &str, position: Position) -> Option<TextSize> {
    let line_index = LineIndex::new(content);
    let line_col = LineCol { line: position.line, col: position.character };
    line_index.offset(line_col)
}

pub fn offset_to_position(content: &str, offset: TextSize) -> Position {
    let line_index = LineIndex::new(content);
    let LineCol { line, col } = line_index.line_col(offset);
    Position { line, character: col }
}

pub fn text_range_to_range(content: &str, range: TextRange) -> Range {
    let line_index = LineIndex::new(content);

    let start = line_index.line_col(range.start());
    let start = Position { line: start.line, character: start.col };

    let end = line_index.line_col(range.end());
    let end = Position { line: end.line, character: end.col };

    Range { start, end }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Located {
    ModuleName(AstPtr<cst::ModuleName>),
    ImportItem(ImportItemId),
    Binder(BinderId),
    Expression(ExpressionId),
    Type(TypeId),
    OperatorInChain(ResolutionDomain, SmolStr),
    Nothing,
}

pub fn locate(engine: &QueryEngine, id: FileId, position: Position) -> Located {
    let (content, parsed, indexed, lowered) = {
        let content = engine.content(id);
        let Ok((parsed, _)) = engine.parsed(id) else { return Located::Nothing };
        let Ok(indexed) = engine.indexed(id) else { return Located::Nothing };
        let Ok(lowered) = engine.lowered(id) else { return Located::Nothing };
        (content, parsed, indexed, lowered)
    };

    let Some(offset) = position_to_offset(&content, position) else {
        return Located::Nothing;
    };

    let node = parsed.syntax_node();
    let token = node.token_at_offset(offset);

    match token {
        TokenAtOffset::None => Located::Nothing,
        TokenAtOffset::Single(token) => locate_single(&indexed, &lowered, token),
        TokenAtOffset::Between(left, right) => locate_between(&indexed, &lowered, left, right),
    }
}

fn locate_single(
    indexed: &FullIndexedModule,
    lowered: &FullLoweredModule,
    token: SyntaxToken,
) -> Located {
    token
        .parent_ancestors()
        .find_map(|node| locate_node(indexed, lowered, node))
        .unwrap_or(Located::Nothing)
}

fn locate_node(
    indexed: &FullIndexedModule,
    lowered: &FullLoweredModule,
    node: SyntaxNode,
) -> Option<Located> {
    let kind = node.kind();
    let ptr = SyntaxNodePtr::new(&node);
    if cst::ModuleName::can_cast(kind) {
        let ptr = ptr.cast()?;
        Some(Located::ModuleName(ptr))
    } else if cst::ImportItem::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = indexed.source.lookup_import(&ptr)?;
        Some(Located::ImportItem(id))
    } else if cst::Binder::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = lowered.source.lookup_bd(&ptr)?;
        Some(Located::Binder(id))
    } else if cst::Expression::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = lowered.source.lookup_ex(&ptr)?;
        Some(Located::Expression(id))
    } else if cst::Type::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = lowered.source.lookup_ty(&ptr)?;
        Some(Located::Type(id))
    } else if cst::QualifiedName::can_cast(kind) {
        let domain = node.ancestors().find_map(|node| {
            let kind = node.kind();
            if cst::Expression::can_cast(kind) {
                Some(ResolutionDomain::Term)
            } else if cst::Type::can_cast(kind) {
                Some(ResolutionDomain::Type)
            } else {
                None
            }
        })?;

        let node = cst::QualifiedName::cast(node)?;
        let token = node.operator()?;
        let text = token.text();
        let text = SmolStr::new(text);

        Some(Located::OperatorInChain(domain, text))
    } else {
        None
    }
}

fn locate_between(
    indexed: &FullIndexedModule,
    lowered: &FullLoweredModule,
    left: SyntaxToken,
    right: SyntaxToken,
) -> Located {
    let left = locate_single(indexed, lowered, left);
    let right = locate_single(indexed, lowered, right);
    match (&left, &right) {
        // If left/right share an ancestor;
        (_, _) if left == right => left,
        (_, Located::Nothing) => left,
        (Located::Nothing, _) => right,
        // otherwise, lean towards the right.
        (_, _) => right,
    }
}

pub fn range_without_annotation(
    content: &str,
    ptr: &SyntaxNodePtr,
    root: &SyntaxNode,
) -> Option<Range> {
    let range = text_range_after_annotation(ptr, root)?;
    Some(text_range_to_range(content, range))
}

pub fn text_range_after_annotation(ptr: &SyntaxNodePtr, root: &SyntaxNode) -> Option<TextRange> {
    let node = ptr.to_node(root);

    let mut children = node.children_with_tokens().peekable();
    children.next_if(|child| matches!(child.kind(), SyntaxKind::Annotation));

    let first = children.peek().map(|child| child.text_range());
    let last = children.last().map(|child| child.text_range());

    first.zip(last).map(|(start, end)| {
        let start = start.start();
        let end = end.end();
        TextRange::new(start, end)
    })
}

pub fn annotation_syntax_range(
    root: &SyntaxNode,
    ptr: SyntaxNodePtr,
) -> (Option<TextRange>, Option<TextRange>) {
    let node = ptr.to_node(root);
    let mut children = node.children_with_tokens().peekable();

    let annotation = children
        .next_if(|child| matches!(child.kind(), SyntaxKind::Annotation))
        .map(|child| child.text_range());

    let first = children.peek().map(|child| child.text_range());
    let last = children.last().map(|child| child.text_range());

    let syntax = first.zip(last).map(|(start, end)| {
        let start = start.start();
        let end = end.end();
        TextRange::new(start, end)
    });

    (annotation, syntax)
}
