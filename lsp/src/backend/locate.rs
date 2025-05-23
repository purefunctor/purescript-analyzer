//! Abstractions for identifying syntax at a given location.

use files::FileId;
use line_index::{LineCol, LineIndex};
use lowering::{BinderId, ExpressionId, FullLoweredModule, TypeId};
use rowan::{SyntaxText, TextRange, TextSize, TokenAtOffset, ast::AstNode};
use syntax::{SyntaxKind, SyntaxNode, SyntaxNodePtr, SyntaxToken, cst};
use tower_lsp::lsp_types::*;

use crate::Backend;

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

#[derive(Debug, PartialEq, Eq)]
pub enum Located {
    Binder(BinderId),
    Expression(ExpressionId),
    Type(TypeId),
    Nothing,
}

pub fn locate(backend: &Backend, id: FileId, position: Position) -> Located {
    let (content, parsed, lowered) = {
        let mut runtime = backend.runtime.lock().unwrap();
        let content = runtime.content(id);
        let (parsed, _) = runtime.parsed(id);
        let lowered = runtime.lowered(id);
        (content, parsed, lowered)
    };

    let Some(offset) = position_to_offset(&content, position) else {
        return Located::Nothing;
    };

    let node = parsed.syntax_node();
    let token = node.token_at_offset(offset);

    match token {
        TokenAtOffset::None => Located::Nothing,
        TokenAtOffset::Single(token) => locate_single(&lowered, token),
        TokenAtOffset::Between(left, right) => locate_between(&lowered, left, right),
    }
}

fn locate_single(lowered: &FullLoweredModule, token: SyntaxToken) -> Located {
    token.parent_ancestors().find_map(|node| locate_node(lowered, node)).unwrap_or(Located::Nothing)
}

fn locate_node(lowered: &FullLoweredModule, node: SyntaxNode) -> Option<Located> {
    let kind = node.kind();
    let ptr = SyntaxNodePtr::new(&node);
    if cst::Binder::can_cast(kind) {
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
    } else {
        None
    }
}

fn locate_between(lowered: &FullLoweredModule, left: SyntaxToken, right: SyntaxToken) -> Located {
    let left = locate_single(lowered, left);
    let right = locate_single(lowered, right);
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

    let start = offset_to_position(content, range.start());
    let end = offset_to_position(content, range.end());

    Some(Range { start, end })
}

pub fn text_range_after_annotation(ptr: &SyntaxNodePtr, root: &SyntaxNode) -> Option<TextRange> {
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

pub fn annotation_syntax_range(
    root: &SyntaxNode,
    ptr: SyntaxNodePtr,
) -> (Option<TextRange>, Option<TextRange>) {
    let node = ptr.to_node(root);
    let mut children = node.children_with_tokens().peekable();

    let annotation = children
        .next_if(|node| matches!(node.kind(), SyntaxKind::Annotation))
        .map(|node| node.text_range());

    let first = children.next();
    let last = children.last();

    let syntax = first.zip(last).map(|(start, end)| {
        let start = start.text_range().start();
        let end = end.text_range().end();
        TextRange::new(start, end)
    });

    (annotation, syntax)
}
