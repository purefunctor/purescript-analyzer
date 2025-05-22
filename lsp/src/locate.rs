//! Abstractions for identifying syntax at a given location.

use files::FileId;
use line_index::{LineCol, LineIndex};
use lowering::{BinderId, ExpressionId, FullLoweredModule, TypeId};
use parsing::ParsedModule;
use rowan::{
    TextSize, TokenAtOffset,
    ast::{AstNode, AstPtr, SyntaxNodePtr},
};
use syntax::{SyntaxNode, SyntaxToken, cst};
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
pub enum Thing {
    Annotation(AstPtr<cst::Annotation>),
    Binder(AstPtr<cst::Binder>),
    Expression(AstPtr<cst::Expression>),
    Type(AstPtr<cst::Type>),
    Nothing,
}

pub fn thing_at_position(content: &str, parsed: &ParsedModule, position: Position) -> Thing {
    let Some(offset) = position_to_offset(content, position) else {
        return Thing::Nothing;
    };

    let node = parsed.clone().syntax_node();
    let token = node.token_at_offset(offset);

    match token {
        TokenAtOffset::None => Thing::Nothing,
        TokenAtOffset::Single(token) => thing_from_single(token),
        TokenAtOffset::Between(left, right) => thing_from_between(left, right),
    }
}

fn thing_from_single(token: SyntaxToken) -> Thing {
    token.parent_ancestors().find_map(thing_classify_node).unwrap_or(Thing::Nothing)
}

fn thing_classify_node(node: SyntaxNode) -> Option<Thing> {
    let kind = node.kind();
    let ptr = SyntaxNodePtr::new(&node);
    if cst::Annotation::can_cast(kind) {
        let ptr = ptr.cast()?;
        Some(Thing::Annotation(ptr))
    } else if cst::Binder::can_cast(kind) {
        let ptr = ptr.cast()?;
        Some(Thing::Binder(ptr))
    } else if cst::Expression::can_cast(kind) {
        let ptr = ptr.cast()?;
        Some(Thing::Expression(ptr))
    } else if cst::Type::can_cast(kind) {
        let ptr = ptr.cast()?;
        Some(Thing::Type(ptr))
    } else {
        None
    }
}

fn thing_from_between(left: SyntaxToken, right: SyntaxToken) -> Thing {
    let left = thing_from_single(left);
    let right = thing_from_single(right);
    match (&left, &right) {
        // If left/right share an ancestor;
        (_, _) if left == right => left,
        (_, Thing::Nothing) => left,
        (Thing::Nothing, _) => right,
        // otherwise, lean towards the right.
        (_, _) => right,
    }
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
    token
        .parent_ancestors()
        .find_map(|node| locate_node(&lowered, node))
        .unwrap_or(Located::Nothing)
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
