//! Abstractions for identifying syntax at a given location.

use line_index::{LineCol, LineIndex};
use parsing::ParsedModule;
use rowan::{
    TextSize, TokenAtOffset,
    ast::{AstNode, AstPtr},
};
use syntax::{SyntaxNode, SyntaxNodePtr, SyntaxToken, cst};
use tower_lsp::lsp_types::*;

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
