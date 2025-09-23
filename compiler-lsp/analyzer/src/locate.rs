//! Abstractions for identifying syntax at a given location.

use async_lsp::lsp_types::*;
use building::QueryEngine;
use files::FileId;
use indexing::{ImportItemId, IndexedModule, TermItemId, TypeItemId};
use line_index::{LineCol, LineIndex};
use lowering::{BinderId, ExpressionId, TermOperatorId, TypeId, TypeOperatorId};
use rowan::{
    TextRange, TextSize, TokenAtOffset,
    ast::{AstNode, AstPtr},
};
use stabilizing::StabilizedModule;
use syntax::{SyntaxNode, SyntaxNodePtr, SyntaxToken, cst};

use crate::{AnalyzerError, extract::AnnotationSyntaxRange};

pub fn position_to_offset(content: &str, position: Position) -> Option<TextSize> {
    let line_index = LineIndex::new(content);

    let line_range = line_index.line(position.line)?;
    let line_content = content[line_range].trim_end_matches(['\n', '\r']);

    let line = position.line;
    let col = if line_content.is_empty() {
        0
    } else {
        let last_column = || line_content.chars().count() as u32;
        line_content
            .char_indices()
            .nth(position.character as usize)
            .map(|(column, _)| column as u32)
            .unwrap_or_else(last_column)
    };

    let line_col = LineCol { line, col };
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

pub fn syntax_range(content: &str, root: &SyntaxNode, ptr: &SyntaxNodePtr) -> Option<Range> {
    let range = AnnotationSyntaxRange::from_ptr(root, ptr);
    range.syntax.map(|range| text_range_to_range(content, range))
}

type ModuleNamePtr = AstPtr<cst::ModuleName>;

#[derive(Debug, PartialEq, Eq)]
pub enum Located {
    ModuleName(ModuleNamePtr),
    ImportItem(ImportItemId),
    Binder(BinderId),
    Expression(ExpressionId),
    Type(TypeId),
    TermOperator(TermOperatorId),
    TypeOperator(TypeOperatorId),
    TermItem(TermItemId),
    TypeItem(TypeItemId),
    Nothing,
}

pub fn locate(
    engine: &QueryEngine,
    id: FileId,
    position: Position,
) -> Result<Located, AnalyzerError> {
    let content = engine.content(id);

    let (parsed, _) = engine.parsed(id)?;
    let stabilized = engine.stabilized(id)?;
    let indexed = engine.indexed(id)?;

    let Some(offset) = position_to_offset(&content, position) else {
        return Ok(Located::Nothing);
    };

    let node = parsed.syntax_node();
    let token = node.token_at_offset(offset);

    Ok(match token {
        TokenAtOffset::None => Located::Nothing,
        TokenAtOffset::Single(token) => locate_single(&stabilized, &indexed, token),
        TokenAtOffset::Between(left, right) => locate_between(&stabilized, &indexed, left, right),
    })
}

fn locate_single(
    stabilized: &StabilizedModule,
    indexed: &IndexedModule,
    token: SyntaxToken,
) -> Located {
    token
        .parent_ancestors()
        .find_map(|node| locate_node(stabilized, indexed, node))
        .unwrap_or(Located::Nothing)
}

fn locate_node(
    stabilized: &StabilizedModule,
    indexed: &IndexedModule,
    node: SyntaxNode,
) -> Option<Located> {
    let kind = node.kind();
    let ptr = SyntaxNodePtr::new(&node);
    if cst::Annotation::can_cast(kind) {
        Some(Located::Nothing)
    } else if cst::ModuleName::can_cast(kind) {
        let ptr = ptr.cast()?;
        Some(Located::ModuleName(ptr))
    } else if cst::ImportItem::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        Some(Located::ImportItem(id))
    } else if cst::Binder::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        Some(Located::Binder(id))
    } else if cst::Expression::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        Some(Located::Expression(id))
    } else if cst::Type::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        Some(Located::Type(id))
    } else if cst::TermOperator::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        Some(Located::TermOperator(id))
    } else if cst::TypeOperator::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        Some(Located::TypeOperator(id))
    } else if cst::Declaration::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        None.or_else(|| indexed.pairs.declaration_to_term(id).map(Located::TermItem))
            .or_else(|| indexed.pairs.declaration_to_type(id).map(Located::TypeItem))
    } else if cst::DataConstructor::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        let id = indexed.pairs.constructor_to_term(id)?;
        Some(Located::TermItem(id))
    } else if cst::ClassMemberStatement::can_cast(kind) {
        let ptr = ptr.cast()?;
        let id = stabilized.lookup_ptr(&ptr)?;
        let id = indexed.pairs.class_member_to_term(id)?;
        Some(Located::TermItem(id))
    } else {
        None
    }
}

fn locate_between(
    stabilized: &StabilizedModule,
    indexed: &IndexedModule,
    left: SyntaxToken,
    right: SyntaxToken,
) -> Located {
    let left = locate_single(stabilized, indexed, left);
    let right = locate_single(stabilized, indexed, right);
    match (&left, &right) {
        // If left/right share an ancestor;
        (_, _) if left == right => left,
        (_, Located::Nothing) => left,
        (Located::Nothing, _) => right,
        // otherwise, lean towards the right.
        (_, _) => right,
    }
}

#[cfg(test)]
mod tests {
    use async_lsp::lsp_types::Position;
    use rowan::TextSize;

    use super::position_to_offset;

    #[test]
    fn zero_on_blank_line() {
        let content = "";
        let position = Position::new(0, 0);

        let offset = position_to_offset(content, position);
        assert_eq!(offset, Some(TextSize::new(0)));
    }

    #[test]
    fn zero_or_lf_line() {
        let content = "\n";
        let position = Position::new(0, 0);

        let offset = position_to_offset(content, position);
        assert_eq!(offset, Some(TextSize::new(0)));
    }

    #[test]
    fn zero_or_crlf_line() {
        let content = "\r\n";
        let position = Position::new(0, 0);

        let offset = position_to_offset(content, position);
        assert_eq!(offset, Some(TextSize::new(0)));
    }

    #[test]
    fn last_on_line() {
        let content = "abcdef";
        let position = Position::new(0, 6);

        let offset = position_to_offset(content, position);
        assert_eq!(offset, Some(TextSize::new(6)));
    }

    #[test]
    fn last_on_line_clamp() {
        let content = "abcdef";
        let position = Position::new(0, 600);

        let offset = position_to_offset(content, position);
        assert_eq!(offset, Some(TextSize::new(6)));
    }

    #[test]
    fn last_on_lf_line() {
        let content = "abcdef\n";
        let position = Position::new(0, 6);

        let offset = position_to_offset(content, position);
        assert_eq!(offset, Some(TextSize::new(6)));
    }

    #[test]
    fn last_on_crlf_line_clamp() {
        let content = "abcdef\r\n";
        let position = Position::new(0, 600);

        let offset = position_to_offset(content, position);
        assert_eq!(offset, Some(TextSize::new(6)));
    }
}
