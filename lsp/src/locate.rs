//! Abstractions for identifying syntax given a location.

use line_index::{LineCol, LineIndex};
use rowan::TextSize;
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
