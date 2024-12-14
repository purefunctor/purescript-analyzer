//! A lexer for PureScript source code.

mod layout;
mod lexed;
mod lexer;

pub use lexed::Lexed;

use layout::Layout;
use lexer::Lexer;
use syntax::SyntaxKind;

/// Tokenizes a source string.
pub fn lex(source: &str) -> Lexed {
    let mut lexer = Lexer::new(source);
    while !lexer.is_eof() {
        lexer.take_token();
    }
    lexer.finish()
}

/// Applies the layout algorithm.
pub fn layout(lexed: &Lexed) -> Vec<SyntaxKind> {
    let mut layout = Layout::new(lexed);
    while !layout.is_eof() {
        layout.take_token();
    }
    layout.finish()
}
