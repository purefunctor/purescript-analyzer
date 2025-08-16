mod categories;
mod layout;
mod lexed;
mod lexer;

pub use lexed::Lexed;
use syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

pub fn lex(source: &str) -> Lexed<'_> {
    let mut lexer = lexer::Lexer::new(source);
    while !lexer.is_eof() {
        lexer.take_token();
    }
    lexer.finish()
}

pub fn layout(lexed: &Lexed) -> Vec<SyntaxKind> {
    let mut layout = layout::Layout::new(lexed);
    while !layout.is_eof() {
        layout.take_token();
    }
    layout.finish()
}
