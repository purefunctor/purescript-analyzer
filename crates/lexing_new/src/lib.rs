mod categories;
mod layout;
mod lexed;
mod lexer;

pub use lexed::Lexed;
use syntax::SyntaxKind;

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

pub fn lex(source: &str) -> Lexed {
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
