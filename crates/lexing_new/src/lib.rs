mod categories;
mod lexed;
mod lexer;

pub use lexed::Lexed;

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
