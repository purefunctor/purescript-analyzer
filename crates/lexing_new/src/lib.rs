mod lexed;
mod lexer;
mod categories;

pub use lexed::Lexed;

pub fn lex(source: &str) -> Lexed {
    let mut lexer = lexer::Lexer::new(source);
    while !lexer.is_eof() {
        lexer.take_token();
    }
    lexer.finish()
}
