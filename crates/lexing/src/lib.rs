//! A lexer for PureScript source code.

mod layout;
mod lexed;
mod lexer;

pub use lexed::Lexed;

use layout::Machine;
use lexer::Lexer;
use syntax::SyntaxKind;

/// Tokenizes a source string.
pub fn lex(source: &str) -> Lexed {
    let mut lexer = Lexer::new(source);
    loop {
        if lexer.is_eof() {
            break lexer.finalize();
        }
        lexer.take_token();
    }
}

/// Applies the layout algorithm.
pub fn layout(lexed: &Lexed) -> Vec<SyntaxKind> {
    let mut machine = Machine::new(lexed);
    loop {
        if machine.is_eof() {
            break machine.finalize();
        }
        machine.take_token();
    }
}
