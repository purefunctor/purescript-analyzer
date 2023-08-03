//! Text-based lexer into a sequence of [`SyntaxKind`]s.
//!
//! Tokens are based off of the compiler's [`lexer`], while the lexer
//! core borrows from [`rustc_lexer`] and [`rust-analyzer`].
//!
//! [`lexer`]: https://github.com/purescript/purescript/blob/master/src/Language/PureScript/CST/Lexer.hs
//! [`rustc_lexer`]: https://doc.rust-lang.org/stable/nightly-rustc/rustc_lexer/
//! [`rust-analyzer`]: https://github.com/rust-lang/rust-analyzer/
mod lexed;
mod lexer;
mod position;

use lexed::Lexed;
use lexer::Lexer;

/// Lexes a `&str` into [`Lexed`].
pub fn lex(source: &str) -> Lexed {
    let mut lexer = Lexer::new(source);
    loop {
        if lexer.is_eof() {
            break lexer.finalize();
        }
        lexer.take_token();
    }
}
