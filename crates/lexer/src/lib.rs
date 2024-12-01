mod grammar;
mod layout;

#[cfg(test)]
mod tests;

use syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Token {
    kind: SyntaxKind,
    offset: usize,
}

impl Token {
    fn new(kind: SyntaxKind, offset: usize) -> Token {
        Token { kind, offset }
    }
}

pub fn tokenize(source: &str) -> Vec<SyntaxKind> {
    let mut input = winnow::Located::new(source);
    let tokens = grammar::tokens(&mut input).unwrap();
    let tokens = layout::layout(source, &tokens);
    tokens
}
