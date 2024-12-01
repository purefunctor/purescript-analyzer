mod layout;
mod rules;

#[cfg(test)]
mod tests;

use winnow::{Located, PResult};

use syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lexed {
    pub kind: SyntaxKind,
    pub offset: usize,
}

impl Lexed {
    fn new(kind: SyntaxKind, offset: usize) -> Lexed {
        Lexed { kind, offset }
    }
}

type Input<'s> = Located<&'s str>;

pub fn tokenize(source: &str) -> PResult<Vec<Lexed>> {
    let mut input = Located::new(source);
    rules::tokens(&mut input)
}
