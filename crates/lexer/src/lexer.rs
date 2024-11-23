mod rules;

#[cfg(test)]
mod tests;

use std::ops::Range;

use winnow::{Located, PResult};

use crate::syntax::SyntaxKind;

#[derive(Debug)]
pub struct Lexed {
    pub kind: LexedKind,
    pub range: Range<usize>,
}

pub type Prefix = Option<(SyntaxKind, Range<usize>)>;

impl Lexed {
    pub fn new(kind: LexedKind, range: Range<usize>) -> Lexed {
        Lexed { kind, range }
    }

    pub fn token(kind: SyntaxKind, range: Range<usize>) -> Lexed {
        Lexed::new(LexedKind::Token(kind), range)
    }

    pub fn qualified(prefix: Prefix, kind: SyntaxKind, range: Range<usize>) -> Lexed {
        Lexed::new(LexedKind::Qualified(prefix, kind), range)
    }
}

#[derive(Debug)]
pub enum LexedKind {
    Token(SyntaxKind),
    Qualified(Prefix, SyntaxKind),
}

type Input<'s> = Located<&'s str>;

pub fn tokenize(source: &str) -> PResult<Vec<Lexed>> {
    let mut input = Located::new(source);
    rules::tokens(&mut input)
}
