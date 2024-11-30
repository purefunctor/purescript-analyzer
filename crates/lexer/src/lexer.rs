mod rules;

#[cfg(test)]
mod tests;

use winnow::{Located, PResult};

use crate::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lexed {
    pub kind: LexedKind,
    pub offset: usize,
}

pub type Prefix = Option<(SyntaxKind, usize)>;

impl Lexed {
    fn token(kind: SyntaxKind, offset: usize) -> Lexed {
        Lexed { kind: LexedKind::Token(kind), offset }
    }

    fn qualified(prefix: Prefix, kind: SyntaxKind, offset: usize) -> Lexed {
        Lexed { kind: LexedKind::Qualified(prefix, kind), offset }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LexedKind {
    Token(SyntaxKind),
    Qualified(Prefix, SyntaxKind),
}

type Input<'s> = Located<&'s str>;

pub fn tokenize(source: &str) -> PResult<Vec<Lexed>> {
    let mut input = Located::new(source);
    rules::tokens(&mut input)
}
