mod grammar;
mod layout;

#[cfg(test)]
mod tests;

use position::Position;
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

pub struct Tokenized<'s> {
    source: &'s str,
    tokens: Vec<Token>,
    positions: Vec<Position>,
}

impl<'s> Tokenized<'s> {
    fn new(source: &'s str, tokens: Vec<Token>) -> Tokenized<'s> {
        let positions = Tokenized::compute_positions(source, &tokens);
        Tokenized { source, tokens, positions }
    }

    fn compute_positions(source: &'s str, tokens: &[Token]) -> Vec<Position> {
        let mut positions = Vec::with_capacity(tokens.len());

        let mut index = 0;
        let mut offset = 0;
        let mut line = 1;
        let mut column = 1;

        for c in source.chars() {
            if offset >= tokens[index].offset {
                index += 1;
                positions.push(Position { offset: offset as u32, line, column });
            }
            offset += c.len_utf8();
            if c == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        positions.push(Position { offset: offset as u32, line, column });

        assert!(tokens.len() == positions.len());

        positions
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn kind(&self, index: usize) -> SyntaxKind {
        assert!(index < self.tokens.len());
        self.tokens[index].kind
    }

    pub fn text(&self, index: usize) -> &'s str {
        assert!(index < self.tokens.len());
        let start = self.tokens[index].offset;
        let end = self.tokens[index + 1].offset;
        &self.source[start..end]
    }

    pub fn position(&self, index: usize) -> Position {
        assert!(index < self.positions.len());
        self.positions[index]
    }
}

pub fn tokenize<'s>(source: &'s str) -> (Tokenized<'s>, Vec<SyntaxKind>) {
    let mut input = winnow::Located::new(source);

    let tokens = grammar::tokens(&mut input).unwrap();
    let tokenized = Tokenized::new(source, tokens);
    let tokens = layout::layout(&tokenized);

    (tokenized, tokens)
}
