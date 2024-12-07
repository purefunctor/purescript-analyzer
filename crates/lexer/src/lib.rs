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

struct Lexed<'s> {
    source: &'s str,
    tokens: Vec<Token>,
    positions: Vec<Position>,
}

impl<'s> Lexed<'s> {
    fn new(source: &'s str, tokens: Vec<Token>) -> Lexed<'s> {
        let positions = Lexed::compute_positions(source, &tokens);
        Lexed { source, tokens, positions }
    }

    fn compute_positions(source: &str, tokens: &[Token]) -> Vec<Position> {
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

    pub fn kind(&self, index: usize) -> SyntaxKind {
        assert!(index < self.tokens.len());
        self.tokens[index].kind
    }

    pub fn position(&self, index: usize) -> Position {
        assert!(index < self.positions.len());
        self.positions[index]
    }
}

#[derive(Debug)]
pub struct Positions<'s> {
    pub source: &'s str,
    pub positions: Vec<Position>,
}

impl<'s> Positions<'s> {
    fn from_lexed(lexed: Lexed<'s>, tokens: &[SyntaxKind]) -> Positions<'s> {
        let source = lexed.source;
        let mut positions = Vec::with_capacity(tokens.len());
        let mut old_positions = lexed.positions.into_iter().peekable();

        for kind in tokens {
            let position = if kind.is_layout() {
                *old_positions.peek().expect("a position")
            } else {
                old_positions.next().expect("a position")
            };
            positions.push(position);
        }

        Positions { source, positions }
    }

    pub fn text(&self, index: usize) -> &'s str {
        assert!(index < self.positions.len());
        let start = self.positions[index].offset as usize;
        let end = self.positions[index + 1].offset as usize;
        &self.source[start..end]
    }
}

pub fn tokenize<'s>(source: &'s str) -> (Vec<SyntaxKind>, Positions<'s>) {
    let mut input = winnow::Located::new(source);

    let tokens = grammar::tokens(&mut input).unwrap();
    let lexed = Lexed::new(source, tokens);

    let tokens = layout::layout(&lexed);
    let positions = Positions::from_lexed(lexed, &tokens);

    (tokens, positions)
}
