//! The input type for the parser.

use syntax::SyntaxKind;

use crate::{lexer::Lexed, position::Position};

pub struct Input {
    tokens: Vec<SyntaxKind>,
    positions: Vec<Position>,
}

impl Input {
    pub fn new() -> Input {
        let tokens = vec![];
        let positions = vec![];
        Input { tokens, positions }
    }

    pub fn push(&mut self, kind: SyntaxKind, position: Position) {
        self.tokens.push(kind);
        self.positions.push(position);
    }

    pub fn kind(&self, index: usize) -> SyntaxKind {
        assert!(index <= self.tokens.len());
        self.tokens[index]
    }

    pub fn position(&self, index: usize) -> Position {
        assert!(index < self.positions.len());
        self.positions[index]
    }
}

impl Lexed<'_> {
    pub fn as_input(&self) -> Input {
        let mut input = Input::new();

        for index in 0..self.len() {
            let kind = self.kind(index);
            let position = self.position(index);

            if kind.is_whitespace_or_comment() {
                continue;
            }

            input.push(kind, position);
        }

        input
    }
}

#[cfg(test)]
mod tests {
    use syntax::SyntaxKind;

    use crate::lexer::lex;

    #[test]
    fn lexed_to_input() {
        let lexed = lex("hello world {- removed -} -- removed");
        let input = lexed.as_input();
        assert_eq!(input.tokens, &[SyntaxKind::Lower, SyntaxKind::Lower]);
    }
}
