//! A generic parser API. See [`grammar`].
//!
//! [`grammar`]: `crate::grammar`

use drop_bomb::DropBomb;
use syntax::SyntaxKind;

#[derive(Debug)]
pub(crate) enum Event {
    Start { kind: SyntaxKind },
    Token { kind: SyntaxKind },
    Error { message: String },
    Finish,
}

pub(crate) struct Parser<'a> {
    input: &'a [SyntaxKind],
    index: usize,
    output: Vec<Event>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a [SyntaxKind]) -> Parser<'a> {
        let index = 0;
        let output = vec![];
        Parser { input, index, output }
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.index == self.input.len()
    }

    pub(crate) fn output(self) -> Vec<Event> {
        self.output
    }

    pub(crate) fn nth(&self, offset: usize) -> SyntaxKind {
        self.input.get(self.index + offset).cloned().unwrap_or(SyntaxKind::EndOfFile)
    }

    pub(crate) fn nth_at(&self, offset: usize, kind: SyntaxKind) -> bool {
        self.nth(offset) == kind
    }

    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    pub(crate) fn consume(&mut self) {
        let kind = self.current();
        self.index += 1;
        self.output.push(Event::Token { kind })
    }

    pub(crate) fn consume_as(&mut self, kind: SyntaxKind) {
        self.index += 1;
        self.output.push(Event::Token { kind });
    }

    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.consume();
        true
    }

    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind));
    }

    pub(crate) fn start(&mut self) -> NodeMarker {
        let index = self.output.len();
        self.output.push(Event::Start { kind: SyntaxKind::Sentinel });
        NodeMarker::new(index)
    }

    pub(crate) fn save(&mut self) -> SaveMarker {
        let input_index = self.index;
        let output_index = self.output.len();
        self.output.push(Event::Start { kind: SyntaxKind::Sentinel });
        SaveMarker::new(input_index, output_index)
    }

    pub(crate) fn error(&mut self, message: impl Into<String>) {
        let message = message.into();
        self.output.push(Event::Error { message })
    }
}

pub(crate) struct NodeMarker {
    index: usize,
    bomb: DropBomb,
}

impl NodeMarker {
    pub fn new(index: usize) -> NodeMarker {
        let bomb = DropBomb::new("NodeMarker must be finished or canceled");
        NodeMarker { index, bomb }
    }

    pub(crate) fn finish(&mut self, parser: &mut Parser, kind: SyntaxKind) {
        self.bomb.defuse();
        match &mut parser.output[self.index] {
            Event::Start { kind: sentinel } => {
                *sentinel = kind;
            }
            _ => unreachable!(),
        }
        parser.output.push(Event::Finish);
    }

    pub(crate) fn cancel(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        if self.index == parser.output.len() - 1 {
            match parser.output.pop() {
                Some(Event::Start { kind: SyntaxKind::Sentinel }) => (),
                _ => unreachable!(),
            }
        }
    }
}

pub struct SaveMarker {
    input_index: usize,
    output_index: usize,
    bomb: DropBomb,
}

impl SaveMarker {
    pub(crate) fn new(input_index: usize, output_index: usize) -> SaveMarker {
        let bomb = DropBomb::new("SaveMarker must be loaded or deleted");
        SaveMarker { input_index, output_index, bomb }
    }

    pub(crate) fn has_error(&self, parser: &Parser) -> bool {
        parser.output[self.output_index..].iter().any(|event| matches!(event, Event::Error { .. }))
    }

    pub(crate) fn load(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        parser.index = self.input_index;
        parser.output.truncate(self.output_index);
    }

    pub(crate) fn delete(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        if self.output_index == parser.output.len() - 1 {
            match parser.output.pop() {
                Some(Event::Start { kind: SyntaxKind::Sentinel }) => (),
                _ => unreachable!(),
            }
        }
    }
}
