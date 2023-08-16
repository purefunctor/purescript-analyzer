//! Implements a generic parser API.

use std::cell::Cell;

use drop_bomb::DropBomb;
use syntax::SyntaxKind;

#[derive(Debug, PartialEq, Eq)]
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
    steps: Cell<u32>,
}

const PARSER_LIMIT: u32 = 10_000_000;

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a [SyntaxKind]) -> Parser<'a> {
        let index = 0;
        let output = vec![];
        let steps = Cell::new(0);
        Parser { input, index, output, steps }
    }

    pub(crate) fn finalize(self) -> Vec<Event> {
        self.output
    }

    pub(crate) fn nth(&self, offset: usize) -> SyntaxKind {
        let steps = self.steps.get();
        if steps > PARSER_LIMIT {
            panic!("infinite loop in parser");
        }
        self.steps.set(steps + 1);
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
        self.consume_as(kind);
    }

    pub(crate) fn consume_as(&mut self, kind: SyntaxKind) {
        self.index += 1;
        self.steps.set(0);
        self.output.push(Event::Token { kind });
    }

    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.consume();
        true
    }

    pub(crate) fn error(&mut self, message: impl Into<String>) {
        let message = message.into();
        self.output.push(Event::Error { message })
    }

    pub(crate) fn error_recover(&mut self, message: impl Into<String>) {
        let mut error = self.start();
        self.index += 1;
        self.steps.set(0);
        self.error(message);
        error.end(self, SyntaxKind::Error);
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(format!("expected {kind:?}"));
        false
    }

    pub(crate) fn start(&mut self) -> NodeMarker {
        let index = self.output.len();
        self.output.push(Event::Start { kind: SyntaxKind::Sentinel });
        NodeMarker::new(index)
    }

    pub(crate) fn save(&mut self) -> SaveMarker {
        let input_index = self.index;
        let event_index = self.output.len();
        self.output.push(Event::Start { kind: SyntaxKind::Sentinel });
        SaveMarker::new(input_index, event_index)
    }
}

pub struct NodeMarker {
    index: usize,
    bomb: DropBomb,
}

impl NodeMarker {
    pub(crate) fn new(index: usize) -> NodeMarker {
        let bomb = DropBomb::new("failed to call end or cancel");
        NodeMarker { index, bomb }
    }

    pub(crate) fn end(&mut self, parser: &mut Parser, kind: SyntaxKind) {
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
    event_index: usize,
    bomb: DropBomb,
}

impl SaveMarker {
    pub(crate) fn new(input_index: usize, event_index: usize) -> SaveMarker {
        let bomb = DropBomb::new("failed to call load or delete");
        SaveMarker { input_index, event_index, bomb }
    }

    pub(crate) fn has_error(&self, parser: &Parser) -> bool {
        parser.output[self.event_index..].iter().any(|event| matches!(event, Event::Error { .. }))
    }

    pub(crate) fn load(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        parser.index = self.input_index;
        parser.output.truncate(self.event_index);
    }

    pub(crate) fn delete(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        if self.event_index == parser.output.len() - 1 {
            match parser.output.pop() {
                Some(Event::Start { kind: SyntaxKind::Sentinel }) => (),
                _ => unreachable!(),
            }
        }
    }
}
