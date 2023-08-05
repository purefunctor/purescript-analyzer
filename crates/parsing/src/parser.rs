use std::cell::Cell;

use drop_bomb::DropBomb;
use syntax::SyntaxKind;

#[derive(Debug, PartialEq, Eq)]
pub enum Event {
    Start { kind: SyntaxKind },
    Token { kind: SyntaxKind },
    Error { message: String },
    Finish,
}

pub struct Parser<'a> {
    input: &'a [SyntaxKind],
    index: usize,
    events: Vec<Event>,
    steps: Cell<u32>,
}

const PARSER_LIMIT: u32 = 10_000_000;

impl<'a> Parser<'a> {
    pub fn new(input: &'a [SyntaxKind]) -> Parser<'a> {
        let index = 0;
        let events = vec![];
        let steps = Cell::new(0);
        Parser { input, index, events, steps }
    }

    pub fn is_eof(&self) -> bool {
        self.index == self.input.len()
    }

    pub fn as_output(self) -> Vec<Event> {
        self.events
    }
}

impl<'a> Parser<'a> {
    /// Starts a new node, returning a [`NodeMarker`].
    pub fn start(&mut self) -> NodeMarker {
        let index = self.events.len();
        self.events.push(Event::Start { kind: SyntaxKind::Sentinel });
        NodeMarker::new(index)
    }
}

pub struct NodeMarker {
    index: usize,
    bomb: DropBomb,
}

impl NodeMarker {
    pub fn new(index: usize) -> NodeMarker {
        let bomb = DropBomb::new("failed to call end or cancel");
        NodeMarker { index, bomb }
    }

    /// Finishes a node's construction.
    pub fn end(&mut self, parser: &mut Parser, kind: SyntaxKind) {
        self.bomb.defuse();
        match &mut parser.events[self.index] {
            Event::Start { kind: sentinel } => {
                *sentinel = kind;
            }
            _ => unreachable!(),
        }
        parser.events.push(Event::Finish);
    }

    /// Cancels a node's construction.
    pub fn cancel(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        if self.index == parser.events.len() - 1 {
            match parser.events.pop() {
                Some(Event::Start { kind: SyntaxKind::Sentinel }) => (),
                _ => unreachable!(),
            }
        }
    }
}

impl<'a> Parser<'a> {
    /// Starts a new node, returning a [`SaveMarker`].
    pub fn save(&mut self) -> SaveMarker {
        let input_index = self.index;
        let event_index = self.events.len();
        self.events.push(Event::Start { kind: SyntaxKind::Sentinel });
        SaveMarker::new(input_index, event_index)
    }
}

pub struct SaveMarker {
    input_index: usize,
    event_index: usize,
    bomb: DropBomb,
}

impl SaveMarker {
    pub fn new(input_index: usize, event_index: usize) -> SaveMarker {
        let bomb = DropBomb::new("failed to call load or delete");
        SaveMarker { input_index, event_index, bomb }
    }

    /// Returns `true` if [`Event::Error`] is emitted after the marker.
    pub fn has_error(&self, parser: &Parser) -> bool {
        parser.events[self.event_index..].iter().any(|event| matches!(event, Event::Error { .. }))
    }

    /// Resets the state of the [`Parser`] to before [`Parser::save`] is called.
    pub fn load(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        parser.index = self.input_index;
        parser.events.truncate(self.event_index);
    }

    /// Ignores the [`SaveMarker`] and retains the state of the [`Parser`].
    pub fn delete(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        if self.event_index == parser.events.len() - 1 {
            match parser.events.pop() {
                Some(Event::Start { kind: SyntaxKind::Sentinel }) => (),
                _ => unreachable!(),
            }
        }
    }
}

impl<'a> Parser<'a> {
    /// Returns the nth token given an `offset`.
    pub fn nth(&self, offset: usize) -> SyntaxKind {
        let steps = self.steps.get();
        if steps > PARSER_LIMIT {
            panic!("infinite loop in parser");
        }
        self.steps.set(steps + 1);
        self.input.get(self.index + offset).cloned().unwrap_or(SyntaxKind::EndOfFile)
    }

    /// Determines if an nth token matches a `kind`.
    pub fn nth_at(&self, offset: usize, kind: SyntaxKind) -> bool {
        self.nth(offset) == kind
    }

    /// Returns the current token.
    pub fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Determines if the current token matches a `kind`.
    pub fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    /// Consumes a token, advancing the parser.
    pub fn consume(&mut self) {
        let kind = self.current();
        self.consume_as(kind);
    }

    /// Consumes a token as a different `kind`.
    pub fn consume_as(&mut self, kind: SyntaxKind) {
        self.index += 1;
        self.steps.set(0);
        self.events.push(Event::Token { kind });
    }

    /// Consumes a token if it matches the `kind`.
    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.consume();
        true
    }

    /// Emit an error with a `message`.
    pub fn error(&mut self, message: impl Into<String>) {
        let message = message.into();
        self.events.push(Event::Error { message })
    }

    pub fn error_recover(&mut self, message: impl Into<String>) {
        let mut error = self.start();
        self.index += 1;
        self.steps.set(0);
        self.error(message);
        error.end(self, SyntaxKind::Error);
    }

    /// Expect to consume a `kind`, emitting an error otherwise.
    pub fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(format!("expected {kind:?}"));
        false
    }
}
