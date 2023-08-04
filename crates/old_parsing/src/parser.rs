use drop_bomb::DropBomb;
use syntax::SyntaxKind;

use crate::{
    input::Input,
    layout::{Layout, LayoutKind},
    position::Position,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Event {
    Start { kind: SyntaxKind },
    Token { kind: SyntaxKind },
    Error { message: String },
    Finish,
}

pub struct Parser {
    input: Input,
    index: usize,

    layouts: Vec<Layout>,
    events: Vec<Event>,
}

impl Parser {
    pub fn new(input: Input) -> Parser {
        let index = 0;
        let layouts = vec![Layout {
            kind: LayoutKind::Root,
            position: Position { offset: 0, line: 1, column: 1 },
        }];
        let events = vec![];
        Parser { input, index, layouts, events }
    }

    pub fn is_eof(&self) -> bool {
        self.index == self.input.len()
    }

    pub fn as_output(self) -> Vec<Event> {
        self.events
    }
}

impl Parser {
    /// Starts a new layout context.
    pub fn layout_start(&mut self, kind: LayoutKind) {
        assert!(!self.is_eof());
        let position = self.input.position(self.index);
        self.layouts.push(Layout::new(kind, position));
    }

    /// Finishes the current layout context.
    pub fn layout_end(&mut self) {
        self.layouts.pop();
    }

    /// Determines if the current token belongs to the next layout context.
    pub fn layout_done(&self) -> bool {
        if self.is_eof() {
            return true;
        }

        let position = self.input.position(self.index);
        let layout = self.layouts.last().unwrap();

        assert!(position.line >= layout.position.line);

        match layout.kind {
            LayoutKind::Root => panic!("Invalid call."),
            // NOTE: handled by is_eof
            LayoutKind::Module => false,
            LayoutKind::Instance => position.column < layout.position.column,
            LayoutKind::Do => position.column < layout.position.column,
            LayoutKind::Let => position.column < layout.position.column,
            LayoutKind::Where => position.column < layout.position.column,
            // NOTE: handled by is_eof
            LayoutKind::Parenthesis => false,
        }
    }

    /// Determines if the current token belongs to the next token group.
    pub fn group_done(&self) -> bool {
        if self.is_eof() {
            return true;
        }

        let position = self.input.position(self.index);
        let layout = self.layouts.last().unwrap();

        assert!(position.line >= layout.position.line);

        match layout.kind {
            LayoutKind::Root => panic!("Invalid call."),
            // NOTE: handled by is_eof
            LayoutKind::Module => position.column == layout.position.column,
            LayoutKind::Instance => position.column <= layout.position.column,
            LayoutKind::Do => position.column <= layout.position.column,
            LayoutKind::Let => position.column <= layout.position.column,
            LayoutKind::Where => position.column <= layout.position.column,
            // NOTE: handled by is_eof
            LayoutKind::Parenthesis => false,
        }
    }
}

impl Parser {
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

impl Parser {
    /// Starts a new node, returning a [`SaveMarker`].
    pub fn save(&mut self) -> SaveMarker {
        let input_index = self.index;
        let layout_index = self.layouts.len();
        let event_index = self.events.len();
        self.events.push(Event::Start { kind: SyntaxKind::Sentinel });
        SaveMarker::new(input_index, layout_index, event_index)
    }
}

pub struct SaveMarker {
    input_index: usize,
    layout_index: usize,
    event_index: usize,
    bomb: DropBomb,
}

impl SaveMarker {
    pub fn new(input_index: usize, layout_index: usize, event_index: usize) -> SaveMarker {
        let bomb = DropBomb::new("failed to call load or delete");
        SaveMarker { input_index, layout_index, event_index, bomb }
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
        parser.layouts.truncate(self.layout_index);
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

impl Parser {
    /// Returns the nth token given an `offset`.
    pub fn nth(&self, offset: usize) -> SyntaxKind {
        self.input.kind(self.index + offset)
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
        self.index += 1;
        self.events.push(Event::Token { kind })
    }

    /// Consumes a token as a different `kind`.
    pub fn consume_as(&mut self, kind: SyntaxKind) {
        self.index += 1;
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

#[cfg(test)]
mod tests {
    use syntax::SyntaxKind::{self, *};

    use crate::{layout::LayoutKind, lexer::lex};

    use super::Parser;

    fn parse_module(parser: &mut Parser) {
        parser.eat(ModuleKw);
        parse_module_name(parser);
        parser.eat(WhereKw);

        parser.layout_start(LayoutKind::Module);
        loop {
            parse_value_declaration(parser);
            if parser.layout_done() {
                break;
            }
        }
        parser.layout_end();
    }

    fn parse_module_name(parser: &mut Parser) {
        parser.eat(SyntaxKind::Upper);
    }

    fn parse_value_declaration(parser: &mut Parser) {
        let mut marker = parser.start();
        loop {
            if parser.at(SyntaxKind::LeftParenthesis) {
                parser.layout_start(LayoutKind::Parenthesis);
            }
            if parser.at(SyntaxKind::RightParenthesis) {
                parser.layout_end();
            }
            parser.consume();
            if parser.group_done() {
                break;
            }
        }
        marker.end(parser, SyntaxKind::ValueDeclaration);
    }

    #[test]
    fn grammar_api_test() {
        let lexed = lex(r"module Hello where
hello = world
  0 'a' 1.2

hello = (
world
0 
'a' 
1.2
)
  ");
        let input = lexed.as_input();
        let mut parser = Parser::new(input);
        parse_module(&mut parser);
        dbg!(parser.layouts);
        dbg!(parser.events);
    }
}
