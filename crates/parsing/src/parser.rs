use syntax::SyntaxKind;

use crate::{
    input::Input,
    layout::{Layout, LayoutKind},
    position::Position,
};

#[derive(Debug)]
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
        let layout = vec![Layout {
            kind: LayoutKind::Root,
            position: Position { offset: 0, line: 1, column: 1 },
        }];
        let events = vec![];
        Parser { input, index, layouts: layout, events }
    }

    pub fn is_eof(&self) -> bool {
        self.index == self.input.len()
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

    /// Determines if the current token belongs to the next group of tokens or layout context.
    pub fn layout_or_group_done(&self) -> bool {
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
            LayoutKind::Instance => position.column <= layout.position.column,
            // NOTE: handled by is_eof
            LayoutKind::Parenthesis => false,
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

    /// Consumes a token if it matches the `kind`.
    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.consume();
        true
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
            if parser.layout_or_group_done() {
                break;
            }
        }
        parser.layout_end();
    }

    fn parse_module_name(parser: &mut Parser) {
        parser.eat(SyntaxKind::Upper);
    }

    fn parse_value_declaration(parser: &mut Parser) {
        loop {
            if parser.at(SyntaxKind::LeftParenthesis) {
                parser.layout_start(LayoutKind::Parenthesis);
            }
            if parser.at(SyntaxKind::RightParenthesis) {
                parser.layout_end();
            }
            parser.consume();
            if parser.layout_or_group_done() {
                break;
            }
        }
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
