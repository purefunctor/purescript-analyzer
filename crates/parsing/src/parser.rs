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
}

impl Parser {
    fn start_layout(&mut self, kind: LayoutKind) {
        assert!(!self.is_eof());
        let position = self.input.position(self.index);
        self.layouts.push(Layout::new(kind, position));
    }

    fn finish_layout(&mut self) {
        self.layouts.pop();
    }

    fn at_layout_end(&self) -> bool {
        if self.is_eof() {
            return true;
        }

        let position = self.input.position(self.index);
        let layout = self.layouts.last().unwrap();

        let after_layout_line = position.line > layout.position.line;
        let before_layout_column = position.column < layout.position.column;

        after_layout_line && before_layout_column
    }

    fn at_layout_sep(&self) -> bool {
        if self.is_eof() {
            return true;
        }

        let position = self.input.position(self.index);
        let layout = self.layouts.last().unwrap();

        let after_layout_line = position.line > layout.position.line;
        let before_layout_column = position.column <= layout.position.column;

        after_layout_line && before_layout_column
    }
}

impl Parser {
    fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::Start { kind });
    }

    fn finish_node(&mut self) {
        self.events.push(Event::Finish);
    }
}

impl Parser {
    fn is_eof(&self) -> bool {
        self.index == self.input.len()
    }

    fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    fn nth(&self, offset: usize) -> SyntaxKind {
        self.input.kind(self.index + offset)
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    fn nth_at(&self, offset: usize, kind: SyntaxKind) -> bool {
        self.nth(offset) == kind
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.index += 1;
        self.events.push(Event::Token { kind });
        true
    }

    fn bump_any(&mut self) {
        let kind = self.current();
        self.index += 1;
        self.events.push(Event::Token { kind });
    }
}

#[cfg(test)]
mod tests {
    use syntax::SyntaxKind::{self, *};

    use crate::{layout::LayoutKind, lexer::lex};

    use super::Parser;

    fn parse_module(parser: &mut Parser) {
        parser.start_node(Module);
        parser.eat(ModuleKw);
        parse_module_name(parser);
        parser.eat(WhereKw);

        parser.start_layout(LayoutKind::Module);
        loop {
            parse_value_declaration(parser);
            if parser.at_layout_end() {
                break;
            }
        }
        parser.finish_layout();
        parser.finish_node();
    }

    fn parse_module_name(parser: &mut Parser) {
        parser.eat(SyntaxKind::Upper);
    }

    fn parse_value_declaration(parser: &mut Parser) {
        parser.start_node(ValueDeclaration);
        loop {
            parser.bump_any();
            if parser.at_layout_sep() {
                break;
            }
        }
        parser.finish_node();
    }

    #[test]
    fn grammar_api_test() {
        let lexed = lex(r"module Hello where
hello = world
  world = hello

one = two
  two = one
  ");
        let input = lexed.as_input();
        let mut parser = Parser::new(input);
        parse_module(&mut parser);
        dbg!(parser.layouts);
        dbg!(parser.events);
    }
}
