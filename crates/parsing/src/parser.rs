use drop_bomb::DropBomb;
use syntax::SyntaxKind;

use crate::builder::Output;

pub(crate) struct Parser<'t> {
    index: usize,
    tokens: &'t [SyntaxKind],
    output: Vec<Output>,
}

impl<'t> Parser<'t> {
    pub(crate) fn new(tokens: &'t [SyntaxKind]) -> Parser<'t> {
        let index = 0;
        let output = vec![];
        Parser { index, tokens, output }
    }

    pub(crate) fn finish(self) -> Vec<Output> {
        self.output
    }

    fn consume(&mut self) {
        let kind = self.tokens[self.index];
        self.index += 1;
        self.output.push(Output::Token { kind });
    }

    fn start(&mut self) -> NodeMarker {
        let index = self.output.len();
        self.output.push(Output::Start { kind: SyntaxKind::Node });
        NodeMarker::new(index)
    }

    fn error(&mut self, message: String) {
        self.output.push(Output::Error { message });
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.tokens.get(self.index) == Some(&kind)
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        self.consume();
        true
    }

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(format!("expected {:?}", kind));
        false
    }
}

struct NodeMarker {
    index: usize,
    bomb: DropBomb,
}

impl NodeMarker {
    fn new(index: usize) -> NodeMarker {
        let bomb = DropBomb::new("failed to call end or cancel");
        NodeMarker { index, bomb }
    }

    fn end(&mut self, parser: &mut Parser, kind: SyntaxKind) {
        self.bomb.defuse();
        match &mut parser.output[self.index] {
            Output::Start { kind: marker } => {
                *marker = kind;
            }
            _ => unreachable!(),
        }
        parser.output.push(Output::Finish);
    }

    // fn cancel(&mut self, parser: &mut Parser) {
    //     self.bomb.defuse();
    //     if self.index == parser.output.len() - 1 {
    //         match parser.output.pop() {
    //             Some(Output::Start { kind: SyntaxKind::Node }) => (),
    //             _ => unreachable!(),
    //         }
    //     }
    // }
}

fn module_name(parser: &mut Parser) {
    let mut marker = parser.start();

    if parser.at(SyntaxKind::PREFIX) {
        parser.consume();
    }
    parser.expect(SyntaxKind::UPPER);

    marker.end(parser, SyntaxKind::ModuleName);
}

pub(crate) fn module(parser: &mut Parser) {
    let mut marker = parser.start();

    module_header(parser);

    marker.end(parser, SyntaxKind::Module);
}

fn module_header(parser: &mut Parser) {
    let mut marker = parser.start();

    parser.eat(SyntaxKind::MODULE);
    module_name(parser);
    parser.eat(SyntaxKind::WHERE);

    marker.end(parser, SyntaxKind::ModuleHeader);
}
