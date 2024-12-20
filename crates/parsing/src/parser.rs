use drop_bomb::DropBomb;
use syntax::{SyntaxKind, TokenSet};

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

    fn nth(&self, index: usize) -> SyntaxKind {
        self.tokens.get(self.index + index).copied().unwrap_or(SyntaxKind::END_OF_FILE)
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

    fn error(&mut self, message: impl Into<String>) {
        self.output.push(Output::Error { message: message.into() });
    }

    fn error_recover(&mut self, message: impl Into<String>) {
        let mut marker = self.start();
        self.error(message);
        self.consume();
        marker.end(self, SyntaxKind::ERROR);
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.nth(0) == kind
    }

    fn at_next(&self, kind: SyntaxKind) -> bool {
        self.nth(1) == kind
    }

    fn at_eof(&self) -> bool {
        self.at(SyntaxKind::END_OF_FILE)
    }

    fn at_in(&self, set: TokenSet) -> bool {
        set.contains(self.nth(0))
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

    fn cancel(&mut self, parser: &mut Parser) {
        self.bomb.defuse();
        if self.index == parser.output.len() - 1 {
            match parser.output.pop() {
                Some(Output::Start { kind: SyntaxKind::Node }) => (),
                _ => unreachable!(),
            }
        }
    }
}

fn module_name(p: &mut Parser) {
    let mut marker = p.start();

    if p.at(SyntaxKind::PREFIX) {
        p.consume();
    }
    p.expect(SyntaxKind::UPPER);

    marker.end(p, SyntaxKind::ModuleName);
}

pub(crate) fn module(p: &mut Parser) {
    let mut marker = p.start();

    module_header(p);

    marker.end(p, SyntaxKind::Module);
}

fn module_header(p: &mut Parser) {
    let mut marker = p.start();

    p.eat(SyntaxKind::MODULE);
    module_name(p);
    module_export_list(p);
    p.eat(SyntaxKind::WHERE);

    marker.end(p, SyntaxKind::ModuleHeader);
}

fn module_export_list(p: &mut Parser) {
    let mut marker = p.start();

    'list: {
        if p.at(SyntaxKind::LEFT_PARENTHESIS) {
            p.consume();

            if p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                p.error("Empty export list");
                p.consume();
                break 'list;
            }

            let mut after_item = false;
            while !p.at(SyntaxKind::RIGHT_PARENTHESIS) && !p.at_eof() {
                if p.at_in(EXPORT_ITEM_START) {
                    module_export_item(p);
                    after_item = true;
                    continue;
                }
                if p.at(SyntaxKind::COMMA) {
                    if p.at_next(SyntaxKind::RIGHT_PARENTHESIS) {
                        p.error_recover("Trailing comma in export list");
                        continue;
                    }
                    if after_item {
                        p.consume();
                        after_item = false;
                        continue;
                    }
                    p.error_recover("Missing item in export list");
                    continue;
                }
                if p.at_in(EXPORT_LIST_RECOVERY) {
                    break;
                }
                p.error_recover("Invalid token in export list");
                after_item = true;
            }

            p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        }
    }

    marker.end(p, SyntaxKind::ModuleExportList);
}

const EXPORT_ITEM_START: TokenSet = TokenSet::new(&[
    SyntaxKind::LOWER,
    SyntaxKind::UPPER,
    SyntaxKind::CLASS,
    SyntaxKind::TYPE,
    SyntaxKind::LEFT_PARENTHESIS,
]);

const EXPORT_LIST_RECOVERY: TokenSet = TokenSet::new(&[SyntaxKind::WHERE]);

fn module_export_item(p: &mut Parser) {
    let mut m = p.start();

    let at_export_list_end =
        |p: &Parser| p.at(SyntaxKind::RIGHT_PARENTHESIS) && p.at_next(SyntaxKind::WHERE);

    let recover = |p: &mut Parser, m: &str| {
        let mut r = p.start();
        p.error(m);
        while !at_export_list_end(p) && !p.at_in(EXPORT_ITEM_RECOVERY) && !p.at_eof() {
            p.consume();
        }
        r.end(p, SyntaxKind::ERROR);
    };

    if p.eat(SyntaxKind::LOWER) {
        m.end(p, SyntaxKind::ModuleExportValue);
    } else if p.eat(SyntaxKind::UPPER) {
        if p.at(SyntaxKind::LEFT_PARENTHESIS) {
            type_items(p);
        }
        m.end(p, SyntaxKind::ModuleExportType);
    } else if p.eat(SyntaxKind::CLASS) {
        if p.eat(SyntaxKind::UPPER) {
            return m.end(p, SyntaxKind::ModuleExportClass);
        }
        recover(p, "Expected a class name");
        m.end(p, SyntaxKind::ModuleExportClass);
    } else if p.eat(SyntaxKind::TYPE) {
        todo!("TypeOperator");
    } else if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
        todo!("Operator");
    } else {
        m.cancel(p);
    }
}

const EXPORT_ITEM_RECOVERY: TokenSet = TokenSet::new(&[SyntaxKind::COMMA, SyntaxKind::WHERE]);

fn type_items(p: &mut Parser) {
    let mut m = p.start();

    p.expect(SyntaxKind::LEFT_PARENTHESIS);

    if p.eat(SyntaxKind::DOUBLE_PERIOD) {
        if p.expect(SyntaxKind::RIGHT_PARENTHESIS) {
            return m.end(p, SyntaxKind::ModuleExportTypeItemsAll);
        } else {
            return m.cancel(p);
        }
    } else {
        let mut after_item = false;
        while !p.at(SyntaxKind::RIGHT_PARENTHESIS) && !p.at_eof() {
            if p.eat(SyntaxKind::UPPER) {
                after_item = true;
                continue;
            }
            if p.at(SyntaxKind::COMMA) {
                if p.at_next(SyntaxKind::RIGHT_PARENTHESIS) {
                    p.error_recover("Trailing comma in constructor list");
                    continue;
                }
                if after_item {
                    p.consume();
                    after_item = false;
                    continue;
                }
                p.error_recover("Missing item in constructor list");
                continue;
            }
            p.error_recover("Invalid item in constructor list");
            after_item = true;
        }
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        m.end(p, SyntaxKind::ModuleExportTypeItemsList);
    };
}
