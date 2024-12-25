mod types;

use std::sync::Arc;

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

    fn error(&mut self, message: impl Into<Arc<str>>) {
        self.output.push(Output::Error { message: message.into() });
    }

    fn error_recover(&mut self, message: impl Into<Arc<str>>) {
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
    let mut m = p.start();

    if p.at(SyntaxKind::PREFIX) {
        p.consume();
    }
    p.expect(SyntaxKind::UPPER);

    m.end(p, SyntaxKind::ModuleName);
}

pub(crate) fn module(p: &mut Parser) {
    let mut m = p.start();

    module_header(p);
    p.expect(SyntaxKind::LAYOUT_START);
    module_statements(p);
    p.expect(SyntaxKind::LAYOUT_END);
    p.expect(SyntaxKind::END_OF_FILE);

    m.end(p, SyntaxKind::Module);
}

fn module_header(p: &mut Parser) {
    let mut m = p.start();

    p.expect(SyntaxKind::MODULE);
    module_name(p);
    module_export_list(p);
    p.expect(SyntaxKind::WHERE);

    m.end(p, SyntaxKind::ModuleHeader);
}

fn module_export_list(p: &mut Parser) {
    let mut m = p.start();

    'list: {
        if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
            if p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                p.error("Empty export list");
                p.consume();
                break 'list;
            }
            while !p.at(SyntaxKind::RIGHT_PARENTHESIS) && !p.at_eof() {
                if p.at_in(EXPORT_ITEM_START) {
                    module_export_item(p);
                    if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_PARENTHESIS) {
                        p.error_recover("Trailing comma");
                    } else if !p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                        p.expect(SyntaxKind::COMMA);
                    }
                } else {
                    if p.at_in(EXPORT_LIST_RECOVERY) {
                        break;
                    }
                    p.error_recover("Invalid token");
                }
            }
            p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        }
    }

    m.end(p, SyntaxKind::ModuleExportList);
}

const EXPORT_ITEM_START: TokenSet = TokenSet::new(&[
    SyntaxKind::CLASS,
    SyntaxKind::LOWER,
    SyntaxKind::MODULE,
    SyntaxKind::TYPE,
    SyntaxKind::UPPER,
    SyntaxKind::LEFT_PARENTHESIS,
]);

const EXPORT_LIST_RECOVERY: TokenSet = TokenSet::new(&[SyntaxKind::WHERE]);

fn module_export_item(p: &mut Parser) {
    let mut m = p.start();

    if p.eat(SyntaxKind::LOWER) {
        m.end(p, SyntaxKind::ModuleExportValue);
    } else if p.eat(SyntaxKind::UPPER) {
        type_items(p);
        m.end(p, SyntaxKind::ModuleExportType);
    } else if p.eat(SyntaxKind::CLASS) {
        p.expect(SyntaxKind::UPPER);
        m.end(p, SyntaxKind::ModuleExportClass);
    } else if p.eat(SyntaxKind::TYPE) {
        p.expect(SyntaxKind::LEFT_PARENTHESIS);
        // Make sure we don't accidentally consume
        // the ')' used to close the export list
        if p.expect(SyntaxKind::OPERATOR) {
            p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        }
        m.end(p, SyntaxKind::ModuleExportTypeOperator);
    } else if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
        p.expect(SyntaxKind::OPERATOR);
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        m.end(p, SyntaxKind::ModuleExportOperator);
    } else if p.eat(SyntaxKind::MODULE) {
        module_name(p);
        m.end(p, SyntaxKind::ModuleExportModule);
    } else {
        m.cancel(p);
    }
}

fn type_items(p: &mut Parser) {
    let mut m = p.start();

    let mut kind = SyntaxKind::ModuleExportTypeItemsList;
    'list: {
        if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
            if p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                p.error("Empty item list");
                p.consume();
                break 'list;
            }
            if p.eat(SyntaxKind::DOUBLE_PERIOD) {
                kind = SyntaxKind::ModuleExportTypeItemsAll;
                while !p.at(SyntaxKind::RIGHT_PARENTHESIS) && !p.at_eof() {
                    if p.at_in(EXPORT_LIST_RECOVERY) {
                        break;
                    }
                    p.error_recover("Invalid token");
                }
            } else {
                while !p.at(SyntaxKind::RIGHT_PARENTHESIS) && !p.at_eof() {
                    if p.eat(SyntaxKind::UPPER) {
                        if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_PARENTHESIS) {
                            p.error_recover("Trailing comma");
                        } else if !p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                            p.expect(SyntaxKind::COMMA);
                        }
                    } else {
                        if p.at_in(EXPORT_LIST_RECOVERY) {
                            break;
                        }
                        p.error_recover("Invalid token");
                    }
                }
            }
            p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        };
    }

    m.end(p, kind);
}

fn module_statements(p: &mut Parser) {
    let mut imports = p.start();

    while p.at(SyntaxKind::IMPORT) && !p.at_eof() {
        import_statement(p);
        while !p.at(SyntaxKind::LAYOUT_SEPARATOR) && !p.at(SyntaxKind::LAYOUT_END) && !p.at_eof() {
            p.error_recover("Invalid token");
        }
        if !p.at(SyntaxKind::LAYOUT_END) {
            p.expect(SyntaxKind::LAYOUT_SEPARATOR);
        }
    }

    imports.end(p, SyntaxKind::ModuleImports);

    let mut statements = p.start();

    while p.at_in(MODULE_STATEMENT_START) && !p.at_eof() {
        module_statement(p);
        while !p.at(SyntaxKind::LAYOUT_SEPARATOR) && !p.at(SyntaxKind::LAYOUT_END) && !p.at_eof() {
            p.error_recover("Invalid token");
        }
        if !p.at(SyntaxKind::LAYOUT_END) {
            p.expect(SyntaxKind::LAYOUT_SEPARATOR);
        }
    }

    statements.end(p, SyntaxKind::ModuleStatements);
}

const MODULE_STATEMENT_START: TokenSet = TokenSet::new(&[
    SyntaxKind::CLASS,
    SyntaxKind::DATA,
    SyntaxKind::LOWER,
    SyntaxKind::TYPE,
    SyntaxKind::INFIX,
    SyntaxKind::INFIXL,
    SyntaxKind::INFIXR,
    SyntaxKind::INSTANCE,
    SyntaxKind::NEWTYPE,
    SyntaxKind::FOREIGN,
]);

fn import_statement(p: &mut Parser) {
    let mut m = p.start();

    p.expect(SyntaxKind::IMPORT);
    module_name(p);
    import_list(p);
    import_alias(p);

    m.end(p, SyntaxKind::ModuleImportStatement);
}

fn import_list(p: &mut Parser) {
    let mut m = p.start();

    p.eat(SyntaxKind::HIDING);
    'list: {
        if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
            if p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                p.error("Empty export list");
                p.consume();
                break 'list;
            }
            while !p.at(SyntaxKind::RIGHT_PARENTHESIS) && !p.at_eof() {
                if p.at_in(IMPORT_ITEM_START) {
                    import_item(p);
                    if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_PARENTHESIS) {
                        p.error_recover("Trailing comma");
                    } else if !p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                        p.expect(SyntaxKind::COMMA);
                    }
                } else {
                    if p.at_in(IMPORT_LIST_RECOVERY) {
                        break;
                    }
                    p.error_recover("Invalid token");
                }
            }
            p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        }
    }

    m.end(p, SyntaxKind::ModuleImportList);
}

const IMPORT_ITEM_START: TokenSet = TokenSet::new(&[
    SyntaxKind::LOWER,
    SyntaxKind::UPPER,
    SyntaxKind::CLASS,
    SyntaxKind::TYPE,
    SyntaxKind::LEFT_PARENTHESIS,
]);

const IMPORT_LIST_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

fn import_item(p: &mut Parser) {
    let mut m = p.start();

    if p.eat(SyntaxKind::LOWER) {
        m.end(p, SyntaxKind::ModuleImportValue);
    } else if p.eat(SyntaxKind::UPPER) {
        type_items(p);
        m.end(p, SyntaxKind::ModuleImportType);
    } else if p.eat(SyntaxKind::CLASS) {
        p.expect(SyntaxKind::UPPER);
        m.end(p, SyntaxKind::ModuleImportClass);
    } else if p.eat(SyntaxKind::TYPE) {
        p.expect(SyntaxKind::LEFT_PARENTHESIS);
        // Make sure we don't accidentally consume
        // the ')' used to close the export list
        if p.expect(SyntaxKind::OPERATOR) {
            p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        }
        m.end(p, SyntaxKind::ModuleImportTypeOperator);
    } else if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
        p.expect(SyntaxKind::OPERATOR);
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        m.end(p, SyntaxKind::ModuleImportTypeOperator);
    } else {
        m.cancel(p);
    }
}

fn import_alias(p: &mut Parser) {
    let mut m = p.start();

    if p.eat(SyntaxKind::AS) {
        module_name(p);
    }

    m.end(p, SyntaxKind::ModuleImportAlias);
}

fn module_statement(p: &mut Parser) {
    if p.at(SyntaxKind::LOWER) {
        value_annotation_or_equation(p);
    }
}

fn value_annotation_or_equation(p: &mut Parser) {
    let m = p.start();
    p.expect(SyntaxKind::LOWER);
    if p.at(SyntaxKind::DOUBLE_COLON) {
        type_annotation(p, m);
    } else {
        value_equation(p, m);
    };
}

fn type_annotation(p: &mut Parser, mut m: NodeMarker) {
    p.expect(SyntaxKind::DOUBLE_COLON);
    types::ty(p);
    m.end(p, SyntaxKind::ValueAnnotation);
}

fn value_equation(p: &mut Parser, mut m: NodeMarker) {
    m.end(p, SyntaxKind::ValueEquation);
}
