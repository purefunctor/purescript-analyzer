mod binders;
mod binding;
mod expressions;
mod generic;
mod names;
mod types;

use std::{cell::Cell, mem, sync::Arc};

use drop_bomb::DropBomb;
use names::LOWER_NON_RESERVED;
use syntax::{SyntaxKind, TokenSet};

use crate::builder::Output;

pub(crate) struct Parser<'t> {
    index: usize,
    tokens: &'t [SyntaxKind],
    output: Vec<Output>,
    fuel: Cell<u8>,
}

type Rule = fn(&mut Parser);

impl<'t> Parser<'t> {
    pub(crate) fn new(tokens: &'t [SyntaxKind]) -> Parser<'t> {
        let index = 0;
        let output = vec![];
        let fuel = Cell::new(u8::MAX);
        Parser { index, tokens, output, fuel }
    }

    pub(crate) fn finish(self) -> Vec<Output> {
        self.output
    }

    fn nth(&self, index: usize) -> SyntaxKind {
        if self.fuel.get() == 0 {
            panic!("invariant violated: fuel exhausted");
        }
        self.fuel.set(self.fuel.get().saturating_sub(1));
        self.tokens.get(self.index + index).copied().unwrap_or(SyntaxKind::END_OF_FILE)
    }

    fn consume(&mut self) {
        self.fuel.set(u8::MAX);
        let kind = self.tokens[self.index];
        self.index += 1;
        self.output.push(Output::Token { kind });
    }

    fn start(&mut self) -> NodeMarker {
        let index = self.output.len();
        self.output.push(Output::Start { kind: SyntaxKind::Node });
        NodeMarker::new(index)
    }

    fn optional(&mut self, rule: Rule) {
        let initial_index = self.index;
        let initial_output = mem::take(&mut self.output);

        rule(self);

        let index = mem::replace(&mut self.index, initial_index);
        let mut output = mem::replace(&mut self.output, initial_output);

        if output.iter().all(|event| !matches!(event, Output::Error { .. })) {
            self.index = index;
            self.output.append(&mut output);
        }
    }

    fn alternative(&mut self, rules: impl IntoIterator<Item = Rule>) {
        let initial_index = self.index;
        let initial_output = mem::take(&mut self.output);

        let mut fallback = None;
        let mut selected = None;

        for rule in rules.into_iter() {
            rule(self);

            let index = mem::replace(&mut self.index, initial_index);
            let output = mem::take(&mut self.output);

            if output.iter().all(|event| !matches!(event, Output::Error { .. })) {
                selected = Some((index, output));
                break;
            } else if fallback.is_none() {
                fallback = Some((index, output));
                continue;
            }
        }

        let (index, mut output) =
            selected.or(fallback).expect("invariant violated: at least one branch");

        self.index = index;
        self.output = initial_output;
        self.output.append(&mut output);
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

    fn eat_in(&mut self, set: TokenSet, kind: SyntaxKind) -> bool {
        if !self.at_in(set) {
            return false;
        }
        self.index += 1;
        self.output.push(Output::Token { kind });
        true
    }

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(format!("expected {:?}", kind));
        false
    }

    fn expect_in(&mut self, set: TokenSet, kind: SyntaxKind, message: &str) -> bool {
        if self.eat_in(set, kind) {
            return true;
        }
        self.error(message);
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
    names::module_name(p);
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
    SyntaxKind::MODULE,
    SyntaxKind::OPERATOR_NAME,
    SyntaxKind::TYPE,
    SyntaxKind::UPPER,
])
.union(names::LOWER_NON_RESERVED);

const EXPORT_LIST_RECOVERY: TokenSet = TokenSet::new(&[SyntaxKind::WHERE]);

fn module_export_item(p: &mut Parser) {
    let mut m = p.start();

    if p.eat_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER) {
        m.end(p, SyntaxKind::ModuleExportValue);
    } else if p.eat(SyntaxKind::UPPER) {
        type_items(p);
        m.end(p, SyntaxKind::ModuleExportType);
    } else if p.eat(SyntaxKind::CLASS) {
        p.expect(SyntaxKind::UPPER);
        m.end(p, SyntaxKind::ModuleExportClass);
    } else if p.eat(SyntaxKind::TYPE) {
        p.expect(SyntaxKind::OPERATOR_NAME);
        m.end(p, SyntaxKind::ModuleExportTypeOperator);
    } else if p.eat(SyntaxKind::OPERATOR_NAME) {
        m.end(p, SyntaxKind::ModuleExportOperator);
    } else if p.eat(SyntaxKind::MODULE) {
        names::module_name(p);
        m.end(p, SyntaxKind::ModuleExportModule);
    } else {
        m.cancel(p);
    }
}

fn type_items(p: &mut Parser) {
    let mut m = p.start();

    let mut kind = SyntaxKind::TypeItemsList;
    'list: {
        if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
            if p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                p.error("Empty item list");
                p.consume();
                break 'list;
            }
            if p.eat(SyntaxKind::DOUBLE_PERIOD) {
                kind = SyntaxKind::TypeItemsAll;
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
    SyntaxKind::TYPE,
    SyntaxKind::INFIX,
    SyntaxKind::INFIXL,
    SyntaxKind::INFIXR,
    SyntaxKind::INSTANCE,
    SyntaxKind::NEWTYPE,
    SyntaxKind::FOREIGN,
])
.union(names::LOWER_NON_RESERVED);

fn import_statement(p: &mut Parser) {
    let mut m = p.start();

    p.expect(SyntaxKind::IMPORT);
    names::module_name(p);
    import_list(p);
    import_alias(p);

    m.end(p, SyntaxKind::ImportStatement);
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

    m.end(p, SyntaxKind::ImportList);
}

const IMPORT_ITEM_START: TokenSet = TokenSet::new(&[
    SyntaxKind::UPPER,
    SyntaxKind::CLASS,
    SyntaxKind::TYPE,
    SyntaxKind::OPERATOR_NAME,
    SyntaxKind::LEFT_PARENTHESIS,
])
.union(names::LOWER_NON_RESERVED);

const IMPORT_LIST_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

fn import_item(p: &mut Parser) {
    let mut m = p.start();

    if p.eat_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER) {
        m.end(p, SyntaxKind::ImportValue);
    } else if p.eat(SyntaxKind::UPPER) {
        type_items(p);
        m.end(p, SyntaxKind::ImportType);
    } else if p.eat(SyntaxKind::CLASS) {
        p.expect(SyntaxKind::UPPER);
        m.end(p, SyntaxKind::ImportClass);
    } else if p.eat(SyntaxKind::TYPE) {
        p.expect(SyntaxKind::OPERATOR_NAME);
        m.end(p, SyntaxKind::ImportTypeOperator);
    } else if p.eat(SyntaxKind::OPERATOR_NAME) {
        m.end(p, SyntaxKind::ImportOperator);
    } else {
        m.cancel(p);
    }
}

fn import_alias(p: &mut Parser) {
    let mut m = p.start();

    if p.eat(SyntaxKind::AS) {
        names::module_name(p);
    }

    m.end(p, SyntaxKind::ImportAlias);
}

fn module_statement(p: &mut Parser) {
    if p.at_in(names::LOWER_NON_RESERVED) {
        value_signature_or_equation(p);
    } else if p.at(SyntaxKind::TYPE) {
        synonym_signature_or_equation(p);
    } else if p.at(SyntaxKind::CLASS) {
        class_signature_or_declaration(p);
    } else if p.at(SyntaxKind::FOREIGN) {
        foreign_import(p);
    } else if p.at(SyntaxKind::INSTANCE) {
        instance_chain(p);
    } else if p.at(SyntaxKind::NEWTYPE) {
        newtype_signature_or_equation(p);
    } else if p.at_in(INFIX_KEYWORD) {
        infix_declaration(p);
    }
}

fn value_signature_or_equation(p: &mut Parser) {
    generic::signature_or_equation(p, SyntaxKind::ValueSignature, SyntaxKind::ValueEquation);
}

const INFIX_KEYWORD: TokenSet =
    TokenSet::new(&[SyntaxKind::INFIX, SyntaxKind::INFIXL, SyntaxKind::INFIXR]);

fn infix_declaration(p: &mut Parser) {
    let mut m = p.start();

    if p.at_in(INFIX_KEYWORD) {
        p.consume();
    } else {
        p.error("Expected INFIX_KEYWORD");
    }

    p.expect(SyntaxKind::INTEGER);

    let mut n = p.start();
    p.eat(SyntaxKind::PREFIX);
    if !(p.eat_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER) || p.eat(SyntaxKind::UPPER)) {
        p.error("Expected LOWER_NON_RESERVED or UPPER");
    }
    n.end(p, SyntaxKind::QualifiedName);

    p.expect(SyntaxKind::AS);

    p.expect_in(
        names::OPERATOR_NON_RESERVED,
        SyntaxKind::OPERATOR,
        "Expected OPERATOR_NON_RESERVED",
    );

    m.end(p, SyntaxKind::InfixDeclaration);
}

fn synonym_signature_or_equation(p: &mut Parser) {
    let mut m = p.start();

    p.expect(SyntaxKind::TYPE);
    p.expect(SyntaxKind::UPPER);

    if p.eat(SyntaxKind::DOUBLE_COLON) {
        types::type_(p);
        m.end(p, SyntaxKind::TypeSynonymSignature);
    } else {
        type_variable_bindings(p);
        p.expect(SyntaxKind::EQUAL);
        types::type_(p);
        m.end(p, SyntaxKind::TypeSynonymEquation);
    }
}

fn class_signature_or_declaration(p: &mut Parser) {
    p.alternative([class_signature, class_declaration]);
}

fn class_signature(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::CLASS);
    p.expect(SyntaxKind::UPPER);
    p.expect(SyntaxKind::DOUBLE_COLON);
    types::type_(p);
    m.end(p, SyntaxKind::ClassSignature);
}

fn class_declaration(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::CLASS);
    p.optional(class_constraints);
    class_head(p);
    if p.at(SyntaxKind::PIPE) {
        class_functional_dependencies(p);
    }
    if p.eat(SyntaxKind::WHERE) {
        class_statements(p);
    }
    m.end(p, SyntaxKind::ClassDeclaration);
}

const CLASS_CONSTRAINTS_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::LEFT_THICK_ARROW,
    SyntaxKind::PIPE,
    SyntaxKind::WHERE,
    SyntaxKind::LAYOUT_SEPARATOR,
    SyntaxKind::LAYOUT_END,
]);

fn class_constraints(p: &mut Parser) {
    let mut m = p.start();
    if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
        while !p.at(SyntaxKind::RIGHT_PARENTHESIS) && !p.at_eof() {
            if p.at_in(types::TYPE_ATOM_START) {
                types::type_5(p);
                if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_PARENTHESIS) {
                    p.error_recover("Trailing comma");
                } else if !p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                    p.expect(SyntaxKind::COMMA);
                }
            } else {
                if p.at_in(CLASS_CONSTRAINTS_RECOVERY) {
                    break;
                }
                p.error_recover("Invalid token");
            }
        }
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
    } else {
        types::type_5(p);
    }
    p.expect(SyntaxKind::LEFT_THICK_ARROW);
    m.end(p, SyntaxKind::ClassConstraints);
}

fn class_head(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::UPPER);
    while p.at_in(types::TYPE_ATOM_START) && !p.at_eof() {
        types::type_atom(p);
    }
    m.end(p, SyntaxKind::ClassHead);
}

const FUNCTIONAL_DEPENDENCY_START: TokenSet =
    TokenSet::new(&[SyntaxKind::RIGHT_ARROW]).union(names::LOWER_NON_RESERVED);

const FUNCTIONAL_DEPENDENCY_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::WHERE, SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

fn class_functional_dependencies(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::PIPE);
    while !p.at(SyntaxKind::WHERE) && !p.at_eof() {
        if p.at_in(FUNCTIONAL_DEPENDENCY_START) {
            class_functional_dependency(p);
            if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::WHERE) {
                p.error_recover("Trailing comma");
            } else if !p.at(SyntaxKind::WHERE) {
                p.expect(SyntaxKind::COMMA);
            }
        } else {
            if p.at_in(FUNCTIONAL_DEPENDENCY_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
    m.end(p, SyntaxKind::ClassFunctionalDependencies);
}

fn class_functional_dependency(p: &mut Parser) {
    let mut m = p.start();
    if p.eat(SyntaxKind::RIGHT_ARROW) {
        p.eat_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER);
        m.end(p, SyntaxKind::FunctionalDependencyDetermined);
    } else {
        while p.eat_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER) {}
        p.expect(SyntaxKind::RIGHT_ARROW);
        while p.eat_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER) {}
        m.end(p, SyntaxKind::FunctionalDependencyDetermines);
    }
}

fn class_statements(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::LAYOUT_START);
    while p.at_in(names::LOWER_NON_RESERVED) && !p.at_eof() {
        class_statement(p);
        while !p.at(SyntaxKind::LAYOUT_SEPARATOR) && !p.at(SyntaxKind::LAYOUT_END) && !p.at_eof() {
            p.error_recover("Invalid token");
        }
        if !p.at(SyntaxKind::LAYOUT_END) {
            p.expect(SyntaxKind::LAYOUT_SEPARATOR);
        }
    }
    p.expect(SyntaxKind::LAYOUT_END);
    m.end(p, SyntaxKind::ClassStatements);
}

fn class_statement(p: &mut Parser) {
    let mut m = p.start();
    p.expect_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER, "Expected LOWER_NON_RESERVED");
    p.expect(SyntaxKind::DOUBLE_COLON);
    types::type_(p);
    m.end(p, SyntaxKind::ClassMemberStatement);
}

fn instance_chain(p: &mut Parser) {
    let mut m = p.start();
    instance_declaration(p);
    while p.at(SyntaxKind::ELSE) && !p.at_eof() {
        instance_declaration(p);
    }
    m.end(p, SyntaxKind::InstanceChain);
}

fn instance_declaration(p: &mut Parser) {
    let mut m = p.start();
    p.eat(SyntaxKind::ELSE);
    p.expect(SyntaxKind::INSTANCE);
    p.optional(instance_constraints);
    instance_head(p);
    if p.eat(SyntaxKind::WHERE) {
        instance_statements(p);
    }
    m.end(p, SyntaxKind::InstanceDeclaration);
}

const INSTANCE_CONSTRAINTS_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::RIGHT_THICK_ARROW,
    SyntaxKind::WHERE,
    SyntaxKind::LAYOUT_SEPARATOR,
    SyntaxKind::LAYOUT_END,
]);

fn instance_constraints(p: &mut Parser) {
    let mut m = p.start();
    if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
        while !p.at(SyntaxKind::RIGHT_PARENTHESIS) && !p.at_eof() {
            if p.at_in(types::TYPE_ATOM_START) {
                types::type_5(p);
                if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_PARENTHESIS) {
                    p.error_recover("Trailing comma");
                } else if !p.at(SyntaxKind::RIGHT_PARENTHESIS) {
                    p.expect(SyntaxKind::COMMA);
                }
            } else {
                if p.at_in(INSTANCE_CONSTRAINTS_RECOVERY) {
                    break;
                }
                p.error_recover("Invalid token");
            }
        }
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
    } else {
        types::type_5(p);
    }
    p.expect(SyntaxKind::RIGHT_THICK_ARROW);
    m.end(p, SyntaxKind::InstanceConstraints);
}

fn instance_head(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::UPPER);
    while p.at_in(types::TYPE_ATOM_START) && !p.at_eof() {
        types::type_atom(p);
    }
    m.end(p, SyntaxKind::InstanceHead);
}

fn instance_statements(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::LAYOUT_START);
    while p.at_in(names::LOWER_NON_RESERVED) && !p.at_eof() {
        instance_statement(p);
        while !p.at(SyntaxKind::LAYOUT_SEPARATOR) && !p.at(SyntaxKind::LAYOUT_END) && !p.at_eof() {
            p.error_recover("Invalid token");
        }
        if !p.at(SyntaxKind::LAYOUT_END) {
            p.expect(SyntaxKind::LAYOUT_SEPARATOR);
        }
    }
    p.expect(SyntaxKind::LAYOUT_END);
    m.end(p, SyntaxKind::InstanceStatements);
}

fn instance_statement(p: &mut Parser) {
    generic::signature_or_equation(
        p,
        SyntaxKind::InstanceSignatureStatement,
        SyntaxKind::InstanceEquationStatement,
    );
}

fn newtype_signature_or_equation(p: &mut Parser) {
    let mut m = p.start();

    p.expect(SyntaxKind::NEWTYPE);
    p.expect(SyntaxKind::UPPER);

    if p.eat(SyntaxKind::DOUBLE_COLON) {
        types::type_(p);
        m.end(p, SyntaxKind::NewtypeSignature);
    } else {
        type_variable_bindings(p);
        p.expect(SyntaxKind::EQUAL);
        p.expect(SyntaxKind::UPPER);
        types::type_atom(p);
        m.end(p, SyntaxKind::NewtypeEquation);
    }
}

const TYPE_VARIABLE_BINDING_START: TokenSet =
    TokenSet::new(&[SyntaxKind::LEFT_PARENTHESIS]).union(names::LOWER_NON_RESERVED);

const TYPE_VARIABLE_BINDING_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);

fn type_variable_bindings(p: &mut Parser) {
    while !p.at(SyntaxKind::EQUAL) && !p.at_eof() {
        if p.at_in(TYPE_VARIABLE_BINDING_START) {
            type_variable_binding(p);
        } else {
            if p.at_in(TYPE_VARIABLE_BINDING_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
}

fn type_variable_binding(p: &mut Parser) {
    let mut m = p.start();

    let closing = p.eat(SyntaxKind::LEFT_PARENTHESIS);
    p.eat_in(names::LOWER_NON_RESERVED, SyntaxKind::LOWER);
    if closing {
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
    }

    m.end(p, SyntaxKind::TypeVariableBinding);
}

fn foreign_import(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::FOREIGN);
    p.expect(SyntaxKind::IMPORT);
    let k = if p.eat(SyntaxKind::DATA) {
        p.expect(SyntaxKind::UPPER);
        SyntaxKind::ForeignImportDataDeclaration
    } else {
        p.expect_in(LOWER_NON_RESERVED, SyntaxKind::LOWER, "Expected LOWER_NON_RESERVED");
        SyntaxKind::ForeignImportValueDeclaration
    };
    p.expect(SyntaxKind::DOUBLE_COLON);
    types::type_(p);
    m.end(p, k);
}
