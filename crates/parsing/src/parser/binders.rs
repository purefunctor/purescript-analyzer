use syntax::{SyntaxKind, TokenSet};

use super::{generic, names, types, NodeMarker, Parser};

pub(super) fn binder(p: &mut Parser) {
    let mut m = p.start();

    binder_1(p);
    if p.eat(SyntaxKind::DOUBLE_COLON) {
        types::type_(p);
        m.end(p, SyntaxKind::BinderTyped);
    } else {
        m.cancel(p);
    }
}

pub(super) fn binder_1(p: &mut Parser) {
    let mut m = p.start();
    let mut i = 0;

    binder_2(p);
    while p.eat_in(names::OPERATOR, SyntaxKind::OPERATOR) && !p.at_eof() {
        binder_2(p);
        i += 1;
    }

    if i > 0 {
        m.end(p, SyntaxKind::BinderOperatorChain);
    } else {
        m.cancel(p);
    }
}

fn binder_2(p: &mut Parser) {
    let mut m = p.start();

    if p.at(SyntaxKind::MINUS) {
        p.consume();
        if p.at(SyntaxKind::INTEGER) {
            p.consume();
            m.end(p, SyntaxKind::BinderInteger);
        } else if p.at(SyntaxKind::NUMBER) {
            p.consume();
            m.end(p, SyntaxKind::BinderNumber);
        } else {
            p.error("Expected INTEGER or NUMBER");
            m.end(p, SyntaxKind::BinderInteger);
        }
    } else if p.at(SyntaxKind::PREFIX) || p.at(SyntaxKind::UPPER) {
        binder_constructor(p, m);
    } else if p.at_in(BINDER_ATOM_START) {
        binder_atom(p);
        m.cancel(p);
    } else {
        m.cancel(p);
    }
}

pub(super) const BINDER_ATOM_START: TokenSet = TokenSet::new(&[
    SyntaxKind::PREFIX,
    SyntaxKind::UPPER,
    SyntaxKind::UNDERSCORE,
    SyntaxKind::STRING,
    SyntaxKind::RAW_STRING,
    SyntaxKind::CHAR,
    SyntaxKind::TRUE,
    SyntaxKind::FALSE,
    SyntaxKind::INTEGER,
    SyntaxKind::NUMBER,
    SyntaxKind::LEFT_SQUARE,
    SyntaxKind::LEFT_CURLY,
    SyntaxKind::LEFT_PARENTHESIS,
])
.union(names::LOWER);

pub(super) const BINDER_START: TokenSet =
    BINDER_ATOM_START.union(TokenSet::new(&[SyntaxKind::MINUS]));

pub(super) fn binder_atom(p: &mut Parser) {
    let mut m = p.start();
    if p.at_in(names::LOWER) {
        binder_named_or_variable(p, &mut m);
    } else if p.at(SyntaxKind::PREFIX) || p.at(SyntaxKind::UPPER) {
        binder_constructor(p, m);
    } else if p.eat(SyntaxKind::UNDERSCORE) {
        m.end(p, SyntaxKind::BinderWildcard);
    } else if p.eat(SyntaxKind::STRING) || p.eat(SyntaxKind::RAW_STRING) {
        m.end(p, SyntaxKind::BinderString);
    } else if p.eat(SyntaxKind::CHAR) {
        m.end(p, SyntaxKind::BinderChar);
    } else if p.eat(SyntaxKind::TRUE) {
        m.end(p, SyntaxKind::BinderTrue);
    } else if p.eat(SyntaxKind::FALSE) {
        m.end(p, SyntaxKind::BinderFalse);
    } else if p.eat(SyntaxKind::INTEGER) {
        m.end(p, SyntaxKind::BinderInteger);
    } else if p.eat(SyntaxKind::NUMBER) {
        m.end(p, SyntaxKind::BinderNumber);
    } else if p.eat(SyntaxKind::LEFT_SQUARE) {
        binder_array(p, m);
    } else if p.eat(SyntaxKind::LEFT_CURLY) {
        binder_record(p, m);
    } else if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
        binder(p);
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        m.end(p, SyntaxKind::BinderParenthesized);
    } else {
        m.cancel(p);
    }
}

const BINDER_ATOM_RECOVERY: TokenSet = TokenSet::new(&[
    SyntaxKind::EQUAL,
    SyntaxKind::PIPE,
    SyntaxKind::RIGHT_ARROW,
    SyntaxKind::LAYOUT_SEPARATOR,
    SyntaxKind::LAYOUT_END,
]);

fn binder_named_or_variable(p: &mut Parser, m: &mut NodeMarker) {
    p.consume();
    if p.eat(SyntaxKind::AT) {
        binder_atom(p);
        m.end(p, SyntaxKind::BinderNamed);
    } else {
        m.end(p, SyntaxKind::BinderVariable);
    }
}

fn binder_constructor(p: &mut Parser, mut m: NodeMarker) {
    let mut n = p.start();
    p.eat(SyntaxKind::PREFIX);
    p.eat(SyntaxKind::UPPER);
    n.end(p, SyntaxKind::QualifiedName);

    while p.at_in(BINDER_ATOM_START) && !p.at_eof() {
        binder_atom(p);
    }
    m.end(p, SyntaxKind::BinderConstructor);
}

fn binder_array(p: &mut Parser, mut m: NodeMarker) {
    while !p.at(SyntaxKind::RIGHT_SQUARE) && !p.at_eof() {
        if p.at_in(BINDER_START) {
            binder(p);
            if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_SQUARE) {
                p.error_recover("Trailing comma");
            } else if !p.at(SyntaxKind::RIGHT_SQUARE) {
                p.expect(SyntaxKind::COMMA);
            }
        } else {
            if p.at_in(BINDER_ATOM_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
    p.expect(SyntaxKind::RIGHT_SQUARE);
    m.end(p, SyntaxKind::BinderArray);
}

fn binder_record(p: &mut Parser, mut m: NodeMarker) {
    while !p.at(SyntaxKind::RIGHT_CURLY) && !p.at_eof() {
        if p.at_in(names::RECORD_LABEL) {
            generic::record_item(p, binder);
            if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_CURLY) {
                p.error_recover("Trailing comma");
            } else if !p.at(SyntaxKind::RIGHT_CURLY) {
                p.expect(SyntaxKind::COMMA);
            }
        } else {
            if p.at_in(BINDER_ATOM_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
    p.expect(SyntaxKind::RIGHT_CURLY);
    m.end(p, SyntaxKind::BinderRecord);
}
