use syntax::{SyntaxKind, TokenSet};

use super::{
    names::{self, LOWER_NON_RESERVED},
    types, NodeMarker, Parser,
};

pub fn expression(p: &mut Parser) {
    let mut m = p.start();

    expression_1(p);
    if p.at(SyntaxKind::DOUBLE_COLON) {
        types::ty(p);
        m.end(p, SyntaxKind::ExpressionTyped);
    } else {
        m.cancel(p);
    }
}

fn expression_1(p: &mut Parser) {
    let mut m = p.start();
    let mut i = 0;

    expression_2(p);
    while p.eat_in(names::OPERATOR_NON_RESERVED, SyntaxKind::OPERATOR) && !p.at_eof() {
        expression_2(p);
        i += 1;
    }

    if i > 0 {
        m.end(p, SyntaxKind::ExpressionOperatorChain);
    } else {
        m.cancel(p);
    }
}

fn expression_2(p: &mut Parser) {
    let mut m = p.start();
    let mut i = 0;

    expression_3(p);
    while p.at(SyntaxKind::TICK) && !p.at_eof() {
        tick_expression(p);
        expression_3(p);
        i += 1;
    }

    if i > 0 {
        m.end(p, SyntaxKind::ExpressionInfixChain);
    } else {
        m.cancel(p);
    }
}

fn tick_expression(p: &mut Parser) {
    let mut m = p.start();
    if p.eat(SyntaxKind::TICK) {
        tick_expression_1(p);
        p.expect(SyntaxKind::TICK);
        m.end(p, SyntaxKind::ExpressionTick);
    } else {
        m.cancel(p);
    }
}

fn tick_expression_1(p: &mut Parser) {
    let mut m = p.start();
    let mut i = 0;

    expression_3(p);
    while p.eat_in(names::OPERATOR_NON_RESERVED, SyntaxKind::OPERATOR) && !p.at_eof() {
        expression_3(p);
        i += 1;
    }

    if i > 0 {
        m.end(p, SyntaxKind::ExpressionOperatorChain);
    } else {
        m.cancel(p);
    }
}

fn expression_3(p: &mut Parser) {
    let mut m = p.start();

    if p.at(SyntaxKind::MINUS) {
        expression_3(p);
        m.end(p, SyntaxKind::ExpressionNegate);
    } else {
        expression_4(p);
        m.cancel(p);
    }
}

fn expression_4(p: &mut Parser) {
    let mut m = p.start();
    let mut i = 0;

    expression_5(p);
    while (p.at(SyntaxKind::AT) || p.at_in(EXPRESSION_START)) && !p.at_eof() {
        expression_argument(p);
        i += 1;
    }

    if i > 0 {
        m.end(p, SyntaxKind::ExpressionApplicationChain);
    } else {
        m.cancel(p);
    }
}

fn expression_argument(p: &mut Parser) {
    let mut m = p.start();
    if p.eat(SyntaxKind::AT) {
        types::ty_atom(p);
        m.end(p, SyntaxKind::ExpressionTypeArgument);
    } else {
        expression_5(p);
        m.end(p, SyntaxKind::ExpressionTermArgument);
    }
}

fn expression_5(p: &mut Parser) {
    let mut m = p.start();

    if p.at(SyntaxKind::IF) {
        expression_if_then_else(p, m);
    } else if p.at(SyntaxKind::LET) {
        expression_let(p, m);
    } else if p.at(SyntaxKind::BACKSLASH) {
        expression_lambda(p, m);
    } else if p.at(SyntaxKind::CASE) {
        expression_case(p, m);
    } else if p.at(SyntaxKind::DO) {
        expression_do(p, m);
    } else if p.at(SyntaxKind::ADO) {
        expression_ado(p, m);
    } else if p.at(SyntaxKind::PREFIX) && p.at_next(SyntaxKind::DO) {
        expression_do(p, m);
    } else if p.at(SyntaxKind::PREFIX) && p.at_next(SyntaxKind::ADO) {
        expression_ado(p, m);
    } else if p.at_in(EXPRESSION_ATOM_START) {
        expression_6(p);
        m.cancel(p);
    } else {
        m.cancel(p);
    }
}

fn expression_if_then_else(p: &mut Parser, mut m: NodeMarker) {
    p.expect(SyntaxKind::IF);
    expression(p);
    p.expect(SyntaxKind::THEN);
    expression(p);
    p.expect(SyntaxKind::ELSE);
    expression(p);
    m.end(p, SyntaxKind::ExpressionIfThenElse);
}

fn expression_let(p: &mut Parser, mut m: NodeMarker) {
    m.end(p, SyntaxKind::ExpressionLetIn);
}

fn expression_lambda(p: &mut Parser, mut m: NodeMarker) {
    m.end(p, SyntaxKind::ExpressionLambda);
}

fn expression_case(p: &mut Parser, mut m: NodeMarker) {
    m.end(p, SyntaxKind::ExpressionCaseOf);
}

fn expression_do(p: &mut Parser, mut m: NodeMarker) {
    m.end(p, SyntaxKind::ExpressionDo);
}

fn expression_ado(p: &mut Parser, mut m: NodeMarker) {
    m.end(p, SyntaxKind::ExpressionAdo);
}

fn expression_6(p: &mut Parser) {
    expression_7(p);
}

fn expression_7(p: &mut Parser) {
    expression_atom(p);
}

fn expression_atom(p: &mut Parser) {
    let mut m = p.start();

    let mut n = p.start();
    if p.eat(SyntaxKind::PREFIX) {
        if p.eat_in(LOWER_NON_RESERVED, SyntaxKind::LOWER) {
            m.end(p, SyntaxKind::ExpressionVariable);
        } else if p.eat(SyntaxKind::UPPER) {
            m.end(p, SyntaxKind::ExpressionConstructor);
        } else if p.eat(SyntaxKind::OPERATOR_NAME) {
            m.end(p, SyntaxKind::ExpressionOperatorName);
        } else {
            m.cancel(p);
        }
        return n.end(p, SyntaxKind::QualifiedName);
    };
    n.cancel(p);

    if p.eat_in(LOWER_NON_RESERVED, SyntaxKind::LOWER) {
        m.end(p, SyntaxKind::ExpressionVariable);
    } else if p.eat(SyntaxKind::UPPER) {
        m.end(p, SyntaxKind::ExpressionConstructor);
    } else if p.eat(SyntaxKind::OPERATOR_NAME) {
        m.end(p, SyntaxKind::ExpressionOperatorName);
    } else if p.eat(SyntaxKind::UNDERSCORE) {
        m.end(p, SyntaxKind::ExpressionSection);
    } else if p.eat(SyntaxKind::HOLE) {
        m.end(p, SyntaxKind::ExpressionHole);
    } else if p.eat(SyntaxKind::STRING) || p.eat(SyntaxKind::RAW_STRING) {
        m.end(p, SyntaxKind::ExpressionString);
    } else if p.eat(SyntaxKind::CHAR) {
        m.end(p, SyntaxKind::ExpressionChar);
    } else if p.eat(SyntaxKind::TRUE) {
        m.end(p, SyntaxKind::ExpressionTrue);
    } else if p.eat(SyntaxKind::FALSE) {
        m.end(p, SyntaxKind::ExpressionFalse);
    } else if p.eat(SyntaxKind::INTEGER) {
        m.end(p, SyntaxKind::ExpressionInteger);
    } else if p.eat(SyntaxKind::NUMBER) {
        m.end(p, SyntaxKind::ExpressionNumber);
    } else if p.eat(SyntaxKind::LEFT_SQUARE) {
        todo!()
    } else if p.eat(SyntaxKind::LEFT_CURLY) {
        todo!()
    } else if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
        todo!()
    } else {
        m.cancel(p);
    }
}

const EXPRESSION_ATOM_START: TokenSet = TokenSet::new(&[
    SyntaxKind::PREFIX,
    SyntaxKind::UPPER,
    SyntaxKind::OPERATOR_NAME,
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
.union(names::LOWER_NON_RESERVED);

const EXPRESSION_START: TokenSet = TokenSet::new(&[
    SyntaxKind::IF,
    SyntaxKind::LET,
    SyntaxKind::BACKSLASH,
    SyntaxKind::CASE,
    SyntaxKind::DO,
    SyntaxKind::ADO,
])
.union(EXPRESSION_ATOM_START);
