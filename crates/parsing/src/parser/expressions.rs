use syntax::{SyntaxKind, TokenSet};

use super::{
    binders::{self, binder_atom},
    binding,
    generic::{self, record_item, RecordItemKind},
    names::{self, LOWER_NON_RESERVED},
    types, NodeMarker, Parser,
};

pub fn expression(p: &mut Parser) {
    let mut m = p.start();

    expression_1(p);
    if p.eat(SyntaxKind::DOUBLE_COLON) {
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
    p.expect(SyntaxKind::TICK);
    tick_expression_1(p);
    p.expect(SyntaxKind::TICK);
    m.end(p, SyntaxKind::ExpressionTick);
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

    if p.eat(SyntaxKind::MINUS) {
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
    p.expect(SyntaxKind::LET);
    binding::bindings(p);
    p.expect(SyntaxKind::IN);
    expression(p);
    m.end(p, SyntaxKind::ExpressionLetIn);
}

fn expression_lambda(p: &mut Parser, mut m: NodeMarker) {
    p.expect(SyntaxKind::BACKSLASH);
    generic::binders_list(p, SyntaxKind::RIGHT_ARROW);
    p.expect(SyntaxKind::RIGHT_ARROW);
    expression(p);
    m.end(p, SyntaxKind::ExpressionLambda);
}

fn expression_case(p: &mut Parser, mut m: NodeMarker) {
    m.end(p, SyntaxKind::ExpressionCaseOf);
}

const DO_STATEMENT_START: TokenSet =
    TokenSet::new(&[SyntaxKind::LET]).union(EXPRESSION_START).union(binders::BINDER_ATOM_START);

fn expression_do(p: &mut Parser, mut m: NodeMarker) {
    p.eat(SyntaxKind::PREFIX);
    p.expect(SyntaxKind::DO);
    do_statements(p);
    m.end(p, SyntaxKind::ExpressionDo);
}

fn do_statements(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::LAYOUT_START);
    while p.at_in(DO_STATEMENT_START) && !p.at_eof() {
        do_statement(p);
        while !p.at(SyntaxKind::LAYOUT_SEPARATOR) && !p.at(SyntaxKind::LAYOUT_END) && !p.at_eof() {
            p.error_recover("Invalid token");
        }
        if !p.at(SyntaxKind::LAYOUT_END) {
            p.expect(SyntaxKind::LAYOUT_SEPARATOR);
        }
    }
    p.expect(SyntaxKind::LAYOUT_END);
    m.end(p, SyntaxKind::DoStatements);
}

fn do_statement(p: &mut Parser) {
    if p.at(SyntaxKind::LET) {
        do_statement_let(p);
    } else {
        let c = p.checkpoint();
        let bind = c.branch(p, do_statement_bind);
        let discard = c.branch(p, do_statement_discard);
        p.decide([bind, discard]);
    }
}

fn do_statement_let(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::LET);
    binding::bindings(p);
    expression(p);
    m.end(p, SyntaxKind::DoStatementLet);
}

fn do_statement_bind(p: &mut Parser) {
    let mut m = p.start();
    let n = p.start();
    binder_atom(p, n);
    p.expect(SyntaxKind::LEFT_ARROW);
    expression(p);
    m.end(p, SyntaxKind::DoStatementBind);
}

fn do_statement_discard(p: &mut Parser) {
    let mut m = p.start();
    expression(p);
    m.end(p, SyntaxKind::DoStatementDiscard);
}

fn expression_ado(p: &mut Parser, mut m: NodeMarker) {
    p.eat(SyntaxKind::PREFIX);
    p.expect(SyntaxKind::ADO);
    do_statements(p);
    p.expect(SyntaxKind::IN);
    expression(p);
    m.end(p, SyntaxKind::ExpressionAdo);
}

fn expression_6(p: &mut Parser) {
    expression_7(p);
}

fn expression_7(p: &mut Parser) {
    let mut m = p.start();
    expression_atom(p);

    if p.at(SyntaxKind::PERIOD) {
        while p.at(SyntaxKind::PERIOD) && !p.at_eof() {
            record_access_field(p);
        }
        m.end(p, SyntaxKind::ExpressionRecordAccess);
    } else {
        m.cancel(p);
    }
}

fn record_access_field(p: &mut Parser) {
    let mut m = p.start();
    p.expect(SyntaxKind::PERIOD);
    names::label(p);
    m.end(p, SyntaxKind::RecordAccessField);
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
        expression_array(p, m);
    } else if p.eat(SyntaxKind::LEFT_CURLY) {
        expression_record(p, m);
    } else if p.eat(SyntaxKind::LEFT_PARENTHESIS) {
        expression(p);
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
        m.end(p, SyntaxKind::ExpressionParenthesized);
    } else {
        m.cancel(p);
    }
}

fn expression_array(p: &mut Parser, mut m: NodeMarker) {
    while !p.at(SyntaxKind::RIGHT_SQUARE) && !p.at_eof() {
        if p.at_in(EXPRESSION_START) {
            expression(p);
            if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_SQUARE) {
                p.error_recover("Trailing comma");
            } else if !p.at(SyntaxKind::RIGHT_SQUARE) {
                p.expect(SyntaxKind::COMMA);
            }
        } else {
            if p.at_in(EXPRESSION_ATOM_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
    p.expect(SyntaxKind::RIGHT_SQUARE);
    m.end(p, SyntaxKind::ExpressionArray);
}

fn expression_record(p: &mut Parser, mut m: NodeMarker) {
    while !p.at(SyntaxKind::RIGHT_CURLY) && !p.at_eof() {
        if p.at_in(names::RECORD_LABEL) {
            record_item(p, RecordItemKind::Expression);
            if p.at(SyntaxKind::COMMA) && p.at_next(SyntaxKind::RIGHT_CURLY) {
                p.error_recover("Trailing comma");
            } else if !p.at(SyntaxKind::RIGHT_CURLY) {
                p.expect(SyntaxKind::COMMA);
            }
        } else {
            if p.at_in(EXPRESSION_ATOM_RECOVERY) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
    p.expect(SyntaxKind::RIGHT_CURLY);
    m.end(p, SyntaxKind::BinderRecord);
}

const EXPRESSION_ATOM_START: TokenSet = TokenSet::new(&[
    SyntaxKind::PREFIX,
    SyntaxKind::UPPER,
    SyntaxKind::OPERATOR_NAME,
    SyntaxKind::UNDERSCORE,
    SyntaxKind::HOLE,
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

const EXPRESSION_ATOM_RECOVERY: TokenSet =
    TokenSet::new(&[SyntaxKind::LAYOUT_SEPARATOR, SyntaxKind::LAYOUT_END]);
