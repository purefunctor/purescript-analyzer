use syntax::{SyntaxKind, TokenSet};

use super::Parser;

pub fn ty(p: &mut Parser) {
    let mut m = p.start();

    ty_1(p);
    if p.at(SyntaxKind::DOUBLE_COLON) {
        ty(p);
        m.end(p, SyntaxKind::TypeKinded);
    } else {
        m.cancel(p);
    }
}

fn ty_1(p: &mut Parser) {
    let mut m = p.start();

    if p.eat(SyntaxKind::FORALL) {
        ty_variable_bindings(p);
        p.expect(SyntaxKind::PERIOD);
        ty_1(p);
        m.end(p, SyntaxKind::TypeForall);
    } else {
        ty_2(p);
        m.cancel(p);
    }
}

fn ty_2(p: &mut Parser) {
    let mut m = p.start();

    ty_3(p);
    if p.eat(SyntaxKind::RIGHT_ARROW) {
        ty_1(p);
        m.end(p, SyntaxKind::TypeArrow);
    } else if p.eat(SyntaxKind::RIGHT_THICK_ARROW) {
        m.end(p, SyntaxKind::TypeConstrained);
    } else {
        m.cancel(p);
    }
}

fn ty_3(p: &mut Parser) {
    let mut m = p.start();
    let mut i = 0;

    ty_4(p);
    while p.eat(SyntaxKind::OPERATOR) && !p.at_eof() {
        ty_4(p);
        i += 1;
    }

    if i > 0 {
        m.end(p, SyntaxKind::TypeOperatorChain);
    } else {
        m.cancel(p);
    }
}

fn ty_4(p: &mut Parser) {
    let mut m = p.start();

    if p.eat(SyntaxKind::MINUS) {
        p.expect(SyntaxKind::INTEGER);
        m.end(p, SyntaxKind::TypeInteger);
    } else {
        ty_5(p);
        m.cancel(p);
    }
}

fn ty_5(p: &mut Parser) {
    let mut m = p.start();
    let mut i = 0;

    ty_atom(p);
    while p.at_in(TYPE_ATOM_START) && !p.at_eof() {
        ty_atom(p);
        i += 1;
    }

    if i > 0 {
        m.end(p, SyntaxKind::TypeApplicationChain);
    } else {
        m.cancel(p);
    }
}

fn ty_atom(p: &mut Parser) {
    let mut m = p.start();
    if p.eat(SyntaxKind::LOWER) {
        m.end(p, SyntaxKind::TypeVariable);
    } else if p.eat(SyntaxKind::UPPER) {
        m.end(p, SyntaxKind::TypeConstructor);
    } else if p.eat(SyntaxKind::PREFIX) {
        p.expect(SyntaxKind::UPPER);
        m.end(p, SyntaxKind::TypeConstructor);
    } else if p.eat(SyntaxKind::STRING) || p.eat(SyntaxKind::RAW_STRING) {
        m.end(p, SyntaxKind::TypeString);
    } else if p.eat(SyntaxKind::INTEGER) {
        m.end(p, SyntaxKind::TypeInteger);
    } else if p.eat(SyntaxKind::OPERATOR_NAME) {
        m.end(p, SyntaxKind::TypeOperator);
    } else if p.eat(SyntaxKind::LEFT_CURLY) {
        todo!()
    } else if p.eat(SyntaxKind::QUESTION) {
        p.expect(SyntaxKind::LOWER);
        m.end(p, SyntaxKind::TypeHole);
    } else if p.eat(SyntaxKind::UNDERSCORE) {
        m.end(p, SyntaxKind::TypeWildcard);
    } else {
        m.cancel(p);
    }
}

const TYPE_ATOM_START: TokenSet = TokenSet::new(&[
    SyntaxKind::LOWER,
    SyntaxKind::UPPER,
    SyntaxKind::PREFIX,
    SyntaxKind::STRING,
    SyntaxKind::RAW_STRING,
    SyntaxKind::INTEGER,
    SyntaxKind::OPERATOR_NAME,
    SyntaxKind::LEFT_CURLY,
    SyntaxKind::QUESTION,
    SyntaxKind::UNDERSCORE,
]);

fn ty_variable_bindings(p: &mut Parser) {
    while !p.at(SyntaxKind::PERIOD) && !p.at_eof() {
        if p.at_in(TYPE_VARIABLE_BINDING_START) {
            ty_variable_binding(p);
        } else {
            if p.at(SyntaxKind::PERIOD) {
                break;
            }
            p.error_recover("Invalid token");
        }
    }
}

fn ty_variable_binding(p: &mut Parser) {
    let mut m = p.start();

    let closing = p.eat(SyntaxKind::LEFT_PARENTHESIS);

    p.eat(SyntaxKind::AT);
    p.expect(SyntaxKind::LOWER);

    if p.eat(SyntaxKind::DOUBLE_COLON) {
        ty(p);
    }

    if closing {
        p.expect(SyntaxKind::RIGHT_PARENTHESIS);
    }

    m.end(p, SyntaxKind::TypeVariableBinding);
}

const TYPE_VARIABLE_BINDING_START: TokenSet =
    TokenSet::new(&[SyntaxKind::AT, SyntaxKind::LEFT_PARENTHESIS, SyntaxKind::LOWER]);
